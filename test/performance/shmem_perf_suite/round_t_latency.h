/*
 *  Copyright (c) 2018 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 *      Redistribution and use in source and binary forms, with or
 *      without modification, are permitted provided that the following
 *      conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

static inline
void long_element_round_trip_latency_get(perf_metrics_t *metric_info)
{
    double start = 0.0;
    double end = 0.0;
    int dest = partner_node(metric_info);
    int receiver = (metric_info->num_pes != 1) ? streaming_node(metric_info) : true;
    *metric_info->target = metric_info->my_node;
    static int check_once = 0;

    if (!check_once) {
        /* check to see whether sender and receiver are the same process */
        if (dest == metric_info->my_node) {
            fprintf(stderr, "Warning: Sender and receiver are the same process (%d)\n",
                             dest);
        }
        /* hostname validation for all sender and receiver processes */
        int status = check_hostname_validation(metric_info);
        if (status != 0) return;
        check_once++;
    }

    if (metric_info->my_node == 0) {
        printf("shmem_long_g results:\n");
        print_latency_header();
    }

    shmem_barrier_all();

    if (receiver) {
        unsigned int i;
        for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
            if(i == metric_info->warmup)
                start = perf_shmemx_wtime();

            *metric_info->target = shmem_long_g(metric_info->target, dest);
        }
        end = perf_shmemx_wtime();

        calc_and_print_results(start, end, sizeof(long), metric_info);

        if(metric_info->validate) {
            if(*metric_info->target != dest)
                printf("validation error shmem_long_g target = %ld != %d\n",
                        *metric_info->target, dest);
        }
    }
} /*gauge small get pathway round trip latency*/

static inline
void long_element_round_trip_latency_put(perf_metrics_t *metric_info)
{
    double start = 0.0;
    double end = 0.0;
    long tmp;
    int dest = partner_node(metric_info);
    int sender = (metric_info->num_pes != 1) ? streaming_node(metric_info) : true;
    unsigned int i;
    tmp = *metric_info->target = INIT_VALUE;
    static int check_once = 0;

    if (!check_once) {
        /* check to see whether sender and receiver are the same process */
        if (dest == metric_info->my_node) {
            fprintf(stderr, "Warning: Sender and receiver are the same process (%d)\n",
                             dest);
        }
        /* hostname validation for all sender and receiver processes */
        int status = check_hostname_validation(metric_info);
        if (status != 0) return;
        check_once++;
    }

    if (metric_info->my_node == 0) {
        printf("Ping-Pong shmem_long_p results:\n");
        print_latency_header();
    }

    shmem_barrier_all();

    if (sender) {
        for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
            if(i == metric_info->warmup)
                start = perf_shmemx_wtime();

            shmem_long_p(metric_info->target, ++tmp, dest);
            shmem_long_wait_until(metric_info->target, SHMEM_CMP_EQ, tmp);
        }
        end = perf_shmemx_wtime();
        metric_info->trials = metric_info->trials * 2; /*output half to get single round trip time*/
        calc_and_print_results(start, end, sizeof(long), metric_info);

   } else {
        for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
            shmem_long_wait_until(metric_info->target, SHMEM_CMP_EQ, ++tmp);
            shmem_long_p(metric_info->target, tmp, dest);
        }
   }

} /* gauge small put pathway round trip latency */
