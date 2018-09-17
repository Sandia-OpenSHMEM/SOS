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
void int_p_latency(perf_metrics_t *metric_info)
{
    double start = 0.0;
    double end = 0.0;
    unsigned int i = 0;
    int dest = partner_node(metric_info);
    int sender = (metric_info->num_pes != 1) ? streaming_node(metric_info) : true;
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
        printf("\nshmem_int_p results:\n");
        print_latency_header();
    }
    shmem_barrier_all();

    /* puts to zero to match gets validation scheme */
    if (sender) {

        for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
            if(i == metric_info->warmup)
                start = perf_shmemx_wtime();

            shmem_int_p((int*) metric_info->dest, metric_info->my_node, dest);
            shmem_quiet();

        }
        end = perf_shmemx_wtime();

        calc_and_print_results(start, end, sizeof(int), metric_info);
    }

    shmem_barrier_all();

    if(!sender && metric_info->validate)
        validate_recv(metric_info->dest, sizeof(int), dest);

} /* latency/bw for one-way trip */

static inline
void int_g_latency(perf_metrics_t *metric_info)
{
    double start = 0.0;
    double end = 0.0;
    unsigned int i = 0;
    int rtnd = -1;
    int dest = partner_node(metric_info);
    int receiver = (metric_info->num_pes != 1) ? streaming_node(metric_info) : true;
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
        printf("\nshmem_int_g results:\n");
        print_latency_header();
    }
    shmem_barrier_all();

    if (receiver) {

        for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
            if(i == metric_info->warmup)
                start = perf_shmemx_wtime();

            rtnd = shmem_int_g((int*) metric_info->src, dest);
        }
        end = perf_shmemx_wtime();

        calc_and_print_results(start, end, sizeof(int), metric_info);
    }

    shmem_barrier_all();

    if(receiver && metric_info->validate)
        validate_recv((char*) &rtnd, sizeof(int), dest);
}
