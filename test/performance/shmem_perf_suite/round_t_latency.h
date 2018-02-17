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

void static inline
long_element_round_trip_latency_get(perf_metrics_t data)
{
    double start = 0.0;
    double end = 0.0;
    int dest = 1, i = 0;
    int partner_pe = partner_node(data.my_node);
    *data.target = data.my_node;

    if (data.my_node == GET_IO_NODE) {
        printf("\nshmem_long_g results:\n");
        print_results_header();
    }

    shmem_barrier_all();

    if (data.my_node == GET_IO_NODE) {
        for (i = 0; i < data.trials + data.warmup; i++) {
            if(i == data.warmup)
                start = perf_shmemx_wtime();

            *data.target = shmem_long_g(data.target, dest);
        }
        end = perf_shmemx_wtime();

        calc_and_print_results(start, end, sizeof(long), data);

        if(data.validate) {
            if(*data.target != partner_pe)
                printf("validation error shmem_long_g target = %ld != %d\n",
                        *data.target, partner_pe);
        }
    }
} /*gauge small get pathway round trip latency*/

void static inline
long_element_round_trip_latency_put(perf_metrics_t data)
{
    double start = 0.0;
    double end = 0.0;
    long tmp;
    int dest = (data.my_node + 1) % data.npes, i = 0;
    tmp = *data.target = INIT_VALUE;

    if (data.my_node == PUT_IO_NODE) {
        printf("\nPing-Pong shmem_long_p results:\n");
        print_results_header();
    }

    shmem_barrier_all();

    if (data.my_node == PUT_IO_NODE) {
        for (i = 0; i < data.trials + data.warmup; i++) {
            if(i == data.warmup)
                start = perf_shmemx_wtime();

            shmem_long_p(data.target, ++tmp, dest);

            shmem_long_wait_until(data.target, SHMEM_CMP_EQ, tmp);
        }
        end = perf_shmemx_wtime();

        data.trials = data.trials*2; /*output half to get single round trip time*/
        calc_and_print_results(start, end, sizeof(long), data);

   } else {
        for (i = 0; i < data.trials + data.warmup; i++) {
            shmem_long_wait_until(data.target, SHMEM_CMP_EQ, ++tmp);

            shmem_long_p(data.target, tmp, dest);
        }
   }

} /*gauge small put pathway round trip latency*/
