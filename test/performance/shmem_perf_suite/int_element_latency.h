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
int_p_latency(perf_metrics_t data)
{
    double start = 0.0;
    double end = 0.0;
    int i = 0;

    if (data.my_node == PUT_IO_NODE) {
        printf("\nStream shmem_int_p results:\n");
        print_results_header();
    }

    /*puts to zero to match gets validation scheme*/
    if (data.my_node == PUT_IO_NODE) {

        for (i = 0; i < data.trials + data.warmup; i++) {
            if(i == data.warmup)
                start = perf_shmemx_wtime();

            shmem_int_p((int*) data.dest, data.my_node, 0);
            shmem_quiet();

        }
        end = perf_shmemx_wtime();

        calc_and_print_results(start, end, sizeof(int), data);
    }

    shmem_barrier_all();

    if((data.my_node == 0) && data.validate)
        validate_recv(data.dest, sizeof(int), partner_node(data.my_node));

} /* latency/bw for one-way trip */

void static inline
int_g_latency(perf_metrics_t data)
{
    double start = 0.0;
    double end = 0.0;
    int i = 0;
    int rtnd = -1;

    if (data.my_node == GET_IO_NODE) {
        printf("\nStream shmem_int_g results:\n");
        print_results_header();
    }

    if (data.my_node == GET_IO_NODE) {

        for (i = 0; i < data.trials + data.warmup; i++) {
            if(i == data.warmup)
                start = perf_shmemx_wtime();

            rtnd = shmem_int_g((int*) data.src, 1);
        }
        end = perf_shmemx_wtime();

        calc_and_print_results(start, end, sizeof(int), data);
    }

    shmem_barrier_all();

    if((data.my_node == 0) && data.validate)
        validate_recv((char*) &rtnd, sizeof(int), partner_node(data.my_node));
}
