/*
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 * *	Redistribution and use in source and binary forms, with or
 *	without modification, are permitted provided that the following
 *	conditions are met:
 *
 *	- Redistributions of source code must retain the above
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
**
**  Notice: micro benchmark ~ two nodes only
**
**  Features of Test:
**  1) small put pingpong latency test
**  2) one sided latency test to calculate latency of various sizes
**    to the network stack
**
*/

#include <latency_common.h>

int main(int argc, char *argv[])
{
    latency_main(argc, argv);

    return 0;
}  /* end of main() */


void
long_element_round_trip_latency(perf_metrics_t data)
{
    double start, end;
    long tmp;
    int dest = (data.my_node + 1) % data.npes, i = 0;
    tmp = *data.target = INIT_VALUE;

    if (data.my_node == 0) {
        printf("\nPing-Pong shmem_long_p results:\n");
        print_results_header();
    }

    shmem_barrier_all();

    if (data.my_node == 0) {
        for (i = 0; i < data.trials + data.warmup; i++) {
            if(i == data.warmup)
                start = shmemx_wtime();

            shmem_long_p(data.target, ++tmp, dest);

            shmem_long_wait_until(data.target, SHMEM_CMP_EQ, tmp);
        }
        end = shmemx_wtime();

        calc_and_print_results(start, end, sizeof(long), data);

   } else {
        for (i = 0; i < data.trials + data.warmup; i++) {
            shmem_long_wait_until(data.target, SHMEM_CMP_EQ, ++tmp);

            shmem_long_p(data.target, tmp, dest);
        }
   }

} /*gauge small put pathway round trip latency*/


void
streaming_latency(int len, perf_metrics_t *data)
{
    double start, end;
    int i = 0;

    /*puts to zero to match gets validation scheme*/
    if (data->my_node == 1) {

        for (i = 0; i < data->trials + data->warmup; i++) {
            if(i == data->warmup)
                start = shmemx_wtime();

            shmem_putmem(data->buf, data->buf, len, 0);
            shmem_quiet();

        }
        end = shmemx_wtime();

        calc_and_print_results(start, end, len, *data);
    }
} /* latency/bw for one-way trip */
