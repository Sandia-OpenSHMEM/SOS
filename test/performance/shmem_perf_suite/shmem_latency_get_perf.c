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

/*
**
**  Notice: micro benchmark ~ two nodes only
**
**  Features of Test:
**  1) small get latency test
**  2) getmem latency test to calculate latency of various sizes
**
*/

#include <latency_common.h>
#include <round_t_latency.h>
#include <int_element_latency.h>

int main(int argc, char *argv[])
{

    latency_main(argc, argv, STYLE_GET);

    return 0;
}  /* end of main() */


void
long_element_round_trip_latency(perf_metrics_t *data)
{
#ifndef USE_NONBLOCKING_API
    long_element_round_trip_latency_get(data);
#endif
}

void
int_element_latency(perf_metrics_t *data)
{
#ifndef USE_NONBLOCKING_API
    int_g_latency(data);
#endif
}

void
streaming_latency(int len, perf_metrics_t *metric_info)
{
    double start = 0.0;
    double end = 0.0;
    unsigned long int i = 0;
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

    shmem_barrier_all();
    if (receiver) {

        for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
            if(i == metric_info->warmup)
                start = perf_shmemx_wtime();
#ifdef USE_NONBLOCKING_API
            shmem_getmem_nbi(metric_info->dest, metric_info->src, len, dest);
            shmem_quiet();
#else
            shmem_getmem(metric_info->dest, metric_info->src, len, dest);
#endif
        }
        end = perf_shmemx_wtime();

        calc_and_print_results(start, end, len, metric_info);
    }
} /* latency/bw for one-way trip */
