/*
 *  Copyright (c) 2017 Intel Corporation. All rights reserved.
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
**  This is a bandwidth centric test for put: back-to-back message rate
**
**  Features of Test: uni-directional bandwidth
**
**  -by default megabytes/second results
**
**NOTE: this test assumes correctness of reduction algorithm
*/

#include <bw_common.h>
#include <uni_dir.h>

int main(int argc, char *argv[])
{
    uni_dir_bw_main(argc, argv);

    return 0;
}

void static inline target_data_uni_bw(int len, perf_metrics_t metric_info, int streaming_node, int start_pe)
{
    double start = 0.0, end = 0.0;
    int i = 0, j = 0;
    int dest = partner_node(metric_info);
    int snode = (metric_info.num_pes != 1)? streaming_node : true;
    size_t final_len = (metric_info.trials + metric_info.warmup - 1) * len;
    int result[2] = {dest, dest};
    int init[2] = {metric_info.my_node, metric_info.my_node};
    long *end_dst_ptr = (long *)(metric_info.dest + (final_len));

    assert(sizeof(int)*2 == sizeof(long));

    shmem_barrier_all();

    /* reset pointer we will be waiting on */
    if(!snode)
        *(end_dst_ptr) = *((long *)init);

    shmem_barrier_all();
    start = perf_shmemx_wtime();

    /* wait on 8 bytes of final chunk, dst was filled with my_node ints */
    if(!snode) {
        shmem_long_wait_until(end_dst_ptr, SHMEM_CMP_EQ, *((long *)result));
        end = perf_shmemx_wtime();

        if(start_pe == metric_info.my_node)
            printf("target: \n");

        calc_and_print_results((end - start), len, metric_info);
    } else if(snode) {
        for (i = 0; i < metric_info.trials + metric_info.warmup; i++) {
            for(j = 0; j < metric_info.window_size; j++)
                shmem_putmem((metric_info.dest + (len*i)), (metric_info.src + (len*i)), len, dest);

            shmem_quiet();
        }
        end = perf_shmemx_wtime();
    }

    shmem_barrier_all();

    if(start_pe == metric_info.my_node && snode)
        printf("initator: \n");
    if(snode)
        calc_and_print_results((end - start), len, metric_info);
}

void static inline target_bw_itr(int len, perf_metrics_t *metric_info, int snode)
{
    int nPEs = 0, stride = 0, start_pe = 0;
    PE_set_used_adjustments(&nPEs, &stride, &start_pe, *metric_info);

    target_data_uni_bw(len, *metric_info, snode, start_pe);

    metric_info->start_len = TARGET_SZ_MAX;
    len = TARGET_SZ_MAX;

    target_data_uni_bw(len, *metric_info, snode, start_pe);

    /* stopping upper layer from iterating, we are done */
    metric_info->max_len = TARGET_SZ_MIN;
}


void
uni_dir_bw(int len, perf_metrics_t *metric_info)
{
    if(metric_info->target_data) {
        target_bw_itr(len, metric_info, !streaming_node(*metric_info));
        return;
    }

    uni_bw(len, metric_info, !streaming_node(*metric_info));
}
