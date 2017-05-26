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

void static inline bi_bw(int len, perf_metrics_t *metric_info)
{
    double start = 0.0, end = 0.0;
    uint32_t dest = partner_node(*metric_info);
    int i = 0, j = 0;

    shmem_barrier_all();

    if (streaming_node(*metric_info)) {
        for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
            if(i == metric_info->warmup)
                start = perf_shmemx_wtime();

            for(j = 0; j < metric_info->window_size; j++)
                shmem_putmem(metric_info->dest, metric_info->src, len, dest);

            shmem_quiet();
        }
        end = perf_shmemx_wtime();

        calc_and_print_results((end - start), len, *metric_info);

    } else {
        for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
            for(j = 0; j < metric_info->window_size; j++)
                shmem_putmem(metric_info->dest, metric_info->src, len, dest);

            shmem_quiet();
        }
    }
}
