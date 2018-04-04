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

void static inline bi_bw_put(int len, perf_metrics_t *metric_info)
{
    double start = 0.0, end = 0.0;
    int dest = partner_node(*metric_info);
    int i = 0, j = 0;
    static int check_once = 0;

    if (!check_once) {
        int status = check_hostname_validation(*metric_info);
        if (status == -1) return;
        check_once++;
    }

    shmem_barrier_all();

    for (i = 0; i < metric_info->warmup; i++) {
        for(j = 0; j < metric_info->window_size; j++) {
#ifdef USE_NONBLOCKING_API
            shmem_putmem_nbi(metric_info->dest, metric_info->src, len, dest);
#else
            shmem_putmem(metric_info->dest, metric_info->src, len, dest);
#endif
        }
        shmem_quiet();
    }

    shmem_barrier_all();
    if (streaming_node(*metric_info)) {
        start = perf_shmemx_wtime();
    }

    for (i = 0; i < metric_info->trials; i++) {
        for(j = 0; j < metric_info->window_size; j++) {
#ifdef USE_NONBLOCKING_API
            shmem_putmem_nbi(metric_info->dest, metric_info->src, len, dest);
#else
            shmem_putmem(metric_info->dest, metric_info->src, len, dest);
#endif
        }
        shmem_quiet();
    }

    shmem_barrier_all();
    if (streaming_node(*metric_info)) {
        end = perf_shmemx_wtime();
        calc_and_print_results((end - start), len, *metric_info);
    }
}

void static inline bi_bw_get(int len, perf_metrics_t *metric_info)
{
    double start = 0.0, end = 0.0;
    int dest = partner_node(*metric_info);
    int i = 0, j = 0;
    static int check_once = 0;

    if (!check_once) {
        int status = check_hostname_validation(*metric_info);
        if (status == -1) return;
        check_once++;
    }

    shmem_barrier_all();

    for (i = 0; i < metric_info->warmup; i++) {
        for(j = 0; j < metric_info->window_size; j++) {
		/* Choosing to skip quiet for both blocking and non-blocking getmem
                 * as this sequence of operation (writing to the same location) is 
                 * currently undefined by the OpenSHMEM Spec. */ 
#ifdef USE_NONBLOCKING_API
            shmem_getmem_nbi(metric_info->dest, metric_info->src, len, dest);
#else
            shmem_getmem(metric_info->dest, metric_info->src, len, dest);
#endif
        }
#ifdef USE_NONBLOCKING_API
        shmem_quiet();
#endif
    }

    shmem_barrier_all();
    if (streaming_node(*metric_info)) {
        start = perf_shmemx_wtime();
    }

    for (i = 0; i < metric_info->trials; i++) {
        for(j = 0; j < metric_info->window_size; j++) {
                /* Choosing to skip quiet for both blocking and non-blocking getmem
                 * as this sequence of operation (writing to the same location) is
                 * currently undefined by the OpenSHMEM Spec. */
#ifdef USE_NONBLOCKING_API
            shmem_getmem_nbi(metric_info->dest, metric_info->src, len, dest);
#else
            shmem_getmem(metric_info->dest, metric_info->src, len, dest);
#endif
        }
#ifdef USE_NONBLOCKING_API
        shmem_quiet();
#endif
    } 

    shmem_barrier_all();
    if (streaming_node(*metric_info)) {
        end = perf_shmemx_wtime();
        calc_and_print_results((end - start), len, *metric_info);
    }
}

