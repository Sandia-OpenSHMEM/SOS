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
#include <target_put.h>

static inline void uni_bw_put(int len, perf_metrics_t *metric_info)
{
    double start = 0.0, end = 0.0;
    unsigned long int i = 0, j = 0;
    int dest = partner_node(*metric_info);
    int snode = (metric_info->num_pes != 1)? streaming_node(*metric_info) : true;
    static int check_once = 0;
    static int fin = -1;

    if (!check_once) {
        /* check to see whether sender and receiver are the same process */
        if (dest == metric_info->my_node) {
            fprintf(stderr, "Warning: Sender and receiver are the same process (%d)\n", 
                             dest);
        }
        /* hostname validation for all sender and receiver processes */
        int status = check_hostname_validation(*metric_info);
        if (status != 0) return;
        check_once++;
    }

    if(metric_info->target_data) {
        target_bw_itr(len, metric_info);
        return;
    }

    shmem_barrier_all();

    if (snode) {
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
    }

    shmem_barrier_all();
    if (snode) {
        start = perf_shmemx_wtime();
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
        shmem_int_p(&fin, 1, dest);
        shmem_int_wait_until(&fin, SHMEM_CMP_EQ, 0);
        end = perf_shmemx_wtime();
        calc_and_print_results(end, start, len, *metric_info);
    } else {
        shmem_int_wait_until(&fin, SHMEM_CMP_EQ, 1);
        shmem_int_p(&fin, 0, dest);
    }
}

static inline void uni_bw_get(int len, perf_metrics_t *metric_info)
{
    double start = 0.0, end = 0.0;
    unsigned long int i = 0, j = 0;
    int dest = partner_node(*metric_info);
    int snode = (metric_info->num_pes != 1)? streaming_node(*metric_info) : true;
    static int check_once = 0;
    static int fin = -1;

    if (!check_once) {
        /* check to see whether sender and receiver are the same process */
        if (dest == metric_info->my_node) {
            fprintf(stderr, "Warning: Sender and receiver are the same process (%d)\n", 
                             dest);
        }
        /* hostname validation for all sender and receiver processes */
        int status = check_hostname_validation(*metric_info);
        if (status != 0) return;
        check_once++;
    }

    if(metric_info->target_data) {
        target_bw_itr(len, metric_info);
        return;
    }

    shmem_barrier_all();

    if (snode) {
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
    }

    shmem_barrier_all();
    if (snode) {
        start = perf_shmemx_wtime();
        for (i = 0; i < metric_info->trials; i++) {
            for(j = 0; j < metric_info->window_size; j++) {
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
        shmem_int_p(&fin, 1, dest);
        shmem_int_wait_until(&fin, SHMEM_CMP_EQ, 0);
        end = perf_shmemx_wtime();
        calc_and_print_results(end, start, len, *metric_info);
    } else {
        shmem_int_wait_until(&fin, SHMEM_CMP_EQ, 1);
        shmem_int_p(&fin, 0, dest);
    }
}

