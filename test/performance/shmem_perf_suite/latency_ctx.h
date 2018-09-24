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
void streaming_put_latency_ctx(int len, perf_metrics_t *metric_info, int streaming_node)
{
    double start = 0.0, end = 0.0;
    unsigned long int i;
    int dest = partner_node(metric_info);
    static int check_once = 0;

    if (!check_once) {
        /* check to see whether sender and receiver are the same process */
        if (dest == metric_info->my_node) {
            fprintf(stderr, "Warning: Sender and receiver are the same "
                            "process (%d)\n", dest);
        }
        /* hostname validation for all sender and receiver processes */
        int status = check_hostname_validation(metric_info);
        if (status != 0) return;
        check_once++;
    }

    shmem_barrier_all();

    if (streaming_node) {
#pragma omp parallel default(none) firstprivate(len, dest) private(i) \
shared(metric_info, start, end) num_threads(metric_info->nthreads)
        {
            const int thread_id = omp_get_thread_num();
            shmem_ctx_t ctx;
            shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

            for (i = 0; i < metric_info->warmup; i++) {
#ifdef USE_NONBLOCKING_API
                shmem_ctx_putmem_nbi(ctx, metric_info->dest + thread_id * len, 
                                     metric_info->src + thread_id * len, len, dest);
#else
                shmem_ctx_putmem(ctx, metric_info->dest + thread_id * len, 
                                 metric_info->src + thread_id * len, len, dest);
#endif
                shmem_ctx_quiet(ctx);
            }
            shmem_ctx_destroy(ctx);
        }
    }

    shmem_barrier_all();
    if (streaming_node) {
#pragma omp parallel default(none) firstprivate(len, dest) private(i) \
shared(metric_info, start, end) num_threads(metric_info->nthreads)
        {
            const int thread_id = omp_get_thread_num();
            shmem_ctx_t ctx;
            shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

#pragma omp barrier
#pragma omp master
            {
                start = perf_shmemx_wtime();
            }

            for (i = 0; i < metric_info->trials; i++) {
#ifdef USE_NONBLOCKING_API
                shmem_ctx_putmem_nbi(ctx, metric_info->dest + thread_id * len, 
                                     metric_info->src + thread_id * len, len, dest);
#else
                shmem_ctx_putmem(ctx, metric_info->dest + thread_id * len, 
                                 metric_info->src + thread_id * len, len, dest);
#endif
                shmem_ctx_quiet(ctx);
            }
            shmem_ctx_destroy(ctx);
        }
    }

    shmem_barrier_all();
    if (streaming_node) {
        end = perf_shmemx_wtime();
        calc_and_print_results(start, end, len, metric_info);
    }

    shmem_barrier_all();
}

static inline
void streaming_get_latency_ctx(int len, perf_metrics_t *metric_info, int streaming_node)
{
    double start = 0.0, end = 0.0;
    unsigned long int i;
    int dest = partner_node(metric_info);
    static int check_once = 0;

    if (!check_once) {
        /* check to see whether sender and receiver are the same process */
        if (dest == metric_info->my_node) {
            fprintf(stderr, "Warning: Sender and receiver are the same "
                            "process (%d)\n", dest);
        }
        /* hostname validation for all sender and receiver processes */
        int status = check_hostname_validation(metric_info);
        if (status != 0) return;
        check_once++;
    }

    shmem_barrier_all();

    if (streaming_node) {
#pragma omp parallel default(none) firstprivate(len, dest) private(i) \
shared(metric_info, start, end) num_threads(metric_info->nthreads)
        {
            const int thread_id = omp_get_thread_num();
            shmem_ctx_t ctx;
            shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

            for (i = 0; i < metric_info->warmup; i++) {
#ifdef USE_NONBLOCKING_API
                shmem_ctx_getmem_nbi(ctx, metric_info->dest + thread_id * len,
                                     metric_info->src + thread_id * len, len, dest);
                shmem_ctx_quiet(ctx);
#else
                shmem_ctx_getmem(ctx, metric_info->dest + thread_id * len,
                                 metric_info->src + thread_id * len, len, dest);
#endif
            }
            shmem_ctx_destroy(ctx);
        }
    }

    shmem_barrier_all();
    if (streaming_node) {
#pragma omp parallel default(none) firstprivate(len, dest) private(i) \
shared(metric_info, start, end) num_threads(metric_info->nthreads)
        {
            const int thread_id = omp_get_thread_num();
            shmem_ctx_t ctx;
            shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

#pragma omp barrier
#pragma omp master
            {
                start = perf_shmemx_wtime();
            }

            for (i = 0; i < metric_info->trials; i++) {
#ifdef USE_NONBLOCKING_API
                shmem_ctx_getmem_nbi(ctx, metric_info->dest + thread_id * len,
                                     metric_info->src + thread_id * len, len, dest);
                shmem_ctx_quiet(ctx);
#else
                shmem_ctx_getmem(ctx, metric_info->dest + thread_id * len,
                                 metric_info->src + thread_id * len, len, dest);
#endif
            }
            shmem_ctx_destroy(ctx);
        }
    }

    shmem_barrier_all();
    if (streaming_node) {
        end = perf_shmemx_wtime();
        calc_and_print_results(start, end, len, metric_info);
    }

    shmem_barrier_all();
}
