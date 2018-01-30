void static inline bi_bw_ctx (int len, perf_metrics_t *metric_info)
{
    double start = 0.0, end = 0.0;
    int dest = partner_node(*metric_info);
    int j = 0;
    char *src = aligned_buffer_alloc(metric_info->nthreads * len);
    char *dst = aligned_buffer_alloc(metric_info->nthreads * len);
    assert(src && dst);

    shmem_barrier_all();

    if (streaming_node(*metric_info)) {
#pragma omp parallel default(none) firstprivate(len, dest) private(j) \
        shared(metric_info, src, dst, start, end) num_threads(metric_info->nthreads)
        {
            int i;
            const int thread_id = omp_get_thread_num();
            shmem_ctx_t ctx;
            shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

            for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
                if(i == metric_info->warmup) {
#pragma omp barrier
#pragma omp master
                    { 
                        start = perf_shmemx_wtime();
                    }
                }

                for(j = 0; j < metric_info->window_size; j++) {
#ifdef USE_NONBLOCKING_API
                    shmem_ctx_putmem_nbi(ctx, dst + thread_id * len, src + thread_id * len, len, dest);
#else
                    shmem_ctx_putmem(ctx, dst + thread_id * len, src + thread_id * len, len, dest);
#endif
                }
                shmem_ctx_quiet(ctx);
            }
#pragma omp barrier
#pragma omp master 
            {
                end = perf_shmemx_wtime();
            }
            shmem_ctx_destroy(ctx);
        }
        calc_and_print_results((end - start), len, *metric_info);
    } else {
#pragma omp parallel default(none) firstprivate(len, dest) private(j) \
        shared(metric_info, src, dst, start, end) num_threads(metric_info->nthreads)
        {
            int i;
            const int thread_id = omp_get_thread_num();
            shmem_ctx_t ctx;
            shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

            for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
                for(j = 0; j < metric_info->window_size; j++) {
#ifdef USE_NONBLOCKING_API
                    shmem_ctx_putmem_nbi(ctx, dst + thread_id * len, src + thread_id * len, len, dest);
#else
                    shmem_ctx_putmem(ctx, dst + thread_id * len, src + thread_id * len, len, dest);
#endif
                } 
                shmem_ctx_quiet(ctx);
            }
            shmem_ctx_destroy(ctx);
        }
    }
    shmem_barrier_all();
}
