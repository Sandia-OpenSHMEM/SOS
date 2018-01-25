#include <time.h>
#include <pthread.h>

void static inline uni_bw_ctx(int len, perf_metrics_t *metric_info,
        int streaming_node)
{
    double start = 0.0, end = 0.0;
    int j = 0;
    int dest = partner_node(*metric_info);
    char *src = aligned_buffer_alloc(metric_info->nthreads * len);
    char *dst = aligned_buffer_alloc(metric_info->nthreads * len);
    assert(src && dst);

    shmem_barrier_all();

    if (streaming_node) {

#pragma omp parallel default(none) firstprivate(metric_info, len, dest, \
        src, dst, stderr) private(j) shared(start, end) num_threads(metric_info->nthreads)
        {
            int i;
            const int thread_id = omp_get_thread_num();
            shmem_ctx_t *contexts = malloc(sizeof(shmem_ctx_t) * 1);
            shmem_ctx_create(SHMEM_CTX_PRIVATE, &contexts[0]);
            //shmem_ctx_t ctx;
            //shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

            for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
                if (i == metric_info->warmup) {
#pragma omp barrier // Keep threads in sync
#pragma omp master
                    {
                        start = perf_shmemx_wtime();
                    }
                }

                for (j = 0; j < metric_info->window_size; j++) {
#ifdef USE_NONBLOCKING_API
                    shmem_ctx_putmem_nbi(contexts[0], dst + thread_id * len, src + thread_id * len, len, dest);
                    //shmem_ctx_putmem_nbi(ctx, dst + thread_id * len, src + thread_id * len, len, dest);
#else
                    shmem_ctx_putmem(contexts[0], dst + thread_id * len, src + thread_id * len, len, dest);
                    //shmem_ctx_putmem(ctx, dst + thread_id * len, src + thread_id * len, len, dest);
#endif
                }
                shmem_ctx_quiet(contexts[0]);
                //shmem_ctx_quiet(ctx);
            }
#pragma omp barrier // Keep threads in sync
#pragma omp master 
            {
                end = perf_shmemx_wtime();
            }
            shmem_ctx_destroy(contexts[0]);
            free(contexts);
            //shmem_ctx_destroy(ctx);
        }

        calc_and_print_results((end - start), len, *metric_info);
    }

    shmem_barrier_all();

    aligned_buffer_free(src);
    aligned_buffer_free(dst);

}
