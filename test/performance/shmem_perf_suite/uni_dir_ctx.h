#include <time.h>
#include <pthread.h>

void static inline uni_bw_ctx(int len, perf_metrics_t *metric_info,
        int streaming_node)
{
    double start = 0.0, end = 0.0;
    int i = 0, j = 0;
    int dest = partner_node(*metric_info);

    char **srcs = (char **)malloc(metric_info->nthreads * sizeof(char *));
    char **dests = (char **)malloc(metric_info->nthreads * sizeof(char *));
    assert(srcs && dests);

    for (i = 0; i < metric_info->nthreads; i++) {
        srcs[i] = aligned_buffer_alloc(len);
        dests[i] = aligned_buffer_alloc(len);
        assert(srcs[i] && dests[i]);
    }

    shmem_barrier_all();

    if (streaming_node) {

#pragma omp parallel default(none) firstprivate(metric_info, len, dest, \
        srcs, dests, stderr) private(j) shared(start) num_threads(metric_info->nthreads)
        {
            int i;
            const int thread_id = omp_get_thread_num();
            shmem_ctx_t ctx;
            char *src_buffer = srcs[thread_id];
            char *dest_buffer = dests[thread_id];

            shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

            for (i = 0; i < metric_info->trials + metric_info->warmup; i++) {
                if (i == metric_info->warmup) {
#pragma omp barrier // Keep threads in sync

#pragma omp master
                    {
                        start = perf_shmemx_wtime();
                    }
                }

                for (j = 0; j < metric_info->window_size; j++) {
                    shmem_ctx_putmem(ctx, dest_buffer, src_buffer, len, dest);
                }
                shmem_ctx_quiet(ctx);
            }
            shmem_ctx_destroy(ctx);
        }
        end = perf_shmemx_wtime();

        calc_and_print_results((end - start), len, *metric_info);
    }

    shmem_barrier_all();

    for (i = 0; i < metric_info->nthreads; i++) {
        aligned_buffer_free(srcs[i]);
        aligned_buffer_free(dests[i]);
    }

    free(srcs);
    free(dests);
}
