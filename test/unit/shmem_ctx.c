#include <stdio.h>
#include <shmem.h>

long pwrk[SHMEM_REDUCE_MIN_WRKDATA_SIZE];
long psync[SHMEM_REDUCE_SYNC_SIZE];

long task_cntr  = 0; /* Next task counter */
long tasks_done = 0; /* Tasks done by this PE */
long total_done = 0; /* Total tasks done by all PEs */

int main(void) {
    int tl, i;
    long ntasks = 1024;  /* Total tasks per PE */

    for (i = 0; i < SHMEM_REDUCE_SYNC_SIZE; i++)
        psync[i] = SHMEM_SYNC_VALUE;

    shmem_init_thread(SHMEM_THREAD_MULTIPLE, &tl);
    if (tl != SHMEM_THREAD_MULTIPLE) shmem_global_exit(1);

    int me = shmem_my_pe();
    int npes = shmem_n_pes();

#pragma omp parallel reduction (+:tasks_done)
    {
        shmem_ctx_t ctx;
        int task_pe = me, pes_done = 0;
        int ret = shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

        if (ret != 0) {
            printf("%d: Error creating context (%d)\n", me, ret);
            shmem_global_exit(2);
        }

        /* Process tasks on all PEs, starting with the local PE.  After
         * all tasks on a PE are completed, help the next PE. */
        while (pes_done < npes) {
            long task = shmem_atomic_fetch_inc(ctx, &task_cntr, task_pe);
            while (task < ntasks) {
                /* Perform task (task_pe, task) */
                tasks_done++;
                task = shmem_atomic_fetch_inc(ctx, &task_cntr, task_pe);
            }
            pes_done++;
            task_pe = (task_pe + 1) % npes;
        }

        shmem_ctx_destroy(ctx);
    }

    shmem_long_sum_to_all(&total_done, &tasks_done, 1, 0, 0, npes, pwrk, psync);

    int result = (total_done != ntasks * npes);
    shmem_finalize();
    return result;
}
