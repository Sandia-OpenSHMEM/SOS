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
 *
 * This test is derived from an example provided in the OpenSHMEM 1.4
 * specification.  Additional copyrights may apply.
 *
 */

#include <stdio.h>
#include <shmem.h>

long pwrk[SHMEM_REDUCE_MIN_WRKDATA_SIZE];
long psync[SHMEM_REDUCE_SYNC_SIZE];

long task_cntr  = 0; /* Next task counter */
long tasks_done = 0; /* Tasks done by this PE */
long total_done = 0; /* Total tasks done by all PEs */

int main(void) {
    int tl, i, ret;
    long ntasks = 1024;  /* Total tasks per PE */

    for (i = 0; i < SHMEM_REDUCE_SYNC_SIZE; i++)
        psync[i] = SHMEM_SYNC_VALUE;

    ret = shmem_init_thread(SHMEM_THREAD_MULTIPLE, &tl);

    if (tl != SHMEM_THREAD_MULTIPLE || ret != 0) {
        printf("Init failed (requested thread level %d, got %d, ret %d)\n",
               SHMEM_THREAD_MULTIPLE, tl, ret);

        if (ret == 0) {
            shmem_global_exit(1);
        } else {
            return ret;
        }
    }

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
            long task = shmem_ctx_long_atomic_fetch_inc(ctx, &task_cntr, task_pe);
            while (task < ntasks) {
                /* Perform task (task_pe, task) */
                tasks_done++;
                task = shmem_ctx_long_atomic_fetch_inc(ctx, &task_cntr, task_pe);
            }
            pes_done++;
            task_pe = (task_pe + 1) % npes;
        }

        shmem_ctx_destroy(ctx);
    }

    shmem_long_sum_to_all(&total_done, &tasks_done, 1, 0, 0, npes, pwrk, psync);

    int result = (total_done != ntasks * npes);
    if (me == 0 && result)
        printf("Error: total_done is %ld, expected %ld\n", total_done, ntasks * npes);

    shmem_finalize();
    return result;
}
