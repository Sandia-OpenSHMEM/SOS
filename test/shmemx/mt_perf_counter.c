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

/* Multi-threaded tests for validation of memory barrier implemented in 
 * different synchronization routines.
*/

#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include <shmemx.h>
#include <unistd.h>

/* For systems without the PThread barrier API (e.g. MacOS) */
#include "pthread_barrier.h"

#define T 2
#define ITER 100
#define WINDOW 64
#define LENGTH 1024

int me, npes;
char *src_array, *dest_array;
uint64_t c_put, c_get, p_put, p_get, target;

pthread_barrier_t fencebar;

static void * thread_main(void *arg) {
    int tid = *(int *) arg;
    int i, j;
    int partner = ((npes % 2 == 0) ? (me % 2 == 0 ? me + 1 : me - 1) : 
                                     (me % 2 != 0 ? me - 1 : 
                                     (me == npes - 1) ? me : me + 1));

    shmem_ctx_t ctx;
    shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

    for (i = 0; i < ITER; i++) {
        for (j = 0; j < WINDOW; j++) {
            shmem_ctx_putmem_nbi(ctx, dest_array + tid * LENGTH, 
                                 src_array + tid * LENGTH, LENGTH, partner);
        }
        shmem_ctx_quiet(ctx);
    }

    shmem_ctx_destroy(ctx);

    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();

    return NULL;
}

static void collect(shmem_ctx_t ctx) {
    shmemx_pcntr_get_completed_put(ctx, &c_put);
    shmemx_pcntr_get_completed_get(ctx, &c_get);
    shmemx_pcntr_get_completed_target(ctx, &target);
    shmemx_pcntr_get_pending_put(ctx, &p_put);
    shmemx_pcntr_get_pending_get(ctx, &p_get);
}

static void *collector_main(void *arg) {
    int i;

    for (i = 0; i < ITER; i++) {
        collect(SHMEM_CTX_DEFAULT);
    }

    return NULL;
}

int main(int argc, char **argv) {
    int tl, i, ret, err;
    pthread_t shmem_threads[T], collector;
    int t_arg[T];
    unsigned long alignment = getpagesize();

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

    me = shmem_my_pe();
    npes = shmem_n_pes();

    src_array = shmem_align(alignment, T * LENGTH);
    dest_array = shmem_align(alignment, T * LENGTH);

    pthread_barrier_init(&fencebar, NULL, T);

    if (me == 0) {
        printf("Starting multi-threaded test on %d PEs, %d threads/PE\n", npes, T);
    }

    for (i = 0; i < T; i++) {
        t_arg[i] = i;
        err = pthread_create(&shmem_threads[i], NULL, thread_main, (void *) &t_arg[i]);
        assert(0 == err);
    }

    err = pthread_create(&collector, NULL, collector_main, NULL);
    assert(0 == err);

    for (i = 0; i < T; i++) {
        err = pthread_join(shmem_threads[i], NULL);
        assert(0 == err);
    }

    err = pthread_join(collector, NULL);
    assert(0 == err);

    pthread_barrier_destroy(&fencebar);    

    shmem_barrier_all(); 

    shmem_finalize();
    return 0;
}
