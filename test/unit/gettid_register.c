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

/* Gettid Register Test: Register a custom gettid function pointer  */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <shmem.h>
#include <shmemx.h>

#define T 8

int dest;

int me, npes;
int errors = 0;


uint64_t my_gettid(void) {
    uint64_t tid_val = (uint64_t)pthread_self();
    return tid_val;
}


static void * thread_main(void *arg) {
    int tid = * (int *) arg;
    int i;

    shmem_ctx_t ctx;
    int ret = shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);
    if (ret != 0) {
        printf("Error creating context (%d)\n", ret);
        shmem_global_exit(2);
    }

    for (i = 1; i <= npes; i++)
        shmem_ctx_int_atomic_add(ctx, &dest, tid, (me + i) % npes);

    shmem_quiet();

    shmem_ctx_destroy(ctx);

    return NULL;
}


int main(int argc, char **argv) {
    int tl, i, ret;
    pthread_t threads[T];
    int       t_arg[T];

    shmemx_register_gettid( &my_gettid );

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

    if (sizeof(pthread_t) > sizeof(uint64_t)) {
        printf("Cannot run this test, size of pthread_t is larger than 64 bits\n");
        shmem_finalize();
        return 0;
    }

    me = shmem_my_pe();
    npes = shmem_n_pes();


    if (me == 0) printf("Starting multithreaded test on %d PEs, %d threads/PE\n", npes, T);

    for (i = 0; i < T; i++) {
        int err;
        t_arg[i] = i;
        err = pthread_create(&threads[i], NULL, thread_main, (void*) &t_arg[i]);
        assert(0 == err);
    }

    for (i = 0; i < T; i++) {
        int err;
        err = pthread_join(threads[i], NULL);
        assert(0 == err);
    }

    shmem_sync_all();

    if (dest != ((T-1)*T/2)*npes) {
        printf("%d: dest = %d, expected %d\n", me, dest, ((T-1)*T/2)*npes);
        errors++;
    }

    shmem_finalize();
    return (errors == 0) ? 0 : 1;
}
