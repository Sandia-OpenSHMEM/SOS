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

/* Multithreaded locking test -- Ensure the SHMEM lock API behaves correctly
 * when used in a multithreaded setting. */

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <pthread.h>
#include <shmem.h>
#include "mt_lock.h"

#define T 4
#define N 4

long dest = 0;
long *locks;

int me, npes;
int errors = 0;

static void * thread_main(void *arg) {
    int i, j;

    for (j = 0; j < N; j++) {
        for (i = 0 ; i < npes; i++) {
            /* Alternate even/odd PEs using set_lock versus a test_lock loop to
             * acquire the lock */
            if ((me+j) % 2 == 0) {
                mtl_set_lock(&locks[i]);
                long d = shmem_long_g(&dest, i);
                shmem_long_p(&dest, d+1, i);
                mtl_clear_lock(&locks[i]);
            } else {
                while (mtl_test_lock(&locks[i])) ;
                long d = shmem_long_g(&dest, i);
                shmem_long_p(&dest, d+1, i);
                mtl_clear_lock(&locks[i]);
            }
        }
    }

    return NULL;
}


int main(int argc, char **argv) {
    int tl, i, ret;
    pthread_t threads[T];
    int       t_arg[T];

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
    locks = shmem_calloc(npes, sizeof(long));

    if (me == 0) printf("Starting MT locking test on %d PEs, %d threads/PE\n", npes, T);

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

    mtl_cleanup();
    shmem_barrier_all();

    if (dest != npes*T*N) {
        printf("%d: Error expected %d, got %ld\n", me, npes*T*N, dest);
        errors++;
    }

    if (me == 0) {
        if (errors) printf("Encountered %d errors\n", errors);
        else printf("Success\n");
    }

    shmem_finalize();
    return (errors == 0) ? 0 : 1;
}
