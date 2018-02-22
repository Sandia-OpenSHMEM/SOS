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
#include <shmem.h>

/* For systems without the PThread barrier API (e.g. MacOS) */
#include "pthread_barrier.h"

#define T 2
#define ITER 100

#ifndef MAX
#define MAX(A,B)   (((A)>(B)) ? (A) : (B))
#endif

int shared_dest_1 = 0, shared_dest_2 = 0, result = 0;
int me, npes, errors = 0, sum_error = 0;
long lock = 0;

pthread_barrier_t fencebar;

long pSync[SHMEM_REDUCE_SYNC_SIZE];
int pWrk[MAX(1, SHMEM_REDUCE_MIN_WRKDATA_SIZE)];

static void * thread_main(void *arg) {
    int tid = *(int *) arg;
    int one = 1, zero = 0;
    int i;

    /* TEST WAIT */
    for (i = 0; i < ITER; i++) {
        shmem_int_wait_until(&shared_dest_1, SHMEM_CMP_EQ, tid);
        shmem_int_atomic_add(&result, one, me);
        shared_dest_1 = (tid + 1) % T;
    }

    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();
    pthread_barrier_wait(&fencebar);

    if (tid == 0) {
        errors += ((result == (T * ITER)) ? 0 : 1);
        if (result != T * ITER) {
            printf("ERROR in WAIT test from %d : result = %d, expected = %d\n", 
                    me, result, T * ITER);
        }
        result = 0;
	shared_dest_1 = 0;
    }

    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();
    pthread_barrier_wait(&fencebar);

    /* TEST WAIT & FENCE WITH NON-ATOMIC READ-WRITE */
    for (i = 0; i < ITER; i++) {
        shmem_int_wait_until(&shared_dest_1, SHMEM_CMP_EQ, tid);
        result++;
        shmem_fence();
        shared_dest_1 = (tid + 1) % T;
    }

    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();
    pthread_barrier_wait(&fencebar);

    if (tid == 0) {
        errors += ((result == (T * ITER)) ? 0 : 1);
        if (result != T * ITER) {
            printf("ERROR in WAIT test from %d : result = %d, expected = %d\n",
                    me, result, T * ITER);
        }
        result = 0;
        shared_dest_1 = 0;
    }

    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();
    pthread_barrier_wait(&fencebar);

    /* TEST FENCE */
    for (i = 0; i < ITER; i++) {
        if (tid == 0) {
            shared_dest_1 += 1;
            shared_dest_2 += 1;
            shmem_fence();
            shmem_int_wait_until(&shared_dest_1, SHMEM_CMP_EQ, zero);
        }

        if (tid == 1) {
            shmem_int_wait_until(&shared_dest_2, SHMEM_CMP_EQ, one);
            shmem_int_atomic_add(&result, shared_dest_1 + shared_dest_2, me);
            shared_dest_2 = 0;
            shared_dest_1 = 0;
            shmem_fence();
        }
    }

    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();
    pthread_barrier_wait(&fencebar);

    if (tid == 0) {
        errors += ((result == (T * ITER)) ? 0 : 1);
        if (result != T * ITER) {
            printf("ERROR in FENCE test from %d : result = %d, expected = %d\n",
                    me, result, T * ITER);
        }
        result = 0;
        shared_dest_1 = 0;
        shared_dest_2 = 0;
    }

    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();
    pthread_barrier_wait(&fencebar);

    /* TEST LOCK */
    for (i = 0; i < ITER; i++) {
        if (tid == 0) {
            shmem_set_lock(&lock);
            shared_dest_1 = 1;
            shmem_clear_lock(&lock);
            shared_dest_2 = 1;
            shmem_int_wait_until(&shared_dest_2, SHMEM_CMP_EQ, zero);
        }

        if (tid == 1) {
            shmem_int_wait_until(&shared_dest_2, SHMEM_CMP_EQ, one);
            result += shared_dest_1;
            shared_dest_1 = 0;
            shared_dest_2 = 0;
        }
    }

    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();
    pthread_barrier_wait(&fencebar);

    if (tid == 0) {
        errors += ((result == ITER) ? 0 : 1);
        if (result != ITER) {
            printf("ERROR in LOCK test from %d : result = %d, expected = %d\n",
                    me, result, T * ITER);
        }
        result = 0;
    }

    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();

    return NULL;
}

int main(int argc, char **argv) {
    int tl, i, ret;
    pthread_t threads[T];
    int t_arg[T];

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

    for (i = 0; i < SHMEM_REDUCE_SYNC_SIZE; i++) {
        pSync[i] = SHMEM_SYNC_VALUE;
    }

    pthread_barrier_init(&fencebar, NULL, T);

    if (me == 0) {
        printf("Starting multi-threaded test on %d PEs, %d threads/PE\n", npes, T);
    }

    for (i = 0; i < T; i++) {
        int err;
        t_arg[i] = i;
        err = pthread_create(&threads[i], NULL, thread_main, (void *) &t_arg[i]);
        assert(0 == err);
    }

    for (i = 0; i < T; i++) {
        int err;
        err = pthread_join(threads[i], NULL);
        assert(0 == err);
    }

    pthread_barrier_destroy(&fencebar);    

    shmem_barrier_all(); 
    shmem_int_sum_to_all(&sum_error, &errors, 1, 0, 0, npes, pWrk, pSync);

    shmem_finalize();
    return (sum_error == 0) ? 0 : 1;
}
