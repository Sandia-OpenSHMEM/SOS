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
 */

/* Thread wait test: Test whether a store performed by one thead will wake up a
 * second thread from a call to shmem_wait. */

#include <stdio.h>
#include <pthread.h>
#include <unistd.h>
#include <shmem.h>

static long shr_var = 0;

static void* src_thread_fn(void *arg) {
    /* Try to get the dst thread to enter wait before the call to sleep */
    sleep(1);

    /* This should wake up the waiting dst thread */
    shr_var = 1;

    /* Quiet should provide a store fence */
    shmem_quiet();
    return NULL;
}

static void* dst_thread_fn(void *arg) {
    shmem_long_wait_until(&shr_var, SHMEM_CMP_NE, 0);
    return NULL;
}

int main(int argc, char* argv[]) {
    int tl, ret;
    pthread_t src_thread, dst_thread;

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

    pthread_create(&dst_thread, NULL, &dst_thread_fn, NULL);
    pthread_create(&src_thread, NULL, &src_thread_fn, NULL);

    pthread_join(dst_thread, NULL);
    pthread_join(src_thread, NULL);

    shmem_finalize();

    return 0;
}
