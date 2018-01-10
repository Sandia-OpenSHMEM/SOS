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

#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>

#define UPPER_LIMIT_NUM_CTX 1024

#define MAX(a,b) ((a)>(b))?(b):(a)
#define WRK_SIZE MAX(2, SHMEM_REDUCE_MIN_WRKDATA_SIZE)

long data = 0;
int min_num_ctx = 0;
int max_num_ctx = 0;

long pSync[SHMEM_REDUCE_SYNC_SIZE];
int pWrk[WRK_SIZE];

int main(int argc, char **argv) {
    int me, npes, i;
    int errors = 0;
    shmem_ctx_t ctx[UPPER_LIMIT_NUM_CTX];

    int new_max = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    /* Create as many contexts as possible until a failure occurs: */
    while (max_num_ctx < UPPER_LIMIT_NUM_CTX) {
        int err = shmem_ctx_create(0, &ctx[max_num_ctx]);
        if (err) {
            printf("%d: Unable to create context #%d (%d)\n", me, max_num_ctx, err);
            /* Need to free up some resources to avoid the open file limit: */
            for (i=1; i<=npes; i++) {
                shmem_ctx_destroy(ctx[max_num_ctx-i]);
            }
            break;
        } else {
            max_num_ctx++;
        }
    }

    /* Some processes might have failed earlier than others - find minimum: */
    shmem_int_min_to_all(&min_num_ctx, &max_num_ctx, 1, 0, 0, npes, pWrk, pSync);

    /* Destroy ~1/2 the contexts to free up resources for the atomics below: */
    new_max = min_num_ctx / 2;
    for (i=max_num_ctx-npes-1; i>new_max; i--) {
        shmem_ctx_destroy(ctx[i]);
    }

    for (i = 0; i < new_max; i++)
        shmem_ctx_long_atomic_inc(ctx[i], &data, (me+1) % npes);

    for (i = 0; i < new_max; i++)
        shmem_ctx_quiet(ctx[i]);

    shmem_sync_all();

    if (data != new_max) {
        printf("%d: error expected %d, got %ld\n", me, new_max, data);
        ++errors;
    } else {
        printf("PE:%d success!\n", me);
    }

    shmem_finalize();
    return errors;
}
