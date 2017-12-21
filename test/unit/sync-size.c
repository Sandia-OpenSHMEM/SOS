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

/* Test various collectives using the same pSync array of SHMEM_SYNC_SIZE */

#include <shmem.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#define N 3
#define MAX(A,B) ((A) > (B)) ? (A) : (B)

long pSync[SHMEM_SYNC_SIZE];
long pWrk[MAX(N/2 + 1, SHMEM_REDUCE_MIN_WRKDATA_SIZE)];

long src[N];
long dst[N];

int main(int argc, char* argv[]) {
    int i, j, me, npes, long_is_32;
    int errors = 0;

    for (i = 0; i < SHMEM_SYNC_SIZE; i++) {
        pSync[i] = SHMEM_SYNC_VALUE;
        pSync[i] = SHMEM_SYNC_VALUE;
    }

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (sizeof(long) == 4) {
        long_is_32 = 1;
    } else if (sizeof(long) == 8) {
        long_is_32 = 0;
    } else {
        printf("Error: sizeof(long) == %zu, must be either 4 or 8\n", sizeof(long));
        shmem_global_exit(1);
    }

    for (i = 0; i < N; i += 1) {
        src[i] = me;
        dst[i] = -1;
    }

    /* Barrier */

    shmem_barrier(0, 0, npes, pSync);
    shmem_barrier_all();

    /* Broadcast */

    if (long_is_32)
        shmem_broadcast32(dst, src, N, 0, 0, 0, npes, pSync);
    else
        shmem_broadcast64(dst, src, N, 0, 0, 0, npes, pSync);

    for (i = 0; i < N && me > 0; i++) {
        if (dst[i] != 0) {
            printf("[%d] Broadcast: got %ld, expected %d\n", me, dst[i], 0);
            ++errors;
        }
    }

    shmem_barrier_all();

    /* Collect */

    long *dst_all = shmem_malloc(npes * N * sizeof(long));

    if (long_is_32)
        shmem_fcollect32(dst_all, src, N, 0, 0, npes, pSync);
    else
        shmem_fcollect64(dst_all, src, N, 0, 0, npes, pSync);

    for (i = 0; i < npes; i++) {
        for (j = 0; j < N; j++) {
            if (dst_all[i*N+j] != i) {
                printf("[%d] Fcollect: got %ld, expected %d\n", me, dst_all[i], j);
                ++errors;
            }
        }
    }

    shmem_free(dst_all);
    shmem_barrier_all();

    /* Reduction */

    shmem_long_max_to_all(dst, src, N, 0, 0, npes, pWrk, pSync);

    for (i = 0; i < N; i++) {
        if (dst[i] != npes-1) {
            printf("[%d] Reduction: got %ld, expected %d\n", npes-1, dst[i], 0);
            ++errors;
        }
    }

    shmem_finalize();

    return errors;
}
