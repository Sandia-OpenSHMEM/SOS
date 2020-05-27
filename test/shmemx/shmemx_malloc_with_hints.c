/*
 *  Copyright (c) 2020 Intel Corporation. All rights reserved.
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

#include <stdio.h>
#include <stdint.h>
#include <shmem.h>
#include <shmemx.h>

#define N 128
#define MAX(a,b) ((a) > (b)) ? a : b
#define WRK_SIZE MAX(N/2+1, SHMEM_REDUCE_MIN_WRKDATA_SIZE)

#define NUM_TESTS 5

#define SHMEMX_MALLOC_INVALID_HINT ~(SHMEMX_MALLOC_ATOMICS_REMOTE)

long pSync[SHMEM_ALLTOALL_SYNC_SIZE];
int pWrk[WRK_SIZE];


int sumtoall_with_malloc_hint(long hint, int mype, int npes)
{
    int failed = 0;
    int i;
    int *dst, *src;

    if (mype == 0)
        printf("Sum reduction operation started\n");

    dst = (int *)shmemx_malloc_with_hints(N * sizeof(*dst), hint);
    src = (int *)shmemx_malloc_with_hints(N * sizeof(*src), hint);

    for (i = 0; i < N; i++) {
        src[i] = mype;
        dst[i] = -1;
    }

    shmem_barrier_all();
    shmem_int_sum_to_all(dst, src, N, 0, 0, npes, pWrk, pSync);

    if (mype == 0) {
        for (i = 0; i < N; i++) {
            int expected;
            expected = npes * (npes-1)/2;
            if (dst[i] != expected) {
                printf("[%d] dst[%d] = %d, expected %d\n", mype, i, dst[i], expected);
                failed = 1;
            }
        }
    }

    return (!failed);
}


int main(int argc, char **argv) {
    int npes, i, mype;
    int passed = 0;
    int fail = 0;

    shmem_init();

    npes = shmem_n_pes();
    mype = shmem_my_pe();

    for (i = 0; i < SHMEM_ALLTOALL_SYNC_SIZE; i++)
        pSync[i] = SHMEM_SYNC_VALUE;

    passed = sumtoall_with_malloc_hint(0, mype, npes);
    passed += sumtoall_with_malloc_hint(SHMEMX_MALLOC_ATOMICS_REMOTE, mype, npes);
    passed += sumtoall_with_malloc_hint(SHMEMX_MALLOC_SIGNAL_REMOTE, mype, npes);
    passed += sumtoall_with_malloc_hint(SHMEMX_MALLOC_ATOMICS_REMOTE | SHMEMX_MALLOC_SIGNAL_REMOTE, mype, npes);
    passed += sumtoall_with_malloc_hint(SHMEMX_MALLOC_INVALID_HINT, mype, npes);


    fail = NO_TESTS - passed;

    if (mype == 0) {
        if (passed != NO_TESTS)
            printf("%d out of %d tests passed\n", fail, NO_TESTS);
        else
            printf("All %d tests passed\n", passed);
    }

    shmem_finalize();
    return 0;
}
