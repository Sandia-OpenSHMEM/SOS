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

#define N 128
#define MAX(a,b) ((a) > (b)) ? a : b
#define WRK_SIZE MAX(N/2+1, SHMEM_REDUCE_MIN_WRKDATA_SIZE)

#define NUM_TESTS 5

#define SHMEM_MALLOC_INVALID_HINT ~(SHMEM_MALLOC_ATOMICS_REMOTE)

static int sumtoall_with_malloc_hint(long hint, int mype, int npes)
{
    int failed = 0;
    int i;
    int *dst, *src;

    if (mype == 0)
        printf("Sum reduction operation started\n");

    dst = (int *)shmem_malloc_with_hints(N * sizeof(*dst), hint);
    src = (int *)shmem_malloc_with_hints(N * sizeof(*src), hint);

    for (i = 0; i < N; i++) {
        src[i] = mype;
        dst[i] = -1;
    }

    shmem_barrier_all();
    shmem_int_sum_reduce(SHMEM_TEAM_WORLD, dst, src, N);

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
    int npes, mype;
    int passed = 0;
    int fail = 0;

    shmem_init();

    npes = shmem_n_pes();
    mype = shmem_my_pe();

    passed = sumtoall_with_malloc_hint(0, mype, npes);
    passed += sumtoall_with_malloc_hint(SHMEM_MALLOC_ATOMICS_REMOTE, mype, npes);
    passed += sumtoall_with_malloc_hint(SHMEM_MALLOC_SIGNAL_REMOTE, mype, npes);
    passed += sumtoall_with_malloc_hint(SHMEM_MALLOC_ATOMICS_REMOTE | SHMEM_MALLOC_SIGNAL_REMOTE, mype, npes);
    passed += sumtoall_with_malloc_hint(SHMEM_MALLOC_INVALID_HINT, mype, npes);

    fail = NUM_TESTS - passed;

    if (mype == 0) {
        if (passed != NUM_TESTS) {
            printf("%d out of %d tests failed\n", fail, NUM_TESTS);
            shmem_global_exit(fail);
        }
        else
            printf("All %d tests passed\n", passed);
    }

    shmem_finalize();
    return 0;
}
