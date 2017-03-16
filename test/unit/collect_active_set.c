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

#include <stdio.h>
#include <stdint.h>
#include <inttypes.h>
#include <shmem.h>

#define MAX_NPES 32

long collect_psync[SHMEM_COLLECT_SYNC_SIZE];

/* Note: Need to alternate psync arrays because the active set changes */
long barrier_psync0[SHMEM_BARRIER_SYNC_SIZE];
long barrier_psync1[SHMEM_BARRIER_SYNC_SIZE];

int64_t src[MAX_NPES];
int64_t dst[MAX_NPES*MAX_NPES];

int main(void)
{
    int i, me, npes;
    int errors = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    if (npes > MAX_NPES) {
        if (me == 0)
            printf("Test requires fewer than %d PEs\n", MAX_NPES);
        shmem_finalize();
        return 0;
    }

    for (i = 0; i < MAX_NPES; i++)
        src[i] = me;

    for (i = 0; i < MAX_NPES*MAX_NPES; i++)
        dst[i] = -1;

    for (i = 0; i < SHMEM_COLLECT_SYNC_SIZE; i++)
        collect_psync[i] = SHMEM_SYNC_VALUE;

    for (i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++) {
        barrier_psync0[i] = SHMEM_SYNC_VALUE;
        barrier_psync1[i] = SHMEM_SYNC_VALUE;
    }

    if (me == 0)
        printf("Shrinking active set test\n");

    shmem_barrier_all();

    /* A total of npes tests are performed, where the active set in each test
     * includes PEs i..npes-1 and each PE contributes PE ID elements */
    for (i = 0; i <= me; i++) {
        int j, k;
        int idx = 0;

        if (me == i)
            printf(" + active set size %d\n", npes-i);

        shmem_collect64(dst, src, me, i, 0, npes-i, collect_psync);

        /* Validate destination buffer data */
        for (j = 0; j < npes - i; j++) {
            for (k = 0; k < i+j; k++, idx++) {
                if (dst[idx] != i+j) {
                    printf("%d: Expected dst[%d] = %d, got dst[%d] = %"PRId64", iteration %d\n",
                           me, idx, i+j, idx, dst[idx], i);
                    errors++;
                }
            }
        }

        /* Validate unused destination buffer */
        for ( ; idx < MAX_NPES*MAX_NPES; idx++) {
            if (dst[idx] != -1) {
                printf("%d: Expected dst[%d] = %d, got dst[%d] = %"PRId64", iteration %d\n",
                       me, idx, -1, idx, dst[idx], i);
                errors++;
            }
        }

        /* Reset for next iteration */
        for (j = 0; j < MAX_NPES*MAX_NPES; j++)
            dst[j] = -1;

        shmem_barrier(i, 0, npes-i, (i % 2) ? barrier_psync0 : barrier_psync1);
    }

    shmem_finalize();

    return errors != 0;
}
