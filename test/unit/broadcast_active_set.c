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

#define NELEM 10

long bcast_psync[SHMEM_BCAST_SYNC_SIZE];

/* Note: Need to alternate psync arrays because the active set changes */
long barrier_psync0[SHMEM_BARRIER_SYNC_SIZE];
long barrier_psync1[SHMEM_BARRIER_SYNC_SIZE];

int64_t src[NELEM];
int64_t dst[NELEM];

int main(void)
{
    int i, me, npes;
    int errors = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    for (i = 0; i < NELEM; i++) {
        src[i] = me;
        dst[i] = -1;
    }

    for (i = 0; i < SHMEM_BCAST_SYNC_SIZE; i++)
        bcast_psync[i] = SHMEM_SYNC_VALUE;

    for (i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++) {
        barrier_psync0[i] = SHMEM_SYNC_VALUE;
        barrier_psync1[i] = SHMEM_SYNC_VALUE;
    }

    if (me == 0)
        printf("Shrinking active set test\n");

    shmem_barrier_all();

    /* A total of npes tests are performed, where the active set in each test
     * includes PEs i..npes-1 */
    for (i = 0; i <= me; i++) {
        int j;

        if (me == i)
            printf(" + active set size %d\n", npes-i);

        shmem_broadcast64(dst, src, NELEM, 0, i, 0, npes-i, bcast_psync);

        /* Validate broadcasted data */
        for (j = 0; j < NELEM; j++) {
            int64_t expected = (me == i) ? i-1 : i;
            if (dst[j] != expected) {
                printf("%d: Expected dst[%d] = %"PRId64", got dst[%d] = %"PRId64", iteration %d\n",
                       me, j, expected, j, dst[j], i);
                errors++;
            }
        }

        shmem_barrier(i, 0, npes-i, (i % 2) ? barrier_psync0 : barrier_psync1);
    }

    shmem_barrier_all();

    for (i = 0; i < NELEM; i++)
        dst[i] = -1;

    if (me == 0)
        printf("Changing root test\n");

    shmem_barrier_all();

    /* A total of npes tests are performed, where the root changes each time */
    for (i = 0; i < npes; i++) {
        int j;

        if (me == i)
            printf(" + root %d\n", i);

        shmem_broadcast64(dst, src, NELEM, i, 0, 0, npes, bcast_psync);

        /* Validate broadcasted data */
        for (j = 0; j < NELEM; j++) {
            int64_t expected = (me == i) ? i-1 : i;
            if (dst[j] != expected) {
                printf("%d: Expected dst[%d] = %"PRId64", got dst[%d] = %"PRId64", iteration %d\n",
                       me, j, expected, j, dst[j], i);
                errors++;
            }
        }

        shmem_barrier(0, 0, npes, barrier_psync0);
    }
    shmem_finalize();

    return errors != 0;
}
