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
#include <shmem.h>

#define NREPS 50

#ifdef ENABLE_DEPRECATED_TESTS
long barrier_psync0[SHMEM_BARRIER_SYNC_SIZE];
long barrier_psync1[SHMEM_BARRIER_SYNC_SIZE];
#endif

int main(void)
{
    int i, me, npes;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

#ifdef ENABLE_DEPRECATED_TESTS
    for (i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++) {
        barrier_psync0[i] = SHMEM_SYNC_VALUE;
        barrier_psync1[i] = SHMEM_SYNC_VALUE;
    }
#endif

    shmem_barrier_all();

    /* A total of npes tests are performed, where the active set in each test
     * includes PEs i..npes-1 */
#ifdef ENABLE_DEPRECATED_TESTS
    for (i = 0; i <= me; i++) {
        int j;

        if (me == i)
            printf(" + iteration %d\n", i);

        /* Test that barrier can be called repeatedly with the *same* pSync */
        for (j = 0; j < NREPS; j++)
            shmem_barrier(i, 0, npes-i, (i % 2) ? barrier_psync0 : barrier_psync1);
    }
#else
    shmem_team_t new_team;
    for (i = 0; i <= npes; i++) {
        int j;

        if (me == i)
            printf(" + iteration %d\n", i);

        /* Test that quiet + sync can be called repeatedly*/
        shmem_team_split_strided(SHMEM_TEAM_WORLD, i, 1, npes-i, NULL, 0, &new_team);
        if (new_team != SHMEM_TEAM_INVALID) {
	    for (j = 0; j < NREPS; j++) {
	        shmem_quiet();
	        shmem_team_sync(new_team);
            }
	}
    }
#endif

    shmem_finalize();

    return 0;
}
