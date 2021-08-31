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

#define NELEM 10

#ifdef ENABLE_DEPRECATED_TESTS
long max_psync[SHMEM_REDUCE_SYNC_SIZE];
long min_psync[SHMEM_REDUCE_SYNC_SIZE];

long min_pwrk[NELEM/2 + SHMEM_REDUCE_MIN_WRKDATA_SIZE];
long max_pwrk[NELEM/2 + SHMEM_REDUCE_MIN_WRKDATA_SIZE];
#endif

long src[NELEM];
long dst_max[NELEM];
long dst_min[NELEM];

static int validate_max(int i, int me, int npes) {
    int errors = 0;
    /* Validate reduced max data */
    for (int j = 0; j < NELEM; j++) {
        long expected = npes-1;
        if (dst_max[j] != expected) {
	    printf("%d: Max expected dst_max[%d] = %ld, got dst_max[%d] = %ld, iteration %d\n",
	           me, j, expected, j, dst_max[j], i);
	    errors++;
        }
    }
    return errors;
}

static int validate_min(int i, int me, int npes) {
    int errors = 0;
    /* Validate reduced min data */
    for (int j = 0; j < NELEM; j++) {
        long expected = i;
        if (dst_min[j] != expected) {
	    printf("%d: Min expected dst_min[%d] = %ld, got dst_min[%d] = %ld, iteration %d\n",
	           me, j, expected, j, dst_min[j], i);
	    errors++;
        }
    }
    return errors;
}

int main(void)
{
    int i, me, npes;
    int errors = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    for (i = 0; i < NELEM; i++) {
        src[i] = me;
        dst_max[i] = -1;
        dst_min[i] = -1;
    }

#ifdef ENABLE_DEPRECATED_TESTS
    for (i = 0; i < SHMEM_REDUCE_SYNC_SIZE; i++) {
        max_psync[i] = SHMEM_SYNC_VALUE;
        min_psync[i] = SHMEM_SYNC_VALUE;
    }
#endif

    if (me == 0)
        printf("Shrinking active set test\n");

    shmem_barrier_all();

    /* A total of npes tests are performed, where the active set in each test
     * includes PEs i..npes-1 */
#ifdef ENABLE_DEPRECATED_TESTS
    for (i = 0; i <= me; i++) {

        if (me == i)
            printf(" + PE_start=%d, logPE_stride=0, PE_size=%d\n", i, npes-i);

        shmem_long_max_to_all(dst_max, src, NELEM, i, 0, npes-i, max_pwrk, max_psync);
	errors += validate_max(i, me, npes);

        shmem_long_min_to_all(dst_min, src, NELEM, i, 0, npes-i, min_pwrk, min_psync);
	errors += validate_min(i, me, npes);

    }
#else
    shmem_team_t new_team;
    for (i = 0; i < npes; i++) {

        if (me == i)
            printf(" + PE_start=%d, PE_stride=1, PE_size=%d\n", i, npes-i);

        shmem_team_split_strided(SHMEM_TEAM_WORLD, i, 1, npes-i, NULL, 0, &new_team);
        if (new_team != SHMEM_TEAM_INVALID) {
            shmem_long_max_reduce(new_team, dst_max, src, NELEM);
	    errors += validate_max(i, me, npes);
	    
            shmem_long_min_reduce(new_team, dst_min, src, NELEM);
	    errors += validate_min(i, me, npes);
	}
    }
#endif


    shmem_finalize();

    return errors != 0;
}
