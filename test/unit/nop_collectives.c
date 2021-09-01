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
#include <shmem.h>

long bcast_psync[SHMEM_BCAST_SYNC_SIZE];
long collect_psync[SHMEM_COLLECT_SYNC_SIZE];
long reduce_psync[SHMEM_REDUCE_SYNC_SIZE];
long alltoall_psync[SHMEM_ALLTOALL_SYNC_SIZE];
long alltoalls_psync[SHMEM_ALLTOALLS_SYNC_SIZE];

int pwrk[SHMEM_REDUCE_MIN_WRKDATA_SIZE];

int main(void) {
    int me;

#ifdef ENABLE_DEPRECATED_TESTS
    int i, npes;

    for (i = 0; i < SHMEM_BCAST_SYNC_SIZE; i++)
        bcast_psync[i] = SHMEM_SYNC_VALUE;

    for (i = 0; i < SHMEM_COLLECT_SYNC_SIZE; i++)
        collect_psync[i] = SHMEM_SYNC_VALUE;

    for (i = 0; i < SHMEM_REDUCE_SYNC_SIZE; i++)
        reduce_psync[i] = SHMEM_SYNC_VALUE;

    for (i = 0; i < SHMEM_ALLTOALL_SYNC_SIZE; i++)
        alltoall_psync[i] = SHMEM_SYNC_VALUE;

    for (i = 0; i < SHMEM_ALLTOALLS_SYNC_SIZE; i++)
        alltoalls_psync[i] = SHMEM_SYNC_VALUE;

    for (i = 0; i < SHMEM_REDUCE_MIN_WRKDATA_SIZE; i++)
        pwrk[i] = 0;
#endif

    shmem_init();

    me = shmem_my_pe();

#ifdef ENABLE_DEPRECATED_TESTS
    npes = shmem_n_pes();
#endif

    if (me == 0) printf("Testing zero length collectives\n");

    if (me == 0) printf(" + broadcast\n");
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_broadcast32(NULL, NULL, 0, 0, 0, 0, npes, bcast_psync);
#else
    shmem_int_broadcast(SHMEM_TEAM_WORLD, NULL, NULL, 0, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_broadcast64(NULL, NULL, 0, 0, 0, 0, npes, bcast_psync);
#else
    shmem_long_broadcast(SHMEM_TEAM_WORLD, NULL, NULL, 0, 0);
#endif
    shmem_barrier_all();

    if (me == 0) printf(" + collect\n");
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_fcollect32(NULL, NULL, 0, 0, 0, npes, collect_psync);
#else
    shmem_int_fcollect(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_fcollect64(NULL, NULL, 0, 0, 0, npes, collect_psync);
#else
    shmem_long_fcollect(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();

#ifdef ENABLE_DEPRECATED_TESTS
    shmem_collect32(NULL, NULL, 0, 0, 0, npes, collect_psync);
#else
    shmem_int_collect(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_collect64(NULL, NULL, 0, 0, 0, npes, collect_psync);
#else
    shmem_long_collect(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();

    if (me == 0) printf(" + reduction\n");
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_int_and_to_all(NULL, NULL, 0, 0, 0, npes, pwrk, reduce_psync);
#else
    shmem_int_and_reduce(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_int_or_to_all(NULL, NULL, 0, 0, 0, npes, pwrk, reduce_psync);
#else
    shmem_int_or_reduce(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_int_xor_to_all(NULL, NULL, 0, 0, 0, npes, pwrk, reduce_psync);
#else
    shmem_int_xor_reduce(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_int_min_to_all(NULL, NULL, 0, 0, 0, npes, pwrk, reduce_psync);
#else
    shmem_int_min_reduce(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_int_max_to_all(NULL, NULL, 0, 0, 0, npes, pwrk, reduce_psync);
#else
    shmem_int_max_reduce(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_int_sum_to_all(NULL, NULL, 0, 0, 0, npes, pwrk, reduce_psync);
#else
    shmem_int_sum_reduce(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_int_prod_to_all(NULL, NULL, 0, 0, 0, npes, pwrk, reduce_psync);
#else
    shmem_int_prod_reduce(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();

    if (me == 0) printf(" + all-to-all\n");
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_alltoall32(NULL, NULL, 0, 0, 0, npes, alltoall_psync);
#else
    shmem_int_alltoall(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_alltoall64(NULL, NULL, 0, 0, 0, npes, alltoall_psync);
#else
    shmem_long_alltoall(SHMEM_TEAM_WORLD, NULL, NULL, 0);
#endif
    shmem_barrier_all();

#ifdef ENABLE_DEPRECATED_TESTS
    shmem_alltoalls32(NULL, NULL, 1, 1, 0, 0, 0, npes, alltoalls_psync);
#else
    shmem_int_alltoalls(SHMEM_TEAM_WORLD, NULL, NULL, 1, 1, 0);
#endif
    shmem_barrier_all();
#ifdef ENABLE_DEPRECATED_TESTS
    shmem_alltoalls64(NULL, NULL, 1, 1, 0, 0, 0, npes, alltoalls_psync);
#else
    shmem_long_alltoalls(SHMEM_TEAM_WORLD, NULL, NULL, 1, 1, 0);
#endif

    if (me == 0) printf("Done\n");

    shmem_finalize();

    return 0;
}
