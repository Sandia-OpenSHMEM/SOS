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

#define CHECK(func, in, out)                                            \
    do {                                                                \
        if (out != in) {                                                \
            printf("[%02d] Error: %s expected=%ld out=%ld\n", me, func, \
                   (long) in, (long) out);                              \
            ++errors;                                                   \
        }                                                               \
    } while (0)

int in, out;
int32_t in_32, out_32;
int64_t in_64, out_64;

int main(void) {
    int i, errors = 0;
    int me;

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

    shmem_init();

    me = shmem_my_pe();

    if (me == 0) printf("Testing single PE active set collectives\n");

    /* Broadcast */
    /* Note: Broadcast does not modify the output buffer at the root */
    if (me == 0) printf(" + broadcast\n");

    in_32 = me; out_32 = -1;
    shmem_broadcast32(&in_32, &out_32, 1, 0, me, 0, 1, bcast_psync);
    CHECK("shmem_broadcast32", -1, out_32);
    shmem_barrier_all();

    in_64 = me; out_64 = -1;
    shmem_broadcast64(&in_64, &out_64, 1, 0, me, 0, 1, bcast_psync);
    CHECK("shmem_broadcast64", -1, out_64);
    shmem_barrier_all();

    /* Collect */
    if (me == 0) printf(" + collect\n");

    in_32 = me; out_32 = -1;
    shmem_fcollect32(&in_32, &out_32, 1, me, 0, 1, collect_psync);
    CHECK("shmem_fcollect32", in_32, out_32);
    shmem_barrier_all();

    in_64 = me; out_64 = -1;
    shmem_fcollect64(&in_64, &out_64, 1, me, 0, 1, collect_psync);
    CHECK("shmem_fcollect64", in_64, out_64);
    shmem_barrier_all();

    in_32 = me; out_32 = -1;
    shmem_collect32(&in_32, &out_32, 1, me, 0, 1, collect_psync);
    CHECK("shmem_collect32", in_32, out_32);
    shmem_barrier_all();

    in_64 = me; out_64 = -1;
    shmem_collect64(&in_64, &out_64, 1, me, 0, 1, collect_psync);
    CHECK("shmem_collect64", in_64, out_64);
    shmem_barrier_all();

    /* Reduction */
    if (me == 0) printf(" + reduction\n");

    in = me; out = -1;
    shmem_int_and_to_all(&in, &out, 1, me, 0, 1, pwrk, reduce_psync);
    CHECK("shmem_int_and_to_all", in, out);
    shmem_barrier_all();

    in = me; out = -1;
    shmem_int_or_to_all(&in, &out, 1, me, 0, 1, pwrk, reduce_psync);
    CHECK("shmem_int_or_to_all", in, out);
    shmem_barrier_all();

    in = me; out = -1;
    shmem_int_xor_to_all(&in, &out, 1, me, 0, 1, pwrk, reduce_psync);
    CHECK("shmem_int_xor_to_all", in, out);
    shmem_barrier_all();

    in = me; out = -1;
    shmem_int_min_to_all(&in, &out, 1, me, 0, 1, pwrk, reduce_psync);
    CHECK("shmem_int_min_to_all", in, out);
    shmem_barrier_all();

    in = me; out = -1;
    shmem_int_max_to_all(&in, &out, 1, me, 0, 1, pwrk, reduce_psync);
    CHECK("shmem_int_max_to_all", in, out);
    shmem_barrier_all();

    in = me; out = -1;
    shmem_int_sum_to_all(&in, &out, 1, me, 0, 1, pwrk, reduce_psync);
    CHECK("shmem_int_sum_to_all", in, out);
    shmem_barrier_all();

    in = me; out = -1;
    shmem_int_prod_to_all(&in, &out, 1, me, 0, 1, pwrk, reduce_psync);
    CHECK("shmem_int_prod_to_all", in, out);
    shmem_barrier_all();

    /* All-to-all */
    if (me == 0) printf(" + all-to-all\n");

    in_32 = me; out_32 = -1;
    shmem_alltoall32(&in_32, &out_32, 1, me, 0, 1, alltoall_psync);
    CHECK("shmem_alltoall32", in_32, out_32);
    shmem_barrier_all();

    in_64 = me; out_64 = -1;
    shmem_alltoall64(&in_64, &out_64, 1, me, 0, 1, alltoall_psync);
    CHECK("shmem_alltoall64", in_64, out_64);
    shmem_barrier_all();

    in_32 = me; out_32 = -1;
    shmem_alltoalls32(&in_32, &out_32, 1, 1, 1, me, 0, 1, alltoalls_psync);
    CHECK("shmem_alltoalls32", in_32, out_32);
    shmem_barrier_all();

    in_64 = me; out_64 = -1;
    shmem_alltoalls64(&in_64, &out_64, 1, 1, 1, me, 0, 1, alltoalls_psync);
    CHECK("shmem_alltoalls64", in_64, out_64);
    shmem_barrier_all();

    if (me == 0) printf("Done\n");

    shmem_finalize();

    return errors;
}
