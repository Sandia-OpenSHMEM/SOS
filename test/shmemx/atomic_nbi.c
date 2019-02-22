/*
 *  Copyright (c) 2018 Intel Corporation. All rights reserved.
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
#include <stdlib.h>
#include <shmem.h>
#include <shmemx.h>

#define N 2

static long target;

int main(void) {
    int i, errors = 0;

    shmem_init();

    int me   = shmem_my_pe();
    int npes = shmem_n_pes();

    long *result = malloc(sizeof(long)*npes);

    /* Test NBI swap */

    target = me;
    for (i = 0; i < npes; i++)
        result[i] = -1;

    shmem_barrier_all();

    /* Loop until we observe 'npes' values that are congruent to 'me' mod N */
    for (int ncompleted = 0; ncompleted < npes; ) {
        for (i = 0; i < npes; i++)
            shmemx_long_atomic_swap_nbi(&result[i], &target, me, i);
        shmem_quiet();
        for (i = 0; i < npes; i++)
            if (me % N == result[i] % N) ++ncompleted;
    }

    shmem_barrier_all();

    if (target < 0 || target >= npes) {
        printf("%d: Invalid target (%ld) in NBI swap test\n", me, target);
        ++errors;
    }

    for (i = 0; i < npes; i++) {
        if (result[i] < 0 || result[i] >= npes) {
            printf("%d: Invalid result (%ld) in NBI swap test\n", me, result[i]);
            ++errors;
        }
    }

    /* Test NBI cswap */

    target = -1;
    for (i = 0; i < npes; i++)
        result[i] = npes;

    shmem_barrier_all();

    /* Target value at each PE counts from -1 up to npes-1 */
    for (;;) {
        int ncompleted = 0;

        /* Attempt to increment each PE not already incremented by me */
        for (i = 0; i < npes; i++)
            if (result[i] != me-1)
                shmemx_long_atomic_compare_swap_nbi(&result[i], &target, me-1, me, i);

        shmem_quiet();

        for (i = 0; i < npes; i++)
            if (result[i] == me-1) ++ncompleted;
        if (ncompleted == npes)
            break;
    }

    shmem_barrier_all();

    if (target != npes-1) {
        printf("%d: Invalid target (%ld) in NBI cswap test\n", me, target);
        ++errors;
    }

    for (i = 0; i < npes; i++) {
        if (result[i] != me-1) {
            printf("%d: Invalid result (%ld) in NBI cswap test\n", me, result[i]);
            ++errors;
        }
    }

    shmem_finalize();
    return errors != 0;
}
