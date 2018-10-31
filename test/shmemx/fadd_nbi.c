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

#include <stdlib.h>
#include <stdio.h>
#include <shmem.h>
#include <shmemx.h>

long ctr = 0;

int main(void) {
    int me, npes, i;
    long *out;
    double t;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    out = malloc(sizeof(long) * npes);

    /* Test blocking fetch-add */

    ctr = 0;
    shmem_barrier_all();
    t = shmemx_wtime();

    for (i = 0; i < npes; i++) {
        out[i] = shmem_long_atomic_fetch_add(&ctr, 1, i);
    }

    shmem_barrier_all();
    t = shmemx_wtime() - t;

    if (me == 0) printf("fetch_add     %10.2fus\n", t*1000000);

    if (ctr != npes)
        shmem_global_exit(1);

    for (i = 0; i < npes; i++)
        if (!(out[i] >= 0 && out[i] < npes))
            shmem_global_exit(2);

    /* Test NBI fetch-add */

    ctr = 0;
    shmem_barrier_all();
    t = shmemx_wtime();

    for (i = 0; i < npes; i++) {
        shmemx_long_atomic_fetch_add_nbi(&out[i], &ctr, 1, i);
    }

    shmem_barrier_all();
    t = shmemx_wtime() - t;

    if (me == 0) printf("fetch_add_nbi %10.2fus\n", t*1000000);

    if (ctr != npes)
        shmem_global_exit(1);

    for (i = 0; i < npes; i++)
        if (!(out[i] >= 0 && out[i] < npes))
            shmem_global_exit(2);

    shmem_finalize();
    return 0;
}
