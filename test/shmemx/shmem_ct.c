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
#include <stdlib.h>
#include <shmem.h>
#include <shmemx.h>

int data;

int main(int argc, char **argv) {
    int me, npes, i;
    int mine, ct_val;
    shmemx_ct_t ct;

    shmem_init();
    me = mine = shmem_my_pe();
    npes = shmem_n_pes();

    shmemx_ct_create(&ct);
    ct_val = shmemx_ct_get(ct);

    if (ct_val != 0) {
        printf("[%3d] Error: Initial value of ct was nonzero (%d)\n", me, ct_val);
        shmem_global_exit(1);
    }

    /* TEST 1: Everyone puts to rank 0 */
    shmem_barrier_all();
    shmemx_putmem_ct(ct, &data, &mine, sizeof(int), 0);

    if (me == 0) {
        shmemx_ct_wait(ct, npes);
    }
    shmem_barrier_all();

    /* Reset the counter */
    shmemx_ct_set(ct, 0);
    ct_val = shmemx_ct_get(ct);
    if (ct_val != 0) {
        printf("[%3d] Error: Reset value of ct was nonzero (%d)\n", me, ct_val);
        shmem_global_exit(1);
    }

    /* TEST 1.5: Everyone gets from rank 0 */
    data = -1;
    shmem_barrier_all();
    shmemx_getmem_ct(ct, &mine, &data, sizeof(int), 0);

    if (me == 0) {
        shmemx_ct_wait(ct, npes);
    }
    shmem_barrier_all();

    if (mine != -1) {
        printf("[%3d] Error: Getmem_ct value read was incorrect (%d)\n", me, mine);
        shmem_global_exit(1);
    }

    /* Reset the counter */
    shmemx_ct_set(ct, 0);
    ct_val = shmemx_ct_get(ct);
    if (ct_val != 0) {
        printf("[%3d] Error: Reset value of ct was nonzero (%d)\n", me, ct_val);
        shmem_global_exit(1);
    }

    /* TEST 2: Everyone puts to every other rank, but not themselves */
    shmem_barrier_all();

    for (i = 1; i < npes; i++)
        shmemx_putmem_ct(ct, &data, &mine, sizeof(int), (me + i) % npes);

    shmemx_ct_wait(ct, npes-1);

    shmem_barrier_all();
    shmemx_ct_free(&ct);

    shmem_finalize();

    return 0;
}
