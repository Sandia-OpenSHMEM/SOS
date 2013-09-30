/*
 *
 * Copyright (c) 2011, 2012
 *  University of Houston System and Oak Ridge National Laboratory.
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * o Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 *
 * o Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 *
 * o Neither the name of the University of Houston System, Oak Ridge
 *   National Laboratory nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>

int dest;

int main(int argc, char **argv) {
    int me, npes, i;
    int src, ct_val;
    shmem_ct_t ct;

    start_pes(0);
    me = src = _my_pe();
    npes = _num_pes();

    shmem_ct_create(&ct);
    ct_val = shmem_ct_get(ct);

    if (ct_val != 0) {
        printf("[%3d] Error: Initial value of ct was nonzero (%d)\n", me, ct_val);
        return 1;
    }

    /* TEST 1: Everyone puts to rank 0 */
    shmem_barrier_all();
    shmem_putmem_ct(ct, &dest, &src, sizeof(int), 0);

    if (me == 0) {
        shmem_ct_wait(ct, npes);
    }
    shmem_barrier_all();

    /* Reset the counter */
    shmem_ct_set(ct, 0);
    ct_val = shmem_ct_get(ct);
    if (ct_val != 0) {
        printf("[%3d] Error: Reset value of ct was nonzero (%d)\n", me, ct_val);
        return 1;
    }

    /* TEST 2: Everyone puts to every other rank, but not themselves */
    shmem_barrier_all();

    for (i = 1; i < npes; i++)
        shmem_putmem_ct(ct, &dest, &src, sizeof(int), (me + i) % npes);

    shmem_ct_wait(ct, npes-1);

    shmem_barrier_all();
    shmem_ct_free(&ct);

    return 0;
}
