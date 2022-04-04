/*
 *  Copyright (c) 2022 Intel Corporation. All rights reserved.
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

long src[NELEM];

int main(void)
{
    int me, npes;
    int errors = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    for (int i = 0; i < NELEM; i++)
        src[i] = me + i;

    shmem_barrier_all();

    shmem_long_broadcast(SHMEM_TEAM_WORLD, src, src, NELEM, 0);

    /* Validate reduced data */
    for (int j = 0; j < NELEM; j++) {
        long expected = j;
        if (src[j] != expected) {
            printf("%d: Expected src[%d] = %ld, got src[%d] = %ld\n", me, j, expected, j, src[j]);
            errors++;
        }
    }

    shmem_finalize();

    return errors != 0;
}
