/*
 *  Copyright (c) 2019 Intel Corporation. All rights reserved.
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

int main(void) {
    int i, n, errors = 0;
    int me, npes;

    static int shr_data = -1;
    int * shr_heap;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    shr_heap = shmem_malloc(sizeof(int));

     shr_data = me;
    *shr_heap = me;

    shmem_barrier_all();

    /* Check shmem_ptr on data segment */

    for (i = n = 0; i < npes; i++) {
        int * ptr = (int *) shmem_ptr(&shr_data, i);

        if (ptr != NULL) {
            int shr_peer = *ptr;
            ++n;

            if (shr_peer != i) {
                printf("%2d: Error, shr_data(%d) = %d, expected %d\n", me, i, shr_peer, i);
                errors++;
            }
        }
        else if (i == me) {
                printf("%2d: Error, shmem_ptr(data) returned NULL for my PE\n", me);
                errors++;
        }
    }

    printf("%2d: Found %d data segment peer(s)\n", me, n);
    fflush(NULL);
    shmem_barrier_all();

    /* Check shmem_ptr on heap segment */

    for (i = n = 0; i < npes; i++) {
        int * ptr = (int *) shmem_ptr(shr_heap, i);

        if (ptr != NULL) {
            int shr_peer = *ptr;
            ++n;

            if (shr_peer != i) {
                printf("%2d: Error, shr_heap(%d) = %d, expected %d\n", me, i, shr_peer, i);
                errors++;
            }
        }
        else if (i == me) {
                printf("%2d: Error, shmem_ptr(heap) returned NULL for my PE\n", me);
                errors++;
        }
    }

    printf("%2d: Found %d heap segment peer(s)\n", me, n);

    shmem_finalize();

    return errors != 0;
}
