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
#include <stdint.h>
#include <stdlib.h>
#include <shmem.h>
#include <shmemx.h>

#define N 128
#define SHMEM_MALLOC_INVALID_HINT ~(SHMEM_MALLOC_ATOMICS_REMOTE)

int main(int argc, char **argv) {
    int npes, mype;
    int errors = 0;

    shmem_init();

    npes = shmem_n_pes();
    mype = shmem_my_pe();

    int *src[N];

    /* Allocate an array of N buffers on the symmeytric heap */
    for(int i = 0; i < N; i++)
        src[i] = (int *)shmem_malloc_with_hints(N * sizeof(int), SHMEMX_MALLOC_NO_BARRIER);
    int *dst = (int *)shmem_malloc(N * sizeof(int));

    for (int i = 0; i < N; i++) {
        for (int j = 0; j < N; j++) {
            src[i][j] = -1;
        }
    }

    /* src is initialized to become a diagonal matrix */
    for (int i = 0; i < N; i++) {
        src[i][i] = i;
        dst[i] = -1;
    }

    shmem_sync_all(); /* sync sender and receiver */

    if (shmem_my_pe() == 0) {
        for (int i = 0; i < N; i++) {
            /* put elements from src's diagonal into dst on PE 1 */
            shmem_int_put(&dst[i], &src[i][i], 1, 1);
        }
    }

    shmem_barrier_all();  /* sync sender and receiver */

    if (shmem_my_pe() == 1) {
        for (int i = 0 ; i < N ; ++i) {
            if (src[i][i] != dst[i]) {
                printf("%d,%d ", src[i][i], dst[i]);
                ++errors;
            }
        }
        if(errors) {
            printf("\nFailed with %d errors\n", errors);
            shmem_global_exit(errors);
        }
    }

    for (int i = 0; i < N; i++)
        shmem_free(src[i]);
    shmem_free(dst);

    if (shmem_my_pe() == 0)
        printf("Passed with 0 errors\n");

    shmem_finalize();
        
    return 0;
}
