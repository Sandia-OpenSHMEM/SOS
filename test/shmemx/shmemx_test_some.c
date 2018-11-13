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
 *
 * This test is derived from an example provided in the OpenSHMEM 1.4
 * specification.  Additional copyrights may apply.
 *
 */

#include <shmem.h>
#include <shmemx.h>
#include <stdlib.h>

#define N 100

int main(void)
{
    int total_sum = 0;

    shmem_init();
    int mype = shmem_my_pe();
    int npes = shmem_n_pes();

    int *my_data = malloc(N * sizeof(int));
    int *all_data = shmem_malloc(N * npes * sizeof(int));

    int *flags = shmem_calloc(npes, sizeof(int));
    size_t *indices = calloc(npes, sizeof(size_t));
    int *status = calloc(npes, sizeof(int));

    for (int i = 0; i < N; i++)
        my_data[i] = mype*N + i;

    for (int i = 0; i < npes; i++)
        shmem_int_put_nbi(&all_data[mype*N], my_data, N, i);

    shmem_fence();

    for (int i = 0; i < npes; i++)
        shmem_int_p(&flags[mype], 1, i);

    int ncompleted = 0;

    while (ncompleted < npes) {
        int ntested = shmemx_int_test_some(flags, npes, indices, status, SHMEM_CMP_NE, 0);
        if (ntested > 0) {
            for (int i = 0; i < ntested; i++) {
                for (int j = 0; j < N; j++)
                    total_sum += all_data[indices[i]*N + j];
            }
            ncompleted += ntested;
        } else {
            /* Overlap some computation here */
        }
    }

    /* Check the flags array */
    for (int i = 0; i < npes; i++) {
        if (flags[i] != 1)
            shmem_global_exit(0);
    }

    /* check result */
    int M = N * npes - 1;
    if (total_sum != M * (M + 1) / 2) {
        shmem_global_exit(1);
    }

    /* Sanity check case with NULL status array */
    ncompleted = shmemx_int_test_some(flags, npes, indices, NULL, SHMEM_CMP_EQ, 1);

    if (ncompleted != npes)
        shmem_global_exit(2);

    shmem_finalize();
    return 0;
}
