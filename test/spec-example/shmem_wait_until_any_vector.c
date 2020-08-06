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
#include <stdlib.h>

#define N 100

int main(void)
{
    int total_sum = 0;

    shmem_init();
    int mype = shmem_my_pe();
    int npes = shmem_n_pes();

    int *ivars = shmem_calloc(npes, sizeof(int));
    int *status = calloc(npes, sizeof(int));
    int *cmp_values = malloc(npes * sizeof(int));

    /* All odd PEs put 2 and all even PEs put 1 */
    for (int i = 0; i < npes; i++) {
        shmem_int_atomic_set(&ivars[mype], mype % 2 + 1, i);

        /* Set cmp_values to the expected values coming from each PE */
        cmp_values[i] = i % 2 + 1;
    }

    size_t completed_idx;
    for (int i = 0; i < npes; i++) {
        completed_idx = shmem_int_wait_until_any_vector(ivars, npes, status, SHMEM_CMP_EQ, cmp_values);
        status[completed_idx] = 1;
        total_sum += ivars[completed_idx];
    }

    /* check the result */
    int correct_result = npes + npes / 2;
    if (total_sum != correct_result) {
        shmem_global_exit(1);
    }

    shmem_finalize();
    return 0;
}
