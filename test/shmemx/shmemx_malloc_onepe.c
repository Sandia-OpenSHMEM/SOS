/*
 *  Copyright (c) 2023 Intel Corporation. All rights reserved.
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
#include <shmemx.h>

int main(int argc, char **argv) {
    int ret = 0;

    int onepe_data_size = 1;
    int *onepe_alloc = NULL;

    static ptrdiff_t onepe_alloc_ptr_original = 0, onepe_alloc_ptr_retrieved = 0;
    int onepe_alloc_val_pe1 = -1;

    shmem_init();

    int me = shmem_my_pe();
    int npes = shmem_n_pes();

    if (me == 1) {
        onepe_alloc = (int *) shmemx_malloc_onepe(onepe_data_size * sizeof(int));
        onepe_alloc[0] = npes + 78;
        onepe_alloc_ptr_original = shmemx_ptrdiff_of_ptr(onepe_alloc);	
        fprintf(stderr, "PE 1 allocated and set the onepe allocation\n");
    }

    shmem_barrier_all();

    if (me != 1) {
        shmem_ptrdiff_get(&onepe_alloc_ptr_retrieved, &onepe_alloc_ptr_original, 1, 1);
        void *onepe_alloc_pe1 = shmemx_ptr_of_ptrdiff(onepe_alloc_ptr_retrieved);
        shmem_int_get(&onepe_alloc_val_pe1, onepe_alloc_pe1, 1, 1);
        fprintf(stderr, "PE 1's onepe allocation is retrieved by PE %d\n", me);
    }

    shmem_barrier_all();

    if (me != 1) {
        if (onepe_alloc_val_pe1 != (npes + 78)) {
            fprintf(stderr, "ERROR: Wrong value retrieved from one_pe allocation %d, expected = %d\n", 
                             onepe_alloc_val_pe1, npes + 78);
            ret = 1;
        }
    }

    if (me == 1)
        shmemx_free_onepe(onepe_alloc);

    shmem_finalize();

    return ret;
}
