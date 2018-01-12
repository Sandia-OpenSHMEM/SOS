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

#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>

#define NUM_CTX 32

long data = 0;

int main(int argc, char **argv) {
    int me, npes, i;
    int errors = 0;
    shmem_ctx_t ctx[NUM_CTX];

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    for (i = 0; i < NUM_CTX; i++) {
        int err = shmem_ctx_create(0, &ctx[i]);

        if (err) {
            printf("%d: Error creating context %d (%d)\n", me, i, err);
            shmem_global_exit(1);
        }
    }

    for (i = 0; i < NUM_CTX; i++)
        shmem_ctx_long_atomic_inc(ctx[i], &data, (me+1) % npes);

    for (i = 0; i < NUM_CTX; i++)
        shmem_ctx_quiet(ctx[i]);

    shmem_sync_all();

    if (data != NUM_CTX) {
        printf("%d: error expected %d, got %ld\n", me, NUM_CTX, data);
        ++errors;
    }

    shmem_finalize();
    return errors;
}
