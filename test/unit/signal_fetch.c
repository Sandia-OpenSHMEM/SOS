/*
 *  Copyright (c) 2020 Intel Corporation. All rights reserved.
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

/* 
 * Validate signal_fetch operation using blocking put_signal
*/

#include <stdio.h>
#include <shmem.h>
#include <string.h>

#include <shmemx.h>

#define MSG_SZ 10

int main(int argc, char *argv[])
{
    long source[MSG_SZ];
    long *target;
    int me, npes, i;
    int errors = 0;

    static uint64_t sig_addr = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    for (i = 0; i < MSG_SZ; i++)
        source[i] = i;

    target = (long *) shmem_calloc(MSG_SZ, sizeof(long));
    if (!target) {
        fprintf(stderr, "Failed to allocate target pointer\n");
        shmem_global_exit(1);
    }

    shmem_barrier_all();
    for (i = 0; i < npes; i++) {
        shmem_long_put_signal(target, source, MSG_SZ, &sig_addr, me, SHMEM_SIGNAL_ADD, i);
    }

    uint64_t sig_value = shmem_signal_fetch(&sig_addr);
    while (sig_value != (uint64_t) ((npes * (npes - 1)) / 2)) {
#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
        shmem_wait_until(&sig_addr, SHMEM_CMP_NE, 0);
#else
        shmem_uint64_wait_until(&sig_addr, SHMEM_CMP_NE, 0);
#endif
        sig_value = shmem_signal_fetch(&sig_addr);
    }

    for (i = 0; i < MSG_SZ; i++) {
        if (target[i] != source[i]) {
            fprintf(stderr, "%10d: target[%d] = %ld not matching %ld with SHMEM_SIGNAL_ADD\n",
                    me, i, target[i], source[i]);
            errors++;
        }
    }
    
    shmem_free(target);
    shmem_finalize();

    return errors;
} 
