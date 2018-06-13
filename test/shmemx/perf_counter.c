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
 */

/* Single-threaded test for validation of performance counter APIs
*/

#include <stdio.h>
#include <shmemx.h>

#define ITER 100
#define WINDOW 64
#define LENGTH 1024

int me, npes;
char *src_array, *dest_array;
uint64_t c_put, c_get, p_put, p_get, target;

static void collect(shmem_ctx_t ctx) {
    shmemx_pcntr_get_completed_put(ctx, &c_put);
    shmemx_pcntr_get_completed_get(ctx, &c_get);
    shmemx_pcntr_get_completed_target(ctx, &target);
    shmemx_pcntr_get_pending_put(ctx, &p_put);
    shmemx_pcntr_get_pending_get(ctx, &p_get);
}

static void put_and_progress_check(void) { 
    int i, j;
    int partner = ((npes % 2 == 0) ? (me % 2 == 0 ? me + 1 : me - 1) : 
                                     (me % 2 != 0 ? me - 1 : 
                                     (me == npes - 1) ? me : me + 1));

    shmem_ctx_t ctx;
    shmemx_pcntr_t pcntr;
    shmem_ctx_create(SHMEM_CTX_PRIVATE, &ctx);

    for (i = 0; i < ITER; i++) {
        for (j = 0; j < WINDOW; j++) {
            shmem_ctx_putmem_nbi(ctx, dest_array, src_array, LENGTH, partner);
            collect(ctx);
        }
        shmem_ctx_quiet(ctx);
    }

    shmemx_pcntr_get_all(ctx, &pcntr);
    shmem_ctx_destroy(ctx);

    /* Report the counter values observed through get_all API after the loop 
     * completion. Except the target counter, other counter values should 
     * reflect the final expected value */
    printf("Value observed of the performance counters from combined API: \n"
           "Completed Put = %10ld\n"
           "Completed Get = %10ld\n"
           "Pending Put   = %10ld\n"
           "Pending Get   = %10ld\n"
           "Target        = %10ld\n"
           , pcntr.completed_put, pcntr.completed_get, pcntr.pending_put, 
           pcntr.pending_get, pcntr.target);

    return;
}

int main(int argc, char **argv) {

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    src_array = shmem_malloc(LENGTH);
    dest_array = shmem_malloc(LENGTH);

    if (me == 0) {
        printf("Performance counter API test with %d PEs\n", npes);
    }

    put_and_progress_check();
    shmem_barrier_all();

    /* Report the counter values observed through single parameter APIs in 
     * the final iteration. The values reported here may be less than the actual
     * final value as they are captured before the barrier one counter at a time 
     * */
    printf("Final value observed of the performance counters from individual APIs: \n"
           "Completed Put = %10ld\n"
           "Completed Get = %10ld\n"
           "Pending Put   = %10ld\n"
           "Pending Get   = %10ld\n"
           "Target        = %10ld\n"
           , c_put, c_get, p_put, p_get, target); 

    shmem_free(dest_array);
    shmem_free(src_array);

    shmem_finalize();
    return 0;
}
