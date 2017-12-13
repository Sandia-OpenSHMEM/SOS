/*
 *  Copyright (c) 2016 Intel COrporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 *      Redistribution and use in source and binary forms, with or
 *      without modfiication, are permitted provided that the following
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

/* Non-Blocking Put Test
 * Tom St. John <tom.st.john@intel.com>
 * January, 2016
 *
 * PE 0 uses non-blocking put to write a message followed by a
 * notification flag to every remote PE,
 */

#include <shmem.h>
#include <string.h>
#include <stdio.h>

int
main(int argc, char* argv[])
{
    long source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    long *target;
    int *flag;
    int i, num_pes;
    int failed = 0;

    shmem_init();

    target = (long*) shmem_malloc(sizeof(long) * 10);
    flag = (int*) shmem_malloc(sizeof(int));
    if (!flag) {
        fprintf(stderr, "ERR - null flag pointer\n");
        shmem_global_exit(1);
    }
    *flag = 0;

    num_pes=shmem_n_pes();

    if (target) {
        memset(target, 0, sizeof(long)*10);
    } else {
        fprintf(stderr, "ERR - null target pointer\n");
        shmem_global_exit(1);
    }

    shmem_barrier_all();

    if (shmem_my_pe() == 0) {
        for(i = 0; i < num_pes; i++) {
            shmem_long_put_nbi(target, source, 10, i);
            shmem_fence();
            shmem_int_atomic_inc(flag, i);
        }
    }

    shmem_int_wait_until(flag, SHMEM_CMP_EQ, 1);

    for (i = 0; i < 10; i++) {
        if (target[i] != source[i]) {
            fprintf(stderr,"[%d] target[%d] = %ld, expected %ld\n",
                    shmem_my_pe(), i, target[i], source[i]);
            failed = 1;
        }
    }

    shmem_free(target);
    shmem_free(flag);

    shmem_finalize();

    return failed;
}
