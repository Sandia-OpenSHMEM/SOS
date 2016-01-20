/*
 *  Copyright (c) 2016 Intel Corporation. All rights reserved.
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

/* Non-Blocking Get Test
 * Tom St. John <tom.st.john@intel.com>
 * January, 2016
 *
 * PE 0 uses a non-blocking get to copy an array from
 * every remote PE.
 */

#include <shmem.h>

#include <string.h>
#include <stdio.h>

static long source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static long target[10];

int
main(int argc, char* argv[])
{
    int i, j, num_pes;;

    shmem_init();

    if (shmem_my_pe() == 0) {
        num_pes=shmem_n_pes();

        for(j = 0;j < num_pes; j++) {
            memset(target, 0, sizeof(target));
            /* get 10 elements from source on remote PEs */
            shmem_long_get_nbi(target, source, 10, j);
            shmem_quiet();

            if (0 != memcmp(source, target, sizeof(long) * 10)) {
                fprintf(stderr,"[%d] Src & Target mismatch?\n",shmem_my_pe());
                for (i = 0 ; i < 10 ; ++i) {
                    printf("%ld,%ld ", source[i], target[i]);
                }
                printf("\n");
                shmem_global_exit(1);
            }
        }
    }

    shmem_finalize();

    return 0;
}
