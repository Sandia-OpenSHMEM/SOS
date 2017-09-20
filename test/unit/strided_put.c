/*
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
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

/*
* adaptation of example from SGI man page for shmem_iput.
*
* Code slightly cleaned up (removed cache call, fixed % format)
*
*/
#include <stdio.h>
#include <shmem.h>

int
main(int argc, char* argv[])
{
    short source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    static short target[10];
    int me, num_pes;
    int ret = 0;

    shmem_init();
    me = shmem_my_pe();
    num_pes = shmem_n_pes();

    if (num_pes != 2) {
        printf("%s: Requires 2 PEs\n", argv[0]);
        shmem_finalize();
        return 0;
    }

    if (me == 0) {
        /* put 10 words into target on PE 1 */
        shmem_short_iput(target, source, 1, 2, 5, 1);
    }

    shmem_barrier_all(); /* sync sender and receiver */

    if (me == 1) {
        if (! (target[0] == source[0] &&
               target[1] == source[2] &&
               target[2] == source[4] &&
               target[3] == source[6] &&
               target[4] == source[8])) {
            printf("target on PE %d is %hd %hd %hd %hd %hd\n", me,
                   target[0], target[1], target[2],
                   target[3], target[4] );
            ret = 1;
        }
    }

    shmem_finalize();

    return ret;
}
