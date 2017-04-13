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
 */

#include <stdio.h>
#include <shmem.h>
#define _IPUT(a) shmem_##a##_iput

//#define IPUT _IPUT(double)
#define IPUT shmem_iput128
#define DataType long double

static DataType source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static DataType target[10];

int main(int argc, char **argv)
{
    int me, nProcs, rc=0;

    shmem_init();
    me = shmem_my_pe();
    nProcs = shmem_n_pes();

    if (me == 0) {
        int j;
        /* put 5 words into target on PE's [1 to (nProcs-1)] */
        for(j=1; j < nProcs; j++)
            IPUT (target, source, 1, 2, 5, j);
    }

    shmem_barrier_all(); /* sync sender and receiver */

    if (me != 0) {
        if (target[0] != 1 ||
            target[1] != 3 ||
            target[2] != 5 ||
            target[3] != 7 ||
            target[4] != 9)
        {
            printf("ERR: target on PE %d is %Lf %Lf %Lf %Lf %Lf\n"
                   "  Expected 1,3,5,7,9?\n",
                   me, target[0], target[1], target[2],
                   target[3], target[4] );
            rc = 1;
        }
    }

    shmem_finalize();

    return rc;
}
