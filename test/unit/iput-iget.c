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
 * Enhanced to not limit PE count.
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <shmem.h>

#define WRDS 5

short source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static short target[10];
static short *results;

int
main(int argc, char **argv)
{
    int me, nProcs, rc=0, j, k;

    shmem_init();
    me = shmem_my_pe();
    nProcs = shmem_n_pes();

    if (me == 0) {
        /* put words into target on PE's [1 to (nProcs-1)] */
        for(j=1; j < nProcs; j++)
            shmem_short_iput(target, source, 1, 2, WRDS, j);
    }

    results = (short*)shmem_malloc(nProcs * WRDS * sizeof(short));
    assert(results);
    memset((void*)results, 0, (nProcs * WRDS * sizeof(short)));

    shmem_barrier_all(); /* sync sender and receiver */

    if (me != 0) {
        // Verify iput data is expected
        if (target[0] != 1 ||
            target[1] != 3 ||
            target[2] != 5 ||
            target[3] != 7 ||
            target[4] != 9)
        {
            printf("%s ERR: target on PE %d is %hd %hd %hd %hd %hd\n"
                "  Expected 1,3,5,7,9?\n", argv[0],
                me, target[0], target[1], target[2],
                target[3], target[4] );
            rc = 1;
        }
    }

    shmem_barrier_all();

    if (rc != 0)
        shmem_global_exit(rc);

    if (me == 0) {
        // Pull data from PE1...(nProcs-1)
        // iget() nProcs target[0...4] --> results[Proc*WRDS ...Proc*WRDS+WRDS]

        for(j=1,k=0; j < nProcs; j++,k+=WRDS)
            shmem_short_iget(&results[k], target, 1, 1, WRDS, j);

        for(j=1,k=0; j < nProcs; j++,k+=WRDS) {
            if (results[k] != 1 ||
                results[k+1] != 3 ||
                results[k+2] != 5 ||
                results[k+3] != 7 ||
                results[k+4] != 9)
            {
                printf("%s [%d] ERR: results %hd %hd %hd %hd %hd\n"
                    "  Expected 1,3,5,7,9?\n", argv[0],
                    me, results[k], results[k+1], results[k+2],
                    results[k+3], results[k+4] );
                rc = 1;
            }
        }
    }
#if _DEBUG
    else {
        printf("%s [%d] target is %hd %hd %hd %hd %hd\n", argv[0],
                me, target[0], target[1], target[2],
                target[3], target[4] );
    }
#endif

    shmem_barrier_all(); /* sync before exiting */
    shmem_free(results);

    shmem_finalize();

    return rc;
}
