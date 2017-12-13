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
 * test shmem_int_atomic_inc() atomic_inc {-v|q} {loop-cnt(default=10)(default=10)}
 * where: -q == quiet, -v == verbose/debug
 *  Loop for loop-cnt
 *   all PEs call shmem_int_atomic_inc(), PE-0 totals
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <shmem.h>

#define Rfprintf if (shmem_my_pe() == 0) fprintf
#define Rprintf  if (shmem_my_pe() == 0)  printf
#define RDfprintf if (Verbose && shmem_my_pe() == 0) fprintf
#define RDprintf if (Verbose && shmem_my_pe() == 0)  printf
#define Vprintf  if (Verbose)  printf
#define Vfprintf if (Verbose) fprintf

int Verbose;
int lock_cnt;
long lock;

int
main(int argc, char* argv[])
{
    int c, cloop, loops;
    int my_rank, num_ranks;
    int Announce = (NULL == getenv("MAKELEVEL")) ? 1 : 0;

    shmem_init();
    my_rank = shmem_my_pe();
    num_ranks = shmem_n_pes();
    if (num_ranks == 1) {
        fprintf(stderr, "ERR - Requires > 1 PEs\n");
        shmem_finalize();
        return 0;
    }

    while((c=getopt(argc,argv,"vq")) != -1) {
        switch(c) {
          case 'v':
            Verbose++;
            break;
          case 'q':
            Announce = 0;
            break;
          default:
            Rfprintf(stderr,"ERR - unknown -%c ?\n",c);
            shmem_finalize();
            return 1;
        }
    }

    if (optind == argc)
        loops = 10;
    else {
        loops = atoi(argv[optind++]);
        if (loops <= 0 || loops > 1000000) {
                Rfprintf(stderr,
                "ERR - loops arg out of bounds '%d'?\n", loops);
            shmem_finalize();
            return 1;
        }
    }

    for(cloop=1; cloop <= loops; cloop++)
    {
        lock_cnt = 0;
        shmem_barrier_all();  /* sync all ranks */

        for(c=0; c < num_ranks; c++)
            shmem_int_atomic_inc( &lock_cnt, c );

        Vprintf("[%d] locked: lock_cnt(%d)\n", my_rank, lock_cnt);

        shmem_int_wait_until( &lock_cnt, SHMEM_CMP_GE, num_ranks );

        shmem_barrier_all();  /* sync all ranks */

        if (lock_cnt != num_ranks)
            printf ("[%d] loop %d: bad lock_cnt %d, expected %d?\n",
                    my_rank, cloop, lock_cnt, num_ranks);

        if ( (cloop % 10) == 0 ) {
            if (my_rank == 0 && Announce)
                printf("%d ranks completed %d loops\n", num_ranks, cloop);
        }
    }

    Vprintf ("[%d] of %d, Exit: lock_cnt %d\n",
                my_rank, num_ranks, lock_cnt);

    shmem_finalize();

    return 0;
}
