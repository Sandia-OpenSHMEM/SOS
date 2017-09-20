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
 * shmem_collect test cat {-v -l loops} {nLongs(12)} {loopIncr(0)}
 *
 * For yod/mpirun -c/np X
 */
#include <shmem.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

#define AMEG (1024UL*1024UL)
#define AGIG (1024UL*1024UL*1024UL)

#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#endif

#define Rfprintf if (shmem_my_pe() == 0) fprintf
#define Rprintf if (shmem_my_pe() == 0)  printf
#define RVfprintf if (Verbose && shmem_my_pe() == 0) fprintf
#define RVprintf if (Verbose && shmem_my_pe() == 0)  printf
#define Vprintf if (Verbose)  printf
#define Vfprintf if (Verbose)  fprintf

#define RDprintf if (Verbose && shmem_my_pe() == 0)  printf
#define RDfprintf if (Verbose && shmem_my_pe() == 0) fprintf

/* option flags */
int Verbose;

#define DFLT_NWORDS 16
#define DFLT_INCR 32
#define DFLT_LOOPS 20

#define VAL 0xCafeBabe

long *dst;
long *src;
long pSync[SHMEM_COLLECT_SYNC_SIZE];

static int
atoi_scaled(char *s)
{
    long val;
    char *e;

    val = strtol(s,&e,0);
    if (e == NULL || *e =='\0')
        return val;

    if (*e == 'k' || *e == 'K')
        val *= 1024;
    else if (*e == 'm' || *e == 'M')
        val *= 1024*1024;
    else if (*e == 'g' || *e == 'G')
        val *= 1024*1024*1024;

    return (int) val;
}

int
main(int argc, char* argv[])
{
    int c, j, cloop, loops = DFLT_LOOPS;
    int mpe, num_pes;
    int nWords=1;
    int nIncr=1;
    int failures=0;
    char *pgm;

    shmem_init();
    mpe = shmem_my_pe();
    num_pes = shmem_n_pes();

    if (num_pes == 1) {
        Rfprintf(stderr,
                 "ERR - Requires > 1 PEs\n");
        shmem_finalize();
        return 0;
    }
    pgm = strrchr(argv[0],'/');
    if ( pgm )
        pgm++;
    else
        pgm = argv[0];

    while((c=getopt(argc,argv,"hqVvl:")) != -1) {
        switch(c) {
            case 'V':
            case 'v':
                Verbose++;
                break;
            case 'l':
                loops = atoi(optarg);
                break;
            case 'h':
                Rfprintf(stderr,
                         "usage: %s {-l loopcnt(%d)} {numLongs(%d)} {loopIncr(%d)}\n",
                         pgm,DFLT_LOOPS,DFLT_NWORDS,DFLT_INCR);
                shmem_finalize();
                return 1;
            default:
                shmem_finalize();
                return 1;
        }
    }

    if (optind == argc)
        nWords = DFLT_NWORDS;
    else {
        nWords = atoi_scaled(argv[optind++]);
        if (nWords <= 0) {
            Rfprintf(stderr, "ERR - Bad nBytes arg?\n");
            shmem_finalize();
            return 1;
        }
    }

    if (optind == argc)
        nIncr = DFLT_INCR;
    else {
        loops = atoi(argv[optind++]);
        if (nIncr <= 0 ) {
            Rfprintf(stderr, "ERR - incLongs arg out of bounds '%d'?\n", nIncr);
            shmem_finalize();
            return 1;
        }
    }

    if ( nWords % 8 ) { // integral multiple of longs
        Rprintf("%s: nWords(%d) not a multiple of %ld?\n",
                pgm,nWords,sizeof(long));
        shmem_finalize();
        return 1;
    }

    for (c = 0; c < SHMEM_COLLECT_SYNC_SIZE;c++)
        pSync[c] = SHMEM_SYNC_VALUE;

    if (Verbose && mpe == 0)
        fprintf(stderr,"loops(%d) nWords(%d) incr-per-loop(%d)\n",
                loops,nWords,nIncr);

    for(cloop=1; cloop <= loops; cloop++) {

        c = (sizeof(long)*nWords) * (num_pes + 1); // src + dst allocation.
        //nWords /= sizeof(long); // convert input of bytes --> longs.

        src = (long*)shmem_malloc(c);
        if ( !src ) {
            Rprintf("[%d] %s: shmem_malloc(%d) failed?\n", mpe, pgm,c);
            shmem_global_exit(1);
        }
        dst = &src[nWords];

        for(j=0; j < nWords; j++)
            src[j] = (long) (j + mpe*nWords);

        shmem_barrier_all();

        shmem_fcollect64(dst,src,nWords,0,0,num_pes,pSync);

        // Expect dst to be consecuative integers 0 ... (nLongs*num_pes)-1
        for(j=0; j < (nWords*num_pes); j++) {
            if ( dst[j] != (long) j ) {
                fprintf(stderr,
                        "[%d] dst[%d] %ld != expected %d\n",mpe,j,dst[j],j);
                shmem_global_exit(1);
            }
        }
        shmem_barrier_all();

        if (Verbose && mpe == 0 && loops > 1) {
            fprintf(stderr,".");
        }
        nWords += nIncr;
    }

    if (Verbose && mpe == 0) {
        fprintf(stderr,"\n");fflush(stderr);
    }
    shmem_free( (void*)src );
    shmem_barrier_all();
    if (Verbose)
        printf("%d(%d) Exit(%d)\n", mpe, num_pes, failures);

    shmem_finalize();

    return failures;
}
