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
 * pingpong test  pingpong-short {-v} {numShorts-2-put(128)} {loop-cnt(10)}
 *
 * For yod -c X
 * Proc0 puts nShorts to Proc1 ... Proc(X-1)
 *   Proc1 ... Proc(X-1) verify put data to be correct.
 *   Proc1 ... Proc(X-1) put nShorts(proc_ID) back to proc0 in
 *     work[proc_ID*nLongs] ... work[proc_ID*(nLongs-1)]
 *   Proc 0 verifes data
 */
#include <shmem.h>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

static int atoi_scaled(char *s);

#define Rfprintf if (shmem_my_pe() == 0) fprintf
#define Rprintf if (shmem_my_pe() == 0)  printf

#define RDprintf if (Verbose && shmem_my_pe() == 0)  printf
#define RDfprintf if (Verbose && shmem_my_pe() == 0) fprintf

/* option flags */
#define OUTPUT_MOD 1 // output debug every X loops on -V
int output_mod = OUTPUT_MOD;
int Verbose;
int Slow;
long pSync0[SHMEM_BARRIER_SYNC_SIZE],
    pSync1[SHMEM_BARRIER_SYNC_SIZE],
    pSync2[SHMEM_BARRIER_SYNC_SIZE],
    pSync3[SHMEM_BARRIER_SYNC_SIZE],
    pSync4[SHMEM_BARRIER_SYNC_SIZE];

#define DFLT_NWORDS 128
#define DFLT_LOOPS 100

#define VAL 0xCafe

#define DataType short

DataType *Target;
DataType *src;
DataType *work;

int
main(int argc, char* argv[])
{
    int c, j, loops, k, l;
    int my_pe, nProcs, nWorkers;
    int  nWords=1;
    int  failures=0;
    char *prog_name;
    DataType *wp;
    long work_sz;

    for(j=0; j < SHMEM_BARRIER_SYNC_SIZE; j++) {
        pSync0[j] = pSync1[j] = pSync2[j] = pSync3[j] =
            pSync4[j] = SHMEM_SYNC_VALUE;
    }

    shmem_init();
    my_pe = shmem_my_pe();
    nProcs = shmem_n_pes();
    nWorkers = nProcs - 1;

    if (nProcs == 1) {
        Rfprintf(stderr,
                 "ERR - Requires > 1 PEs\n");
        shmem_finalize();
        return 0;
    }

    for(j=0; j < nProcs; j++)
        if ( shmem_pe_accessible(j) != 1 ) {
            fprintf(stderr,
                    "ERR - pe %d not accessible from pe %d\n",
                    j, my_pe);
        }

    prog_name = strrchr(argv[0],'/');
    if ( prog_name )
        prog_name++;
    else
        prog_name = argv[0];

    while((c=getopt(argc,argv,"hvM:s")) != -1) {
        switch(c) {
            case 's':
                Slow++;
                break;
            case 'v':
                Verbose++;
                break;
            case 'M':
                output_mod = atoi(optarg);
                if (output_mod <= 0) {
                    Rfprintf(stderr, "ERR - output modulo arg out of "
                             "bounds '%d'?\n", output_mod);
                    shmem_finalize();
                    return 1;
                }
                Rfprintf(stderr,"%s: output modulo %d\n",
                         prog_name,output_mod);
                break;
            case 'h':
                Rfprintf(stderr,
                         "usage: %s {nWords-2-put(%d)K/M} {Loop-count(%d)K/M}\n",
                         prog_name, DFLT_NWORDS, DFLT_LOOPS);
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
            Rfprintf(stderr, "ERR - Bad nWords '%d'?\n", nWords);
            shmem_finalize();
            return 1;
        }
    }

    if (optind == argc)
        loops = DFLT_LOOPS;
    else {
        loops = atoi_scaled(argv[optind++]);
        if (loops <= 0 || loops > 1000000) {
            Rfprintf(stderr,
                     "ERR - loops arg out of bounds '%d'?\n", loops);
            shmem_finalize();
            return 1;
        }
    }

    work_sz = (nProcs*nWords) * sizeof(DataType);
    work = shmem_malloc( work_sz );
    if ( !work ) {
        fprintf(stderr,"[%d] ERR - work = shmem_malloc(%ld) ?\n",my_pe,work_sz);
        shmem_global_exit(1);
    }

    Target = shmem_malloc( 2 * nWords * sizeof(DataType) ); // Target & src
    if ( !Target ) {
        fprintf(stderr,"[%d] ERR - Target = shmem_malloc(%ld) ?\n",
                my_pe, (nWords * sizeof(DataType)));
        shmem_global_exit(1);
    }
    src = &Target[nWords];

    if (Verbose) Rprintf("%s: %d loops of %d shorts per put/get cycle\n",
                         prog_name,loops,nWords);

    for(j=0; j < nWords; j++)
        src[j] = (DataType)VAL;

    for(j=0; j < loops; j++) {

#if _DEBUG
        if ( Verbose && (j==0 || (j % output_mod) == 0) )
            fprintf(stderr,"[%d] +(%d)\n", shmem_my_pe(),j);
#endif
        shmem_barrier(0, 0, nProcs, pSync0);
        if ( my_pe == 0 ) {
            int p;
            for(p=1; p < nProcs; p++)
                shmem_short_put(Target, src, nWords, p);
        }
        else {
            if (Slow) {
                /* wait for each put to complete */
                for(k=0; k < nWords; k++)
                    shmem_short_wait(&Target[k],my_pe);
            } else {
                /* wait for last word to be written */
                shmem_short_wait(&Target[nWords-1],my_pe);
            }
        }
#if _DEBUG
        if ( Verbose && (j==0 || (j % output_mod) == 0) )
            fprintf(stderr,"[%d] -(%d)\n", my_pe,j);
#endif
        shmem_barrier(0, 0, nProcs, pSync1);

        RDprintf("Workers[1 ... %d] verify Target data put by my_pe 0\n",
                 nWorkers);

        /* workers verify put data is expected */
        if ( my_pe != 0 ) {
            for(k=0; k < nWords; k++) {
                if (Target[k] != (DataType)VAL) {
                    fprintf(stderr, "[%d] Target[%d] %#hx "
                            "!= %#hx?\n",
                            my_pe,k,Target[k],(DataType)VAL);
                    failures++;
                }
                assert(Target[k] == (DataType)VAL);
                Target[k] = my_pe;
            }
        }
        else /* clear results buffer, workers will put here */
            memset(work, 0, work_sz);

        shmem_barrier(0, 0, nProcs, pSync2);

        RDprintf("Workers[1 ... %d] put Target data to PE0 work "
                 "vector\n",nWorkers);

        if ( my_pe != 0 ) {
            /* push nWords of val my_pe back to PE zero */
            shmem_short_put(&work[my_pe*nWords], Target, nWords, 0);
        }
        else {
            /* wait for procs 1 ... nProcs to complete put()s */
            for(l=1; l < nProcs; l++) {
                wp = &work[ l*nWords ]; // procs nWords chunk
#if 1
                /* wait for last DataType to be written from each PE */
                shmem_short_wait(&wp[nWords-1],0);
#else
                for(k=0; k < nWords; k++)
                    shmem_short_wait(&wp[k],0);
#endif
            }
        }

        shmem_barrier(0, 0, nProcs, pSync3);

        if ( my_pe == 0 ) {
            RDprintf("Loop(%d) PE0 verifing work data.\n",j);
            for(l=1; l < nProcs; l++) {
                wp = &work[ l*nWords ]; // procs nWords chunk
                for(k=0; k < nWords; k++) {
                    if (wp[k] != l) {
                        fprintf(stderr,
                                "[0] PE(%d)_work[%d] %hd "
                                "!= %hd?\n",
                                l,k,work[k],(DataType)l);
                        failures++;
                    }
                    assert(wp[k] == l);
                    break;
                }
                if (failures)
                    break;
            }
        }
        shmem_barrier(0, 0, nProcs, pSync4);

        if (loops > 1) {
            RDfprintf(stderr,".");
            RDprintf("Loop(%d) Pass.\n",j);
        }
    }
    RDfprintf(stderr,"\n");fflush(stderr);

    shmem_free( work );
    shmem_free( Target );

    shmem_barrier_all();

    RDprintf("%d(%d) Exit(%d)\n", my_pe, nProcs, failures);

    shmem_finalize();

    return failures;
}

static int
atoi_scaled(char *s)
{
    long val;
    char *e;

    val = strtol(s,&e,0);
    if (e == NULL || *e =='\0')
        return (int)val;

    if (*e == 'k' || *e == 'K')
        val *= 1024;
    else if (*e == 'm' || *e == 'M')
        val *= 1024*1024;
    else if (*e == 'g' || *e == 'G')
        val *= 1024*1024*1024;

    return (int)val;
}
