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
 * ipgm / shmem iput/iget multiple
 *  usage: ipgm [-rmhd] [nWords] [loops] [incWords-per-loop]
 *   where:
 *     -r   == use shmem_realloc() per loop
 *     -n   == use shmem_malloc() + shmem_free() per loop
 *     -h   == help text.
 *     -d/v == enable debug.
 *
 * start-loop
 *   shmem_malloc() or shmem_realloc() src and target arrays.
 *   PE0 puts nWords to PE1 ... PE(num_pes-1)
 *   PE* validates received data
 *   PE* adds shmem_my_pe() to each received data element.
 *   PE0 pulls/iget()s data from PE* into loop/PE specific results range.
 *   PE0 validates received for each PE.
 *   if shmem_malloc() then shmem_free()
 *   nWords += incWords
 * end-loop
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <shmem.h>

#define DFLT_NWORDS 16
#define DFLT_LOOPS 16
#define DFLT_INCR 16

#define DataType long
#define IPUT shmem_long_iput
#define IGET shmem_long_iget

static DataType *source;
static DataType *target;
static DataType *results;

static char *pgm;
static int Debug; /* ==1 std dbg, 2==more, 3==max */

#define Dprintf if (Debug && me==0) printf
#define Zprintf if (me==0) printf

void usage (void);
int getSize (char *);
int target_data_good(DataType *, int, int, int);

void
usage (void)
{
    if (shmem_my_pe() == 0) {
        fprintf (stderr,
            "Usage: %s {-rmhv}  [nWords(%d)] [loops(%d)] [incWords(%d)]\n",
            pgm,DFLT_NWORDS,DFLT_LOOPS,DFLT_INCR);
        fprintf (stderr,
            "  where:\n"
            "    -r == use shmem_realloc() instead of shmem_malloc()\n"
            "    -m == use shmem_malloc() instead of shmem_realloc()\n"
            "    -v == Verbose output\n"
            "    -h == help.\n");
    }
    shmem_global_exit(1);
}

int
getSize (char *str)
{
    int size;
    char mod[32];

    switch (sscanf (str, "%d%1[mMkK]", &size, mod))
    {
    case 1:
        return (size);

    case 2:
        switch (*mod)
        {
        case 'm':
        case 'M':
            return (size << 20);

        case 'k':
        case 'K':
            return (size << 10);

        default:
            return (size);
        }

    default:
        return (-1);
    }
}

int
target_data_good( DataType *data, int elements, int id, int lineno )
{
    int j, k, rc=0;

    for(j=0,k=1; j < elements; j++,k+=2) {
        if ( data[j] != ((DataType)k + id) ) {
            printf("PE[%d] ERR @ line#%d data[%d] wanted %d != %ld\n",
                shmem_my_pe(), lineno, j, k+id, data[j] );

            for(rc=0,k=1; rc < elements; rc++,k++) {
                printf("%ld ",data[rc]);
            }
            rc = 1;
            break;
        }
        else
            if (Debug > 2) printf("%ld ",data[j]);
    }
    if (Debug > 2 || rc)
        printf("\n");
    fflush(stdout);
    return rc;
}

int
main(int argc, char **argv)
{
    int me, nProcs, workers, rc=0, l, j, c, Malloc=1;
    int nWords, nWords_start, prev_sz=0, ridx, loops, incWords;

    pgm = strrchr(argv[0],'/');
    if ( pgm )
        pgm++;
    else
        pgm = argv[0];

    shmem_init();
    me = shmem_my_pe();
    nProcs = shmem_n_pes();
    workers = nProcs - 1;

    if (nProcs <= 1) {
        fprintf(stderr, "ERR - Requires > 1 PEs\n");
        shmem_finalize();
        return 0;
    }

    while ((c = getopt (argc, argv, "hmrvdD")) != -1)
        switch (c)
        {
        case 'm':
            Malloc=1;
            break;
        case 'r':
            Malloc=0;
            break;
        case 'v':
        case 'd':
            Debug++;
            break;
        case 'D':
            Debug=2;
            break;
        case 'h':
        default:
            usage();
            break;
        }

    if (optind == argc)
        nWords = DFLT_NWORDS;
    else if ((nWords = getSize (argv[optind++])) <= 0)
        usage ();

    if (nWords % 2 != 0) {
        if (me == 0)
            fprintf(stderr," nWords(%d) not even?\n",nWords);
        shmem_finalize();
        exit(1);
    }
    nWords_start = nWords;

    if (optind == argc)
            loops = DFLT_LOOPS;
    else if ((loops = getSize (argv[optind++])) < 0)
        usage ();

    if (optind == argc)
        incWords = DFLT_INCR;
    else if ((incWords = getSize (argv[optind++])) < 0)
        usage ();

    if (loops > 1 ) {
        if (incWords > 0 && ((incWords % 2) != 0)) {
            if (me == 0)
                fprintf(stderr," incWords(%d) not even?\n",incWords);
            shmem_finalize();
            exit(1);
        }
    }

    if (Debug && me == 0)
        printf("%s: workers(%d) nWords(%d) loops(%d) nWords-incr-per-loop(%d)\n",
            pgm, workers, nWords, loops, incWords);

    for(l=0,ridx=0; l < loops; l++) {
        // reserve space for worker PEs to deposit data
        rc =  ((workers * nWords) * sizeof(DataType)) + prev_sz;
        if (Debug > 2)
            printf("alloc: results[%ld]\n",(rc/sizeof(DataType)));
        results = (DataType *)shmem_realloc(results,rc);
        if (!results)
        {
            perror ("Failed results memory allocation");
            shmem_global_exit(1);
        }
        prev_sz = rc;

        if (me==0 && Debug > 2) {
            int idx = ridx;
            printf("alloc: results[%ld] ridx %d psz %ld\n",
                (rc/sizeof(DataType)),ridx,(prev_sz/sizeof(DataType)));
            for(j=1; j < nProcs; j++) {
                printf("  PE[%d] results[%d...%d]\n",
                            j,idx,(idx+(nWords-1)));
                idx += nWords;
            }
        }

        rc = (2 * nWords) * sizeof(DataType);
        if (Debug > 2)
            printf("source %ld words\n",rc/sizeof(DataType));
        if (Malloc)
            source = (DataType *)shmem_malloc(rc);
        else
            source = (DataType *)shmem_realloc(source,rc);

        if (! source) {
            perror ("Failed source memory allocation");
            shmem_global_exit(1);
        }
        if (Debug > 3)
            printf("shmem_malloc() source %p (%d bytes)\n",(void*)source,rc);

        /* init source data */
        for(j=0; j < nWords*2; j++)
            source[j] = j+1;

        rc = nWords * sizeof(DataType);
        if (Debug > 2)
            printf("target %ld words\n",rc/sizeof(DataType));
        if (Malloc)
            target = (DataType *)shmem_malloc(rc);
        else
            target = (DataType *)shmem_realloc(target,rc);

        if ( ! target ) {
            perror ("Failed target memory allocation");
            shmem_global_exit(1);
        }
        memset(target, 0, rc);
        if (Debug > 3)
            printf("shmem_malloc() target %p (%d bytes)\n",(void*)target,rc);

        shmem_barrier_all();

        if (me == 0) {
            /* put nWords of DataType into target on PE's [1 to (nProcs-1)] */
            for(j=1; j < nProcs; j++)
                IPUT(target, source, 1, 2, nWords, j);
        }

        shmem_barrier_all();

        if (me != 0) {
            // Verify iput target data
            rc = target_data_good(target, nWords, 0, __LINE__);
            if (rc)
                shmem_global_exit(1);
            // add my PE to target data so PE0 will know I have seen the data.
            for(j=0; j < nWords; j++)
                target[j] += me;
        }

        shmem_barrier_all();

        if (me == 0) {
            // Pull data from PE1...(nProcs-1)
            // iget() target[0...nWords] -->
            //      results[0 ... nWords] == loop-0 PE1 results
            //      results[loop*PE1*nWords ... PE*nWords] == loop-0 PE1 results

            for(j=1; j < nProcs; j++) {
                if (Debug > 1)
                    printf("PE[0] iget(%d words PE[%d]) results[%d...%d]\n",
                                    nWords,j,ridx,(ridx+(nWords-1)));
                IGET(&results[ridx], target, 1, 1, nWords, j);
                rc = target_data_good( &results[ridx], nWords, j, __LINE__);
                if (rc)
                    shmem_global_exit(1);
                ridx += nWords;
            }
        }

        shmem_barrier_all();

        if (Malloc) {
            shmem_free(source);
            shmem_free(target);
        }

        nWords += incWords;

        if (Debug && me == 0 && loops > 1)
            printf("End loop %d: nWords %d\n", (l+1), nWords);

        shmem_barrier_all();
    }

    if (me == 0) {
        nWords = nWords_start;
        for(l=0,ridx=0; l < loops; l++) {
            for(j=1; j < nProcs; j++) {
                if (Debug > 1)
                    printf("  Validate: loop %d PE-%d ridx %d nWords %d\n",
                            l,j,ridx,nWords);
                rc = target_data_good( &results[ridx], nWords, j, __LINE__);
                if (rc)
                    shmem_global_exit(1);
                ridx += nWords;
            }
            nWords += incWords;
        }
    }

    if (Debug)
        printf("PE-%d exit(0)\n",me);

    shmem_finalize();

    return 0;
}
