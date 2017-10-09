/* :vim:sw=4:ts=4:expandtab: */
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
 *  usage: shrealloc [-p] [nWords] [loops] [incWords-per-loop]
 *    where: -p == power-of-two allocation size bump per loop
 *      [nWords] # of longs to shmem_realloc()\n"
 *      [loops(1)]  # of loops\n"
 *      [incWords(2)] nWords += incWords per loop\n");
 * Loop:
 *  PE* shmem_realloc(nWords)
 *   set *DataType = 1
 *  PE* shmem_realloc(nWords)
 *   set *DataType = 2
 *  PE* shmem_realloc(nWords)
 *   set *DataType = 3
 *
 *  for(1...3) allocated ranges
 *    verify
 * end-loop
 * shmem_free(3 allocations)
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <shmem.h>

#define DFLT_NWORDS 32
#define DFLT_INCR 1025
#define DFLT_LOOPS 50
#define DFLT_PLOOPS 21  // reduced to stay under shmem_malloc() stack limit.

#define DataType int

static DataType *source;
static DataType *target;
static DataType *result;

static int source_sz;
static int target_sz;
static int result_sz;

static int prev_source_idx = 0;
static int prev_target_idx = 0;
static int prev_result_idx = 0;

static char *pgm;

void usage (void);
int getSize (char *);

void
usage (void)
{
    if (shmem_my_pe() == 0 ) {
        fprintf (stderr,
            "Usage: %s [-p]  [nWords(%d)] [loops(%d)] [incWords(%d)]\n",
            pgm, DFLT_NWORDS, DFLT_LOOPS, DFLT_INCR);
        fprintf (stderr,
            "  -p  == (2**0 ... 2**22) shmem_realloc(), other args ignored\n"
            "  -v == verbose output\n"
            "  [nWords] # of longs to shmem_realloc()\n"
            "  [loops]  # of loops\n"
            "  [incWords] nWords += incWords per loop\n");
    }
    exit (1);
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
main(int argc, char **argv)
{
    int me, nProcs, c, l;
    int nWords, loops, incWords;
    int Verbose = 0, power2 = 0, modulo = 5;
    DataType *dp;

    pgm = strrchr(argv[0],'/');
    if ( pgm )
        pgm++;
    else
        pgm = argv[0];

    shmem_init();
    me = shmem_my_pe();
    nProcs = shmem_n_pes();

    if (nProcs <= 1) {
        fprintf(stderr, "ERR - Requires > 1 PEs\n");
        shmem_finalize();
        return 0;
    }

    while ((c = getopt (argc, argv, "hpv")) != -1)
        switch (c)
        {
        case 'p':
            power2++;
            break;
        case 'v':
            Verbose++;
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

    if (optind == argc)
            loops = DFLT_LOOPS;
    else if ((loops = getSize (argv[optind++])) < 0)
        usage ();

    if (optind == argc)
        incWords = DFLT_INCR;
    else if ((incWords = getSize (argv[optind++])) < 0)
        usage ();

    if (power2) {
        nWords = 1;
        modulo = 1;
        loops = DFLT_PLOOPS;
    }

    if (Verbose && me == 0) {
        if (power2) {
            printf("%s: nWords(1) << 1 per loop(%d).\n", pgm, DFLT_PLOOPS);
        }
        else
            printf("%s: nWords(%d) loops(%d) nWords-incr-per-loop(%d)\n",
                pgm, nWords, loops, incWords);
    }

    for(l=0; l < loops; l++)
    {
        result_sz = (nProcs-1) * (nWords * sizeof(DataType));
        result = (DataType *)shmem_realloc(result,result_sz);
        if (! result)
        {
            perror ("Failed result memory allocation");
            exit (1);
        }
        if (prev_result_idx == 0)
            for(dp=result; dp < &result[(result_sz/sizeof(DataType))];)
                *dp++ = 1;
        else
            for(dp=&result[prev_result_idx];
                dp < &result[(result_sz/sizeof(DataType))];) *dp++ = 1;
        prev_result_idx = result_sz / sizeof(DataType);

        target_sz = nWords * sizeof(DataType);
        if (!(target = (DataType *)shmem_realloc(target,target_sz)))
        {
            perror ("Failed target memory allocation");
            exit (1);
        }
        if (prev_target_idx == 0)
            for(dp=target; dp < &target[(target_sz / sizeof(DataType))];)
             *dp++ = 2;
        else
            for(dp=&target[prev_target_idx];
                dp < &target[(target_sz/sizeof(DataType))];) *dp++ = 2;
        prev_target_idx = target_sz / sizeof(DataType);

        source_sz = 2 * nWords * sizeof(DataType);
        if (!(source = (DataType *)shmem_realloc(source,source_sz)))
        {
            perror ("Failed source memory allocation");
            exit (1);
        }
        if (prev_source_idx == 0)
            for(dp=source; dp < &source[(source_sz / sizeof(DataType))];)
                *dp++ = 3;
        else
            for(dp=&source[prev_source_idx];
                dp < &source[(source_sz/sizeof(DataType))];) *dp++ = 3;
        prev_source_idx = source_sz / sizeof(DataType);

        for(dp=source; dp < &source[(source_sz / sizeof(DataType))]; dp++)
            if (*dp != 3 ) {
                printf("source not consistent @ 3?\n");
                break;
            }

        for(dp=target; dp < &target[(target_sz / sizeof(DataType))]; dp++)
            if (*dp != 2 ) {
                printf("target not consistent @ 2?\n");
                break;
            }

        for(dp=result; dp < &result[(result_sz / sizeof(DataType))]; dp++)
            if (*dp != 1 ) {
                printf("result not consistent @ 1?\n");
                break;
            }

        if (loops > 1) {
            if (Verbose && me == 0) {
                if (l == 0 || (l % modulo == 0))
                    printf("End loop %3d nWords(%d)\n",(l+1),nWords);
            }
            if (power2)
                nWords <<= 1;
            else
                nWords += incWords; // watch for double inc.
        }
    }

    shmem_free(source);
    shmem_free(target);
    shmem_free(result);

    shmem_finalize();

    return 0;
}
