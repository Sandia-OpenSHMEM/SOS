/* :vim:sw=4:ts=4: */
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
 *  usage: shmemalign [-v]
 *    where: -v == Verbose display
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <shmem.h>

#define DFLT_NWORDS 257
#define DFLT_INCR 31
#define DFLT_LOOPS 1

#define DataType long

static DataType *target;
static int target_sz;

static char *pgm;
static int Verbose = 0;

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
            "  -v == Verbose output\n"
            "  [nWords] # of longs to shmem_malloc()\n"
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
    int me, c, l, j;
    int nWords, loops, incWords;

    pgm = strrchr(argv[0],'/');
    if ( pgm )
        pgm++;
    else
        pgm = argv[0];

    shmem_init();
    me = shmem_my_pe();

    while ((c = getopt (argc, argv, "hpv")) != -1)
        switch (c)
        {
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

    if (Verbose && me == 0)
        fprintf (stderr, "nWords(%d) loops(%d) incWords(%d)]\n",
                 nWords, loops, incWords);

    for(l=0; l < loops; l++)
    {
        /* align 2**2 ... 2**23; 24 exceeds symetric heap max */
        for(j=0,c=2; j < 23; j++,c<<=1)
        {
            target_sz = nWords * sizeof(DataType);
            if (!(target = (DataType *)shmem_align(c,target_sz))) {
                perror ("Failed target memory allocation");
                exit (1);
            }

            if ( (unsigned long)target & (c-1) ) {
                    fprintf(stdout,"PE%d Unaligned? ",me);
                    fflush(stdout);
                    fprintf(stdout,"align[%#09x]target %p\n",
                                        c, (void*)target);
                    shmem_global_exit(1);
            }
            else if (Verbose > 1 && me == 0)
                    fprintf(stdout,"align[%#09x]target %p\n",
                                        c, (void*)target);
            shmem_barrier_all();
            shmem_free(target);
        }
        nWords += incWords;
        if (Verbose && me == 0)
            fprintf(stdout,"Fini loop %d\n",(l+1));
    }

    shmem_finalize();

    return 0;
}
