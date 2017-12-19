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
 * Neighbor swap:
 * swap between odd numbered PEs and their right neighbor (modulo num_pes)
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

#include <shmem.h>

static int atoi_scaled(char *s);
static void usage(char *pgm);

int Verbose;

#define DFLT_LAPS 50


int
main(int argc, char *argv[])
{
    char *pgm;
    int l, laps = DFLT_LAPS;
    long *target;
    int me, npes;
    long swapped_val, new_val;

    shmem_init();
    me = shmem_my_pe();
    npes = shmem_n_pes();

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

    while ((l = getopt (argc, argv, "l:vh")) != EOF) {
        switch (l)
        {
          case 'l':
              if ((laps = atoi_scaled(optarg)) <= 0) {
                  fprintf(stderr,"ERR: Bad loop count %d\n",laps);
                  shmem_finalize();
                  return 1;
              }
              break;
          case 'v':
              Verbose++;
              break;
          case 'h':
              usage(pgm);
              shmem_finalize();
              return 0;
          default:
              if (me == 0) {
                  fprintf(stderr,"%s: unknown switch '-%c'?\n",pgm,l);
                  usage(pgm);
              }
              shmem_finalize();
              return 1;
        }
    }

    for (l=0; l < laps; l++) {
        target = (long *) shmem_malloc(sizeof (*target));
        if (!target) {
            fprintf(stderr,"[%d] shmem_malloc() failed?\n",me);
            shmem_global_exit(1);
        }

        *target = me;
        new_val = me;

        shmem_barrier_all();

        if (me & 1) {
            swapped_val = shmem_long_atomic_swap(target, new_val, (me + 1) % npes);
            if (Verbose > 1)
                printf("[%d] target %ld, swapped %ld\n",
                       me, *target, swapped_val);
        }
        shmem_barrier_all();
        shmem_free (target);
        if (Verbose == 1 && me == 0)  fprintf(stderr,".");
    }
    if (Verbose && me == 0)  fprintf(stderr,"\n");

    shmem_finalize();

    return 0;
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

static void
usage(char *pgm)
{
    fprintf(stderr,
        "usage: %s -{lhv}\n"
        "  where:\n"
        "    -l loops (%d)  loop count.\n"
        "    -v             be verbose, multiple 'v' more verbose\n"
        "    -h             this text.\n",
        pgm,DFLT_LAPS);
}

