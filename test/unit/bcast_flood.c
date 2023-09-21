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

/* Broadcast flood - bcast 100KB to all, default to using 2 pSync vars */

#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <getopt.h>
#include <string.h>
#include <assert.h>

#include <shmem.h>
#include "tests_sos/wtime.h"

static int atoi_scaled(char *s);
static void usage(char *pgm);

int Verbose=0;
int Serialize;

#define DFLT_LOOPS 600      // downsized for 'make check'
//#define DFLT_LOOPS 10000
#define N_ELEMENTS 25600    /*100 KB as ints */

int
main(int argc, char **argv)
{
    int i;
    int *target;
    int *source;
    int me, npes, elements=N_ELEMENTS, loops=DFLT_LOOPS;
    char *pgm;
    double start_time, time_taken;

    shmem_init();
    me = shmem_my_pe();
    npes = shmem_n_pes();

    if ((pgm=strrchr(argv[0],'/'))) {
        pgm++;
    } else {
        pgm = argv[0];
    }

    while ((i = getopt (argc, argv, "hve:l:p:s")) != EOF) {
        switch (i)
        {
          case 'v':
              Verbose++;
              break;
          case 'e':
              if ((elements = atoi_scaled(optarg)) <= 0) {
                  fprintf(stderr,"ERR: Bad elements count %d\n",elements);
                  shmem_finalize();
                  return 1;
              }
              break;
          case 'l':
              if ((loops = atoi_scaled(optarg)) <= 0) {
                  fprintf(stderr,"ERR: Bad loop count %d\n",loops);
                  shmem_finalize();
                  return 1;
              }
              break;
          case 's':
              Serialize++;
              break;
          case 'h':
              if (me == 0)
                  usage(pgm);
              return 0;
          default:
              if (me == 0) {
                  fprintf(stderr,"%s: unknown switch '-%c'?\n",pgm,i);
                  usage(pgm);
              }
              shmem_finalize();
              return 1;
        }
    }

    source = (int *) shmem_malloc( elements * sizeof(*source) );
    if (!source) {
        fprintf(stderr, "ERR - null source pointer\n");
        shmem_global_exit(1);
    }

    target = (int *) shmem_malloc( elements * sizeof(*target) );
    if (!target) {
        fprintf(stderr, "ERR - null target pointer\n");
        shmem_global_exit(1);
    }
    for (i = 0; i < elements; i += 1) {
        source[i] = i + 1;
        target[i] = -90;
    }

    if (me==0 && Verbose) {
        fprintf(stderr,"loops %d nElems %d\n",
                        loops,elements);
    }

    shmem_barrier_all();

    for(time_taken = 0.0, i = 0; i < loops; i++) {

        start_time = tests_sos_wtime();

        shmem_int_broadcast(SHMEM_TEAM_WORLD, target, source, elements, 0);

        if (Serialize) shmem_barrier_all();

        time_taken += (tests_sos_wtime() - start_time);

    }

    if(me == 0 && Verbose) {
        printf("%d loops of Broadcast32(%ld bytes) over %d PEs: %7.3f secs\n",
            loops, (elements*sizeof(*source)), npes, time_taken);
        elements = (elements * loops * sizeof(*source)) / (1024*1024);
        printf("  %7.5f secs per broadcast() @ %7.4f MB/sec\n",
               (time_taken/(double)loops), ((double)elements / time_taken) );
    }

    if (Verbose > 1)  fprintf(stderr,"[%d] pre B1\n",me);

    shmem_barrier_all();

    if (Verbose > 1)  fprintf(stderr,"[%d] post B1\n",me);

    shmem_free(target);
    shmem_free(source);

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
        "    -e ints        # of integers to broadcast\n"
        "    -v             be verbose, multiple 'v' more verbose\n"
        "    -h             this text.\n",
        pgm,DFLT_LOOPS);
}

