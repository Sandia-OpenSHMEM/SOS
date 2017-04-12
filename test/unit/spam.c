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

/* spam - shmem performance amalgamation
 *
 * one2many -     timed loops of [PE-0 put 2048 bytes to 1..(npes-1) PEs
 * many2one -     timed loops of [PE-0 get 2048 bytes to 1..(npes-1) PEs
 * all2all_get -  timed loops of get 2048 bytes all-2-all.
 * all2all_put -  timed loops of put 2048 bytes all-2-all.
 * neighbor_put - timed loops of put 2048 bytes to next neighbor.
 * neighbor_get - timed loops of get 2048 bytes to next neighbor.
 * bcast -        timed loops of broadcast 2048 bytes to all.
 * collect -      timed loops of collect 2048 bytes from all PEs to 0.
 * fcollect -     timed loops of fcollect 2048 bytes from all PEs to 0.
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <sys/time.h>
#include <getopt.h>
#include <string.h>
#include <assert.h>

#include <shmem.h>
#include <shmemx.h>

void one2many_put(int *dst, int *src, int Elems, int me, int npe, int laps);
void many2one_get(int *dst, int *src, int Elems, int me, int npe, int laps);
void all2all_get(int *dst, int *src, int Elems, int me, int npes, int laps);
void all2all_put(int *dst, int *src, int Elems, int me, int npes, int laps);
void neighbor_put(int *dst, int *src, int Elems, int me, int npes, int laps);
void neighbor_get(int *dst, int *src, int Elems, int me, int npes, int laps);
void bcast(int *dst, int *src, int Elems, int me, int npes, int laps);
void collect(int *dst, int *src, int Elems, int me, int npes, int laps);
void fcollect(int *dst, int *src, int Elems, int me, int npes, int laps);

static int atoi_scaled(char *s);
static void usage(char *pgm);

#ifndef HAVE_SHMEMX_WTIME
static double shmemx_wtime(void);
#endif

int Verbose=1;
int All2=0;
int Bcast=0;
int Collect=0;
int Many=0;
int Neighbor=0;

#define DFLT_LOOPS 20000

#define N_ELEMENTS 512    /* # ints */

int
main(int argc, char **argv)
{
    int i;
    int *target;
    int *source;
    int me, npes, elements=N_ELEMENTS, loops=DFLT_LOOPS;
    char *pgm;

    shmem_init();
    me = shmem_my_pe();
    npes = shmem_n_pes();

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

    /* lower-case switch enable only a specific test; otherwise run all tests */
    while ((i = getopt (argc, argv, "hvqe:l:abcmn")) != EOF) {
        switch (i)
        {
          case 'a':
              All2++;
              break;
          case 'b':
              Bcast++;
              break;
          case 'c':
              Collect++;
              break;
          case 'm':
              Many++;
              break;
          case 'n':
              Neighbor++;
              break;
          case 'q':
              Verbose=0;
              break;
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
          case 'h':
              if (me == 0)
                  usage(pgm);
              shmem_finalize();
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

    if (All2==0 && Bcast==0 && Collect==0 && Many==0 && Neighbor==0)
        All2 = Bcast = Collect = Many = Neighbor = 1;

    source = (int *) shmem_malloc( elements * sizeof(*source) );
    target = (int *) shmem_malloc( elements * sizeof(*target) );

    for (i = 0; i < elements; i += 1) {
        source[i] = i + 1;
        target[i] = -90;
    }

    shmem_barrier_all();

    if (Neighbor) {
        neighbor_put( target, source, elements, me, npes, loops );
        neighbor_get( target, source, elements, me, npes, loops );
    }

    if (All2) {
        all2all_put( target, source, elements, me, npes, loops );
        all2all_get( target, source, elements, me, npes, loops );
    }

    if (Many) {
        one2many_put( target, source, elements, me, npes, loops );
        many2one_get( target, source, elements, me, npes, loops );
    }

    if (Bcast) bcast( target, source, elements, me, npes, loops );

    if (Collect) {
        collect( NULL, source, elements, me, npes, loops );
        fcollect( NULL, source, elements, me, npes, loops );
    }

    shmem_barrier_all();

    shmem_free(target);
    shmem_free(source);

    shmem_finalize();

    return 0;
}

void
one2many_put(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, pe;
    double start_time, elapsed_time;
    long total_bytes = loops * elements * sizeof(*src) * (npes - 1);

    if (me == 0) {
        fprintf(stdout,"%s: %d loops of put(%ld bytes) to %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes-1);
        fflush(stdout);
    }
    shmem_barrier_all();

    if (me == 0) {
        start_time = shmemx_wtime();
        for(i = 0; i < loops; i++) {
            for(pe = 1; pe < npes; pe++)
                shmem_int_put(target, src, elements, pe);
        }
        elapsed_time = shmemx_wtime() - start_time;

        if (Verbose) {
            printf("%7.3f secs\n", elapsed_time);
            printf("  %7.5f usecs / put(), %ld Kbytes @ %7.4f MB/sec\n\n",
                   (elapsed_time/((double)loops*(npes-1)))*1000000.0,
                   (total_bytes/1024),
                   ((double)total_bytes/(1024.0*1024.0)) / elapsed_time );
        }
    }
    shmem_barrier_all();
}

void
many2one_get(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, pe;
    double start_time, elapsed_time;
    long total_bytes = loops * elements * sizeof(*src) * (npes - 1);

    if (me == 0) {
        fprintf(stdout,"%s: %d loops of get(%ld bytes) from %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes-1);
        fflush(stdout);
    }

    shmem_barrier_all();

    if (me == 0) {
        start_time = shmemx_wtime();
        for(i = 0; i < loops; i++) {
            for(pe = 1; pe < npes; pe++)
                shmem_int_get(target, src, elements, pe);
        }
        elapsed_time = shmemx_wtime() - start_time;

        if (Verbose) {
            printf("%7.3f secs\n", elapsed_time);
            printf("  %7.5f usecs / get(), %ld Kbytes @ %7.4f MB/sec\n\n",
                   (elapsed_time/((double)loops*(npes-1)))*1000000.0,
                   (total_bytes/1024),
                   ((double)total_bytes/(1024.0*1024.0)) / elapsed_time );
        }
    }
    shmem_barrier_all();
}

void
all2all_get(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, pe;
    double start_time, elapsed_time;
    long total_bytes = loops * elements * sizeof(*src) * npes;

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of get(%ld bytes) from all %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes);
        fflush(stdout);
    }
    shmem_barrier_all();

    start_time = shmemx_wtime();
    for(i = 0; i < loops; i++) {
        for(pe = 0; pe < npes; pe++)
            shmem_int_get(target, src, elements, pe);
    }
    elapsed_time = shmemx_wtime() - start_time;

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", elapsed_time);
        printf("  %7.5f usecs / get(), %ld Kbytes @ %7.4f MB/sec\n\n",
               (elapsed_time/((double)loops*npes))*1000000.0,
               (total_bytes/1024),
               ((double)total_bytes/(1024.0*1024.0)) / elapsed_time );
    }

    shmem_barrier_all();
}


void
all2all_put(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, pe;
    double start_time, elapsed_time;
    long total_bytes = loops * elements * sizeof(*src) * npes;

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of put(%ld bytes) to all %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes);
        fflush(stdout);
    }

    shmem_barrier_all();

    start_time = shmemx_wtime();
    for(i = 0; i < loops; i++) {
        for(pe = 0; pe < npes; pe++)
            shmem_int_put(target, src, elements, pe);
    }
    elapsed_time = shmemx_wtime() - start_time;

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", elapsed_time);
        printf("  %7.5f usecs / put(), %ld Kbytes @ %7.4f MB/sec\n\n",
               (elapsed_time/((double)loops*npes))*1000000.0,
               (total_bytes/1024),
               ((double)total_bytes/(1024.0*1024.0)) / elapsed_time );
    }

    shmem_barrier_all();
}

void
neighbor_put(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, neighbor_pe;
    double start_time, elapsed_time;
    long total_bytes = loops * elements * sizeof(*src);

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of put(%ld bytes) to neighbor, %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes);
        fflush(stdout);
    }

    shmem_barrier_all();

    neighbor_pe = (me + 1) % npes;

    start_time = shmemx_wtime();
    for(i = 0; i < loops; i++)
        shmem_int_put(target, src, elements, neighbor_pe);
    elapsed_time = shmemx_wtime() - start_time;

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", elapsed_time);
        printf("  %7.5f usecs / put(), %ld Kbytes @ %7.4f MB/sec\n\n",
               (elapsed_time/((double)loops*npes))*1000000.0,
               (total_bytes/1024),
               ((double)total_bytes/(1024.0*1024.0)) / elapsed_time );
    }
    shmem_barrier_all();
}

void
neighbor_get(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, neighbor_pe;
    double start_time, elapsed_time;
    long total_bytes = loops * elements * sizeof(*src);

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of get(%ld bytes) from neighbor, %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes);
        fflush(stdout);
    }

    shmem_barrier_all();

    neighbor_pe = (me + 1) % npes;

    start_time = shmemx_wtime();
    for(i = 0; i < loops; i++)
        shmem_int_get(target, src, elements, neighbor_pe);
    elapsed_time = shmemx_wtime() - start_time;

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", elapsed_time);
        printf("  %7.5f usecs / get(), %ld Kbytes @ %7.4f MB/sec\n\n",
               (elapsed_time/((double)loops*npes))*1000000.0,
               (total_bytes/1024),
               ((double)total_bytes/(1024.0*1024.0)) / elapsed_time );
    }
    shmem_barrier_all();
}


void
bcast(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i;
    double start_time, elapsed_time;
    long *ps, *pSync, *pSync1;
    long total_bytes = loops * elements * sizeof(*src);

    pSync = (long*)shmem_malloc( 2 * sizeof(long) * SHMEM_BCAST_SYNC_SIZE );
    pSync1 = &pSync[SHMEM_BCAST_SYNC_SIZE];
    for (i = 0; i < SHMEM_BCAST_SYNC_SIZE; i++) {
        pSync[i] = pSync1[i] = SHMEM_SYNC_VALUE;
    }

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of broadcast32(%ld bytes) over %d PEs: ",
                __FUNCTION__,loops,(elements*sizeof(*src)),npes);
        fflush(stdout);
    }

    shmem_barrier_all();

    start_time = shmemx_wtime();
    for(i = 0; i < loops; i++) {
        ps = (i & 1) ? pSync1 : pSync;
        shmem_broadcast32( target, src, elements, 0, 0, 0, npes, ps );
    }
    elapsed_time = shmemx_wtime() - start_time;

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", elapsed_time);
        printf("  %7.5f usecs / broadcast32(), %ld Kbytes @ %7.4f MB/sec\n\n",
               (elapsed_time/((double)loops*npes))*1000000.0,
               (total_bytes/1024),
               ((double)total_bytes/(1024.0*1024.0)) / elapsed_time );
    }
    shmem_barrier_all();
    shmem_free( pSync );
}


void
collect(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i;
    double start_time, elapsed_time;
    long total_bytes = loops * elements * sizeof(*src);
    long *ps, *pSync, *pSync1;

    pSync = (long*) shmem_malloc( 2 * sizeof(long) * SHMEM_COLLECT_SYNC_SIZE );
    pSync1 = &pSync[SHMEM_COLLECT_SYNC_SIZE];
    for (i = 0; i < SHMEM_COLLECT_SYNC_SIZE; i++) {
        pSync[i] = pSync1[i] = SHMEM_SYNC_VALUE;
    }
    target = (int *) shmem_malloc( elements * sizeof(*target) * npes );

    if (me==0 && Verbose) {
        fprintf(stdout,"%s: %d loops of collect32(%ld bytes) over %d PEs: ",
                __FUNCTION__,loops,(elements*sizeof(*src)),npes);
        fflush(stdout);
    }

    shmem_barrier_all();

    start_time = shmemx_wtime();
    for(i = 0; i < loops; i++) {
        ps = (i & 1) ? pSync1 : pSync;
        shmem_collect32( target, src, elements, 0, 0, npes, ps );
    }
    elapsed_time = shmemx_wtime() - start_time;

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", elapsed_time);
        printf("  %7.5f usecs / collect32(), %ld Kbytes @ %7.4f MB/sec\n\n",
               (elapsed_time/((double)loops*npes))*1000000.0,
               (total_bytes/1024),
               ((double)total_bytes/(1024.0*1024.0)) / elapsed_time );
    }
    shmem_barrier_all();
    shmem_free(target);
    shmem_free( pSync );
    shmem_barrier_all();
}



void
fcollect(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i;
    double start_time, elapsed_time;
    long total_bytes = loops * elements * sizeof(*src);
    long *ps, *pSync, *pSync1;

    pSync = (long*) shmem_malloc( 2 * sizeof(long) * SHMEM_COLLECT_SYNC_SIZE );
    pSync1 = &pSync[SHMEM_COLLECT_SYNC_SIZE];
    for (i = 0; i < SHMEM_COLLECT_SYNC_SIZE; i++) {
        pSync[i] = pSync1[i] = SHMEM_SYNC_VALUE;
    }
    target = (int *) shmem_malloc( elements * sizeof(*target) * npes );

    if (me==0 && Verbose) {
        fprintf(stdout,"%s: %d loops of fcollect32(%ld bytes) over %d PEs: ",
                __FUNCTION__,loops,(elements*sizeof(*src)),npes);
        fflush(stdout);
    }

    shmem_barrier_all();

    start_time = shmemx_wtime();
    for(i = 0; i < loops; i++) {
        ps = &pSync[(i&1)];
        shmem_fcollect32( target, src, elements, 0, 0, npes, ps );
    }
    elapsed_time = shmemx_wtime() - start_time;

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", elapsed_time);
        printf("  %7.5f usecs / fcollect32(), %ld Kbytes @ %7.4f MB/sec\n\n",
               (elapsed_time/((double)loops*npes))*1000000.0,
               (total_bytes/1024),
               ((double)total_bytes/(1024.0*1024.0)) / elapsed_time );
    }
    shmem_barrier_all();
    shmem_free(target);
    shmem_free( pSync );
    shmem_barrier_all();
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
        "    -e ints  (%d)  # of integers to Tx\n"
        "    -a             enable all-2-all tests\n"
        "    -b             enable broadcast tests\n"
        "    -c             enable collective tests\n"
        "    -m             enable Many-2-one, one-2-many tests\n"
        "    -n             enable put/get to neighbor PEtests\n"
        "    -v             be verbose, multiple 'v' more verbose\n"
        "    -q             be quite\n"
        "    -h             this text.\n",
        pgm,DFLT_LOOPS, N_ELEMENTS);
}


#ifndef HAVE_SHMEMX_WTIME

double
shmemx_wtime(void)
{
    double wtime;
    struct timeval tv;

    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
    return wtime;
}

#endif /* HAVE_SHMEMX_WTIME */
