/*
 * (big puts) each PE puts N elements (1MB) to ((my_pe()+1) mod num_pes()).
 */

#include <stdio.h>
#include <stdlib.h>
#include <getopt.h>
#include <string.h>
#include <time.h>
#include <sys/time.h>

#include <shmem.h>

#define NUM_ELEMENTS 25600  // 1 MB as longs
//#define DFLT_LOOPS 10000  // reset when Portals4 can achieve this.
#define DFLT_LOOPS 100

int Verbose;
int Sync;
int Track;
int elements = NUM_ELEMENTS;
double sum_time, time_taken;

double pWrk[_SHMEM_REDUCE_MIN_WRKDATA_SIZE];
long pSync[_SHMEM_REDUCE_SYNC_SIZE];

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
        "usage: %s -{hvclst}\n"
        "  where: (big puts)\n"
        "    -v              be verbose, multiple 'v' more verbose\n"
        "    -e element-cnt (%d)  # of int sized elements to put\n"
        "    -l loops (%d)  loop count.\n"
        "    -s             synchronize: barrier after each shmem_put()\n"
        "    -t             track: output '.' for every 200 shmem_put()s\n",
        pgm,NUM_ELEMENTS,DFLT_LOOPS);
}


static inline double gettime_sec(void)
{
    struct timeval tv;
    gettimeofday(&tv, 0);
    return (double)((tv.tv_usec / 1000000.0) + tv.tv_sec);
}


int
main(int argc, char **argv)
{
    int loops=DFLT_LOOPS;
    char *pgm;
    int *Target;
    int *Source;
    int i, me, npes;
    int target_PE;
    long bytes;
    double start_time, *total_time;

    start_pes(0);
    me = _my_pe();
    npes = _num_pes();

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

    while ((i = getopt (argc, argv, "hve:l:st")) != EOF) {
        switch (i)
        {
          case 'v':
              Verbose++;
              break;
          case 'e':
              if ((elements = atoi_scaled(optarg)) <= 0) {
                  fprintf(stderr,"ERR: Bad elements count %d\n",elements);
                  return 1;
              }
              break;
          case 'l':
              if ((loops = atoi_scaled(optarg)) <= 0) {
                  fprintf(stderr,"ERR: Bad loop count %d\n",loops);
                  return 1;
              }
              break;
          case 's':
              Sync++;
              break;
          case 't':
              Track++;
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
              return 1;
        }
    }

    for(i=0; i < _SHMEM_REDUCE_SYNC_SIZE; i++)
        pSync[i] = _SHMEM_SYNC_VALUE;

    target_PE = (me+1) % npes;

    total_time = (double *) shmalloc( npes * sizeof(double) );
    if (!total_time) {
      fprintf(stderr,"ERR: bad total_time shmalloc(%ld)\n",
              (elements * sizeof(double)));
      return 1;
    }
    for(i=0; i < npes; i++)
        total_time[i] = -1.0;

    Source = (int *) shmalloc( elements * sizeof(*Source) );
    if (!Source) {
      fprintf(stderr,"ERR: bad Source shmalloc(%ld)\n",
              (elements * sizeof(*Target)));
      shfree(total_time);
      return 1;
    }

    Target = (int *) shmalloc( elements * sizeof(*Target) );
    if (!Target) {
      fprintf(stderr,"ERR: bad Target shmalloc(%ld)\n",
              (elements * sizeof(*Target)));
      shfree(Source);
      shfree(total_time);
      return 1;
    }

    for (i = 0; i < elements; i++) {
      Target[i] = -90;
      Source[i] = i + 1;
    }

    bytes = loops * sizeof(int) * elements;

    if (Verbose && me==0) {
      fprintf(stderr,
        "%s: INFO - %d loops, put %d (int) elements to PE+1 Max put ??\n",
              pgm, loops, elements);
    }
    shmem_barrier_all();

    for(i=0; i < loops; i++) {

		start_time = shmem_wtime();

		shmem_int_put(Target, Source, elements, target_PE);

		time_taken += (shmem_wtime() - start_time);

		if (me==0) {
		  if ( Track && i > 0 && ((i % 200) == 0))
		    fprintf(stderr,".%d",i);
		}
		if (Sync)
		    shmem_barrier_all();
    }

    // collect time per node.
    shmem_double_put( &total_time[me], &time_taken, 1, 0 );
    shmem_double_sum_to_all(&sum_time, &time_taken, 1, 0, 0, npes, pWrk, pSync);

    shmem_barrier_all();

    if ( Track && me == 0 ) fprintf(stderr,"\n");

    if(Verbose && me == 0) {
        double rate, comp_time;

        if (Verbose > 1)
            fprintf(stdout,"Individule PE times: (seconds)\n");
        for(i=0,comp_time=0.0; i < npes; i++) {
            comp_time += total_time[i];
            if (Verbose > 1)
                fprintf(stdout,"  PE[%d] %8.6f\n",i,total_time[i]);
        }

        sum_time /= (double)npes;
        comp_time /= (double)npes;
        if (sum_time != comp_time)
            printf("%s: computed_time %7.5f != sum_to_all_time %7.5f)\n",
                pgm, comp_time, sum_time );

        rate = ((double)bytes/(1024.0*1024.0)) / comp_time;
        printf("%s: shmem_int_put() %7.4f MB/sec (bytes %ld secs %7.4f)\n",
                pgm, rate, bytes, sum_time);
    }

    shfree(total_time);
    shfree(Target);
    shfree(Source);

    return 0;
}
