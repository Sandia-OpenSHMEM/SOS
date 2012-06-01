/* Broadcast flood - bcast 100KB to all, default to using 2 pSync vars */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <getopt.h>
#include <string.h>
#include <assert.h>

#include <shmem.h>

static int atoi_scaled(char *s);
static void usage(char *pgm);

int Verbose=0;
int Serialize;

long *pSync;

#define DFLT_LOOPS 600      // downsized for 'make check'
//#define DFLT_LOOPS 10000
#define N_ELEMENTS 25600    /*100 KB as ints */

int
main(int argc, char **argv)
{
	int i,ps,ps_cnt=2;
	int *target;
	int *source;
	int me, npes, elements=N_ELEMENTS, loops=DFLT_LOOPS;
    char *pgm;
	double start_time, time_taken;

	start_pes(0);
	me = _my_pe();
	npes = _num_pes();

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

    while ((i = getopt (argc, argv, "hve:l:p:s")) != EOF) {
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
          case 'p':
              if ((ps_cnt = atoi_scaled(optarg)) <= 0) {
                  fprintf(stderr,"ERR: Bad pSync[] elements %d\n",loops);
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
              return 1;
        }
    }

	ps_cnt *= _SHMEM_BCAST_SYNC_SIZE;
	pSync = shmalloc( ps_cnt * sizeof(long) );

	for (i = 0; i < ps_cnt; i++)
	  pSync[i] = _SHMEM_SYNC_VALUE;

	source = (int *) shmalloc( elements * sizeof(*source) );

	target = (int *) shmalloc( elements * sizeof(*target) );
	for (i = 0; i < elements; i += 1) {
	    source[i] = i + 1;
	    target[i] = -90;
	}

    if (me==0 && Verbose)
        fprintf(stderr,"ps_cnt %d loops %d nElems %d\n",
                        ps_cnt,loops,elements);

	shmem_barrier_all();

	for(time_taken = 0.0, ps = i = 0; i < loops; i++) {

	    start_time = shmem_wtime();

	    shmem_broadcast32(target, source, elements, 0, 0, 0, npes, &pSync[ps]);

        if (Serialize) shmem_barrier_all();

	    time_taken += (shmem_wtime() - start_time);

        if (ps_cnt > 1 ) {
	        ps += _SHMEM_BCAST_SYNC_SIZE;
	        if ( ps >= ps_cnt ) ps = 0;
        }
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

	shfree(pSync);
	shfree(target);
	shfree(source);

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
        "    -p cnt         # of pSync[] elements\n"
        "    -v             be verbose, multiple 'v' more verbose\n"
        "    -h             this text.\n",
        pgm,DFLT_LOOPS);
}

