
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

	start_pes(0);
	me = _my_pe();
	npes = _num_pes();

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
                  return 1;
              }
              break;
          case 'v':
              Verbose++;
              break;
          case 'h':
              usage(pgm);
              return 0;
          default:
              if (me == 0) {
                  fprintf(stderr,"%s: unknown switch '-%c'?\n",pgm,l);
                  usage(pgm);
              }
              return 1;
        }
    }

    for(l=0; l < laps; l++) {

    	target = (long *) shmalloc(sizeof (*target));
        if (!target) {
            fprintf(stderr,"[%d] shmalloc() failed?\n",me);
            return 1;
        }

	    *target = me;
	    new_val = me;

	    shmem_barrier_all();

	    if (me & 1) {
	        swapped_val = shmem_long_swap (target, new_val, (me + 1) % npes);
	        if (Verbose > 1)
                printf("[%d] target %ld, swapped %ld\n",
                        me, *target, swapped_val);
	    }
	    shmem_barrier_all();
	    shfree (target);
	    if (Verbose == 1 && me == 0)  fprintf(stderr,".");
    }
	if (Verbose && me == 0)  fprintf(stderr,"\n");

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

