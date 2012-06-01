/* spam - shmem performance amalgamation
 *
 * one2many -     timed loops of [PE-0 put 512 bytes to 1..(npes-1) PEs
 * many2one -     timed loops of [PE-0 get 512 bytes to 1..(npes-1) PEs
 * all2all_get -  timed loops of get 512 bytes all-2-all.
 * all2all_put -  timed loops of put 512 bytes all-2-all.
 * neighbor_put - timed loops of put 512 bytes to next neighbor.
 * neighbor_get - timed loops of get 512 bytes to next neighbor.
 * bcast -        timed loops of broadcast 512 bytes to all.
 * collect -      timed loops of collect 512 bytes from all PEs to 0.
 * fcollect -     timed loops of fcollect 512 bytes from all PEs to 0.
*/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <getopt.h>
#include <string.h>
#include <assert.h>

#include <shmem.h>

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

int Verbose=1;
int All2=1;
int Bcast=1;
int Collect=1;
int Many=1;
int Neighbor=1;

#define DFLT_LOOPS 20000

#define N_ELEMENTS 128    /* 512 bytes as ints */

int
main(int argc, char **argv)
{
    int i;
	int *target;
	int *source;
	int me, npes, elements=N_ELEMENTS, loops=DFLT_LOOPS;
    char *pgm;

	start_pes(0);
	me = _my_pe();
	npes = _num_pes();

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

    /* captialized switches exclude all test except for one.
     * lower-case switch enable specific test.
     * -A == only the All2All tests.
     * -b == enable broadcast test.
     */
    while ((i = getopt (argc, argv, "hve:l:ABCMNabcmn")) != EOF) {
        switch (i)
        {
          case 'A': // only All2all Tx/Rx tests
              Bcast = Collect = Many = Neighbor = 0;
              break;
          case 'B': // only braodcast
              All2 = Collect = Many = Neighbor = 0;
              break;
          case 'C': // only collect/fcollect
              All2 = Bcast = Many = Neighbor = 0;
              break;
          case 'M': // only Many2one + one2Many
              All2 = Bcast = Collect = Neighbor = 0;
              break;
          case 'N': // only Neighbor Tx/Rx tests
              All2 = Bcast = Collect = Many = 0;
              break;
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

	source = (int *) shmalloc( elements * sizeof(*source) );

	target = (int *) shmalloc( elements * sizeof(*target) );

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

	shfree(target);
	shfree(source);

	return 0;
}

void
one2many_put(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, pe;
	double start_time, time_taken=0.0;
    long total_bytes = loops * elements * sizeof(*src) * (npes - 1);

	if (me == 0) {
        fprintf(stdout,"%s: %d loops of put(%ld bytes) to %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes-1);
        fflush(stdout);
    }
	shmem_barrier_all();

	if (me == 0) {
	    for(i = 0; i < loops; i++) {
	        for(pe = 1; pe < npes; pe++) {
	            start_time = shmem_wtime();
	            shmem_int_put(target, src, elements, pe);
	            time_taken += (shmem_wtime() - start_time);
            }
	    }

	    if (Verbose) {
            printf("%7.3f secs\n", time_taken);
            printf("  %7.5f usecs / put(), %ld Kbytes @ %7.4f MB/sec\n\n",
                (time_taken/((double)loops*(npes-1)))*1000000.0,
                (total_bytes/1024),
                ((double)total_bytes/(1024.0*1024.0)) / time_taken );
        }
    }
	shmem_barrier_all();
}

void
many2one_get(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, pe;
	double start_time, time_taken=0.0;
    long total_bytes = loops * elements * sizeof(*src) * (npes - 1);

	if (me == 0) {
        fprintf(stdout,"%s: %d loops of get(%ld bytes) from %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes-1);
        fflush(stdout);
    }

	shmem_barrier_all();

	if (me == 0) {
	    for(i = 0; i < loops; i++) {
	        for(pe = 1; pe < npes; pe++) {
	            start_time = shmem_wtime();
	            shmem_int_get(target, src, elements, pe);
	            time_taken += (shmem_wtime() - start_time);
            }
	    }

	    if (Verbose) {
            printf("%7.3f secs\n", time_taken);
            printf("  %7.5f usecs / get(), %ld Kbytes @ %7.4f MB/sec\n\n",
                (time_taken/((double)loops*(npes-1)))*1000000.0,
                (total_bytes/1024),
                ((double)total_bytes/(1024.0*1024.0)) / time_taken );
        }
    }
	shmem_barrier_all();
}

void
all2all_get(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, pe;
	double start_time, time_taken=0.0;
    long total_bytes = loops * elements * sizeof(*src) * npes;

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of get(%ld bytes) from all %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes);
        fflush(stdout);
    }
	shmem_barrier_all();

    for(i = 0; i < loops; i++) {
        for(pe = 0; pe < npes; pe++) {
            start_time = shmem_wtime();
            shmem_int_get(target, src, elements, pe);
            time_taken += (shmem_wtime() - start_time);
           }
    }

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", time_taken);
        printf("  %7.5f usecs / get(), %ld Kbytes @ %7.4f MB/sec\n\n",
                (time_taken/((double)loops*npes))*1000000.0,
                (total_bytes/1024),
                ((double)total_bytes/(1024.0*1024.0)) / time_taken );
    }

	shmem_barrier_all();
}


void
all2all_put(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, pe;
	double start_time, time_taken=0.0;
    long total_bytes = loops * elements * sizeof(*src) * npes;

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of put(%ld bytes) to all %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes);
        fflush(stdout);
    }

	shmem_barrier_all();

    for(i = 0; i < loops; i++) {
        for(pe = 0; pe < npes; pe++) {
            start_time = shmem_wtime();
            shmem_int_put(target, src, elements, pe);
            time_taken += (shmem_wtime() - start_time);
           }
    }

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", time_taken);
        printf("  %7.5f usecs / put(), %ld Kbytes @ %7.4f MB/sec\n\n",
                (time_taken/((double)loops*npes))*1000000.0,
                (total_bytes/1024),
                ((double)total_bytes/(1024.0*1024.0)) / time_taken );
    }

	shmem_barrier_all();
}

void
neighbor_put(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, neighbor_pe;
	double start_time, time_taken=0.0;
    long total_bytes = loops * elements * sizeof(*src);

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of put(%ld bytes) to neighbor, %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes);
        fflush(stdout);
    }

	shmem_barrier_all();

    neighbor_pe = (me + 1) % npes;

    for(i = 0; i < loops; i++) {
        start_time = shmem_wtime();
        shmem_int_put(target, src, elements, neighbor_pe);
        time_taken += (shmem_wtime() - start_time);
    }

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", time_taken);
        printf("  %7.5f usecs / put(), %ld Kbytes @ %7.4f MB/sec\n\n",
                (time_taken/((double)loops*npes))*1000000.0,
                (total_bytes/1024),
                ((double)total_bytes/(1024.0*1024.0)) / time_taken );
    }
	shmem_barrier_all();
}

void
neighbor_get(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i, neighbor_pe;
	double start_time, time_taken=0.0;
    long total_bytes = loops * elements * sizeof(*src);

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of get(%ld bytes) to neighbor, %d PEs: ",
                __FUNCTION__, loops, (elements*sizeof(*src)), npes);
        fflush(stdout);
    }

	shmem_barrier_all();

    neighbor_pe = (me + 1) % npes;

    for(i = 0; i < loops; i++) {
        start_time = shmem_wtime();
        shmem_int_get(target, src, elements, neighbor_pe);
        time_taken += (shmem_wtime() - start_time);
    }

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", time_taken);
        printf("  %7.5f usecs / get(), %ld Kbytes @ %7.4f MB/sec\n\n",
                (time_taken/((double)loops*npes))*1000000.0,
                (total_bytes/1024),
                ((double)total_bytes/(1024.0*1024.0)) / time_taken );
    }
	shmem_barrier_all();
}


void
bcast(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i;
	double start_time, time_taken=0.0;
    long *ps, *pSync, *pSync1;
    long total_bytes = loops * elements * sizeof(*src);

	pSync = (long*)shmalloc( 2 * sizeof(long) * _SHMEM_BCAST_SYNC_SIZE );
	pSync1 = &pSync[_SHMEM_BCAST_SYNC_SIZE];
    for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i++) {
        pSync[i] = pSync1[i] = _SHMEM_SYNC_VALUE;
    }

    if (me==0 && Verbose) {
        fprintf(stdout, "%s: %d loops of broadcast32(%ld bytes) over %d PEs: ",
                __FUNCTION__,loops,(elements*sizeof(*src)),npes);
        fflush(stdout);
    }

	shmem_barrier_all();

    for(i = 0; i < loops; i++) {
        ps = (i & 1) ? pSync1 : pSync;
        start_time = shmem_wtime();
        shmem_broadcast32( target, src, elements, 0, 0, 0, npes, ps );
        time_taken += (shmem_wtime() - start_time);
    }

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", time_taken);
        printf("  %7.5f usecs / broadcast32(), %ld Kbytes @ %7.4f MB/sec\n\n",
                (time_taken/((double)loops*npes))*1000000.0,
                (total_bytes/1024),
                ((double)total_bytes/(1024.0*1024.0)) / time_taken );
    }
	shmem_barrier_all();
    shfree( pSync );
}


void
collect(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i;
	double start_time, time_taken=0.0;
    long total_bytes = loops * elements * sizeof(*src);
    long *ps, *pSync, *pSync1;

	pSync = (long*) shmalloc( 2 * sizeof(long) * _SHMEM_COLLECT_SYNC_SIZE );
	pSync1 = &pSync[_SHMEM_COLLECT_SYNC_SIZE];
    for (i = 0; i < _SHMEM_COLLECT_SYNC_SIZE; i++) {
        pSync[i] = pSync1[i] = _SHMEM_SYNC_VALUE;
    }
	target = (int *) shmalloc( elements * sizeof(*target) * npes );

    if (me==0 && Verbose) {
        fprintf(stdout,"%s: %d loops of collect32(%ld bytes) over %d PEs: ",
                __FUNCTION__,loops,(elements*sizeof(*src)),npes);
        fflush(stdout);
    }

	shmem_barrier_all();

    for(i = 0; i < loops; i++) {
        ps = (i & 1) ? pSync1 : pSync;
        start_time = shmem_wtime();
        shmem_collect32( target, src, elements, 0, 0, npes, ps );
        time_taken += (shmem_wtime() - start_time);
    }

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", time_taken);
        printf("  %7.5f usecs / collect32(), %ld Kbytes @ %7.4f MB/sec\n\n",
                (time_taken/((double)loops*npes))*1000000.0,
                (total_bytes/1024),
                ((double)total_bytes/(1024.0*1024.0)) / time_taken );
    }
	shmem_barrier_all();
	shfree(target);
    shfree( pSync );
	shmem_barrier_all();
}

  

void
fcollect(int *target, int *src, int elements, int me, int npes, int loops)
{
    int i;
	double start_time, time_taken=0.0;
    long total_bytes = loops * elements * sizeof(*src);
    long *ps, *pSync, *pSync1;

	pSync = (long*) shmalloc( 2 * sizeof(long) * _SHMEM_COLLECT_SYNC_SIZE );
	pSync1 = &pSync[_SHMEM_COLLECT_SYNC_SIZE];
    for (i = 0; i < _SHMEM_COLLECT_SYNC_SIZE; i++) {
        pSync[i] = pSync1[i] = _SHMEM_SYNC_VALUE;
    }
	target = (int *) shmalloc( elements * sizeof(*target) * npes );

    if (me==0 && Verbose) {
        fprintf(stdout,"%s: %d loops of fcollect32(%ld bytes) over %d PEs: ",
                __FUNCTION__,loops,(elements*sizeof(*src)),npes);
        fflush(stdout);
    }

	shmem_barrier_all();

    for(i = 0; i < loops; i++) {
        ps = &pSync[(i&1)];
        start_time = shmem_wtime();
        shmem_fcollect32( target, src, elements, 0, 0, npes, ps );
        time_taken += (shmem_wtime() - start_time);
    }

    if (me==0 && Verbose) {
        printf("%7.3f secs\n", time_taken);
        printf("  %7.5f usecs / fcollect32(), %ld Kbytes @ %7.4f MB/sec\n\n",
                (time_taken/((double)loops*npes))*1000000.0,
                (total_bytes/1024),
                ((double)total_bytes/(1024.0*1024.0)) / time_taken );
    }
	shmem_barrier_all();
	shfree(target);
    shfree( pSync );
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
        "    -e ints  (%d)  # of integers to operate on\n"
        "    -A             only the all-2-all tests\n"
        "    -B             only the broadcast tests\n"
        "    -C             only the collect/fcollect tests\n"
        "    -M             only the Many-2-one + one-2-Many tests\n"
        "    -N             only the put/get to/from neighbor PE tests\n"
        "    -a             enable all-2-all tests\n"
        "    -b             enable broadcast tests\n"
        "    -c             enable collective tests\n"
        "    -m             enable Many-2-one, one-2-many tests\n"
        "    -n             enable put/get to neighbor PEtests\n"
        "    -v             be verbose, multiple 'v' more verbose\n"
        "    -h             this text.\n",
        pgm,DFLT_LOOPS, N_ELEMENTS);
}

