/*
 * shmem_collect test cat {-v -l loops} {nLongs(12)} {loopIncr(0)}
 *
 * For yod/mpirun -c/np X
 */
#include <shmem.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

#define AMEG (1024UL*1024UL)
#define AGIG (1024UL*1024UL*1024UL)

#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#endif

#define Rfprintf if (_my_pe() == 0) fprintf
#define Rprintf if (_my_pe() == 0)  printf
#define RVfprintf if (Verbose && _my_pe() == 0) fprintf
#define RVprintf if (Verbose && _my_pe() == 0)  printf
#define Vprintf if (Verbose)  printf
#define Vfprintf if (Verbose)  fprintf

#define RDprintf if (Verbose && _my_pe() == 0)  printf
#define RDfprintf if (Verbose && _my_pe() == 0) fprintf

/* option flags */
int Verbose;

#define DFLT_NWORDS 16
#define DFLT_INCR 32
#define DFLT_LOOPS 20

#define VAL 0xCafeBabe

long *dst;
long *src;
long pSync[_SHMEM_COLLECT_SYNC_SIZE];

static int
atoi_scaled(char *s)
{
    long val;
    char *e;

    val = strtol(s,&e,0);
    if (e == NULL || *e =='\0')
        return val;

    if (*e == 'k' || *e == 'K')
        val *= 1024;
    else if (*e == 'm' || *e == 'M')
        val *= 1024*1024;
    else if (*e == 'g' || *e == 'G')
        val *= 1024*1024*1024;

    return (int) val;
}

int
main(int argc, char* argv[])
{
	int c, j, cloop, loops = DFLT_LOOPS;
	int mpe, num_pes;
	int nWords=1;
	int nIncr=1;
	int failures=0;
	char *pgm;

	start_pes(0);
	mpe = _my_pe();
	num_pes = _num_pes();

	if (num_pes == 1) {
   		Rfprintf(stderr,
			"ERR - Requires > 1 PEs\n");
		return 1;
	}
	pgm = strrchr(argv[0],'/');
	if ( pgm )
		pgm++;
	else
		pgm = argv[0];

	while((c=getopt(argc,argv,"hqVvl:")) != -1) {
		switch(c) {
		  case 'V':
		  case 'v':
			Verbose++;
			break;
		  case 'l':
            loops = atoi(optarg);
            break;
		  case 'h':
			Rfprintf(stderr,
                "usage: %s {-l loopcnt(%d)} {numLongs(%d)} {loopIncr(%d)}\n",
                    pgm,DFLT_LOOPS,DFLT_NWORDS,DFLT_INCR);
			return 1;
		  default:
			return 1;
		}
	}

	if (optind == argc)
		nWords = DFLT_NWORDS;
	else {
		nWords = atoi_scaled(argv[optind++]);
		if (nWords <= 0) {
    			Rfprintf(stderr, "ERR - Bad nBytes arg?\n");
			return 1;
		}
	}

	if (optind == argc)
		nIncr = DFLT_INCR;
	else {
		loops = atoi(argv[optind++]);
		if (nIncr <= 0 ) {
   		    Rfprintf(stderr, "ERR - incLongs arg out of bounds '%d'?\n", nIncr);
			return 1;
		}
	}

    if ( nWords % 8 ) { // integral multiple of longs
	    Rprintf("%s: nWords(%d) not a multiple of %ld?\n",
            pgm,nWords,sizeof(long));
        return 1;
    }

    for (c = 0; c < _SHMEM_COLLECT_SYNC_SIZE;c++)
        pSync[c] = _SHMEM_SYNC_VALUE;

    if (Verbose && mpe == 0)
	    fprintf(stderr,"loops(%d) nWords(%d) incr-per-loop(%d)\n",
            loops,nWords,nIncr);

	for(cloop=1; cloop <= loops; cloop++) {

        c = (sizeof(long)*nWords) * (num_pes + 1); // src + dst allocation.
        //nWords /= sizeof(long); // convert input of bytes --> longs.

        src = (long*)shmalloc(c);
        if ( !src ) {
	        Rprintf("[%d] %s: shmalloc(%d) failed?\n", mpe, pgm,c);
            return 0;
        }
        dst = &src[nWords];

	    for(j=0; j < nWords; j++)
		    src[j] = (long) (j + mpe*nWords);

		shmem_barrier_all();

        shmem_fcollect64(dst,src,nWords,0,0,num_pes,pSync);

        // Expect dst to be consecuative integers 0 ... (nLongs*num_pes)-1
        for(j=0; j < (nWords*num_pes); j++) {
            if ( dst[j] != (long) j ) {
                fprintf(stderr,
                    "[%d] dst[%d] %ld != expected %d\n",mpe,j,dst[j],j);
                return 1;
            }
        }
		shmem_barrier_all();

		if (Verbose && mpe == 0 && loops > 1) {
			fprintf(stderr,".");
		}
        nWords += nIncr;
	}

    if (Verbose && mpe == 0) {
	    fprintf(stderr,"\n");fflush(stderr);
    }
    shfree( (void*)src );
	shmem_barrier_all();
	if (Verbose)
        printf("%d(%d) Exit(%d)\n", mpe, num_pes, failures);

	return failures;
}
