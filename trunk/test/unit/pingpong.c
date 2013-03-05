/*
 * pingpong test  pingpong {-v} {nLongs-2-put(128)} {loop-cnt(10)}
 *
 * For yod -c X
 * Proc0 puts nLongs to Proc1 ... Proc(X-1)
 *   Proc1 ... Proc(X-1) verify put data to be correct.
 *   Proc1 ... Proc(X-1) put nLongs(proc_ID) back to proc0 in
 *     work[proc_ID*nLongs] ... work[proc_ID*(nLongs-1)]
 *   Proc 0 verifes data
 */
#include <shmem.h>

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

static int atoi_scaled(char *s);

#define Rfprintf if (_my_pe() == 0) fprintf
#define Rprintf if (_my_pe() == 0)  printf

#define RDprintf if (Verbose && _my_pe() == 0)  printf
#define RDfprintf if (Verbose && _my_pe() == 0) fprintf

/* option flags */
#define OUTPUT_MOD 1	// output debug every X loops on -V
int output_mod = OUTPUT_MOD;
int Verbose;
int Slow;

long pSync0[_SHMEM_BARRIER_SYNC_SIZE],
    pSync1[_SHMEM_BARRIER_SYNC_SIZE],
    pSync2[_SHMEM_BARRIER_SYNC_SIZE],
    pSync3[_SHMEM_BARRIER_SYNC_SIZE],
    pSync4[_SHMEM_BARRIER_SYNC_SIZE];

#define DFLT_NWORDS 128
#define DFLT_LOOPS 10

#define VAL 0xCafeBabe

long *Target;
long *src;
long *work;

int
main(int argc, char* argv[])
{
	int c, j, loops, k, l;
	int my_pe, nProcs, nWorkers;
	int  nWords=1;
	int  failures=0;
	char *prog_name;
	long *wp,work_sz;

    for(j=0; j < _SHMEM_BARRIER_SYNC_SIZE; j++) {
        pSync0[j] = pSync1[j] = pSync2[j] = pSync3[j] =
            pSync4[j] = _SHMEM_SYNC_VALUE;
    }

	start_pes(0);
	my_pe = _my_pe();
	nProcs = _num_pes();
	nWorkers = nProcs - 1;

	if (nProcs == 1) {
   		Rfprintf(stderr,
			"ERR - Requires > 1 PEs (yod -c X, where X > 1\n");
		return 1;
	}

	for(j=0; j < nProcs; j++)
		if ( shmem_pe_accessible(j) != 1 ) {
			fprintf(stderr,
				"ERR - pe %d not accessible from pe %d\n",
				j, my_pe);
		}

	prog_name = strrchr(argv[0],'/');
	if ( prog_name )
		prog_name++;
	else
		prog_name = argv[0];

	while((c=getopt(argc,argv,"hvM:s")) != -1) {
		switch(c) {
		  case 's':
			Slow++;
			break;
		  case 'v':
			Verbose++;
			break;
		  case 'M':
			output_mod = atoi(optarg);
			if (output_mod <= 0) {
    				Rfprintf(stderr, "ERR - output modulo arg out of "
						"bounds '%d'?\n", output_mod);
				return 1;
			}
   			Rfprintf(stderr,"%s: output modulo %d\n",
					prog_name,output_mod);
			break;
		  case 'h':
			Rfprintf(stderr,
				"usage: %s {nWords-2-put(%d)K/M} {Loop-count(%d)K/M}\n",
				prog_name, DFLT_NWORDS, DFLT_LOOPS);
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
    			Rfprintf(stderr, "ERR - Bad nWords arg '%d'?\n", nWords);
			return 1;
		}
	}

	if (optind == argc)
		loops = DFLT_LOOPS;
	else {
		loops = atoi_scaled(argv[optind++]);
		if (loops <= 0 || loops > 1000000) {
    			Rfprintf(stderr,
				"ERR - loops arg out of bounds '%d'?\n", loops);
			return 1;
		}
	}

    work_sz = (nProcs*nWords) * sizeof(long);
	work = shmalloc( work_sz );
	if ( !work ) {
   		fprintf(stderr,"[%d] ERR - work = shmalloc(%ld) ?\n",my_pe,work_sz);
		return 1;
	}

	Target = shmalloc( 2 * nWords * sizeof(long) );
	if ( !Target ) {
   		fprintf(stderr,"[%d] ERR - Target = shmalloc(%ld) ?\n",
                my_pe, (nWords * sizeof(long)));
		return 1;
	}
    src = &Target[nWords];

#if _DEBUG
	Rprintf("%s: %d loops of %d longs per put\n",prog_name,loops,nWords);
#endif

	for(j=0; j < nWords; j++)
		src[j] = VAL;

	for(j=0; j < loops; j++) {

#if _DEBUG
		if ( Verbose && (j==0 || (j % output_mod) == 0) )
    			fprintf(stderr,"[%d] +(%d)\n", my_pe,j);
#endif
        shmem_barrier(0, 0, nProcs, pSync0);
		if ( my_pe == 0 ) {
			int p;
			for(p=1; p < nProcs; p++)
				shmem_long_put(Target, src, nWords, p);
		}
		else {
			if (Slow) {
				/* wait for each put to complete */
				for(k=0; k < nWords; k++)
					shmem_wait(&Target[k],my_pe);
			} else {
				/* wait for last word to be written */
				shmem_wait(&Target[nWords-1],my_pe);
			}
		}
#if _DEBUG
		if ( Verbose && (j==0 || (j % output_mod) == 0) )
    			fprintf(stderr,"[%d] -(%d)\n", _my_pe(),j);
#endif
        shmem_barrier(0, 0, nProcs, pSync1);

		RDprintf("Workers[1 ... %d] verify Target data put by proc0\n",
			nWorkers);

		/* workers verify put data is expected */
		if ( my_pe != 0 ) {
			for(k=0; k < nWords; k++) {
				if (Target[k] != VAL) {
					fprintf(stderr, "[%d] Target[%d] %#lx "
							"!= %#x?\n",
							my_pe,k,Target[k],VAL);
					failures++;
				}
				assert(Target[k] == VAL);
				Target[k] = my_pe;
			}
		}
		else	/* clear results buffer, workers will put here */
			memset(work, 0, work_sz);

        shmem_barrier(0, 0, nProcs, pSync2);

		RDprintf("Workers[1 ... %d] put Target data to PE0 work "
			"vector\n",nWorkers);

		if ( my_pe != 0 ) {
			/* push nWords of val my_pe back to PE zero */
			shmem_long_put(&work[my_pe * nWords], Target, nWords, 0);
		}
		else {
			/* wait for procs 1 ... nProcs to complete put()s */
			for(l=1; l < nProcs; l++) {
				wp = &work[ l*nWords ]; // procs nWords chunk
#if 1
				/* wait for last long to be written from each PE */
				shmem_wait(&wp[nWords-1],0);
#else
				for(k=0; k < nWords; k++)
					shmem_wait(&wp[k],0);
#endif
			}
		}

        shmem_barrier(0, 0, nProcs, pSync3);

		if ( my_pe == 0 ) {
			RDprintf("Loop(%d) PE0 verifing work data.\n",j);
			for(l=1; l < nProcs; l++) {
				wp = &work[ l*nWords ]; // procs nWords chunk
				for(k=0; k < nWords; k++) {
					if (wp[k] != l) {
						fprintf(stderr,
						"[0] PE(%d)_work[%d] %ld "
							"!= %d?\n",
							l,k,work[k],l);
						failures++;
					}
					assert(wp[k] == l);
					break;
				}
				if (failures)
					break;
			}
		}
        shmem_barrier(0, 0, nProcs, pSync4);
#if _DEBUG
		if (loops > 1) {
			Rfprintf(stderr,".");
			RDprintf("Loop(%d) Pass.\n",j);
		}
#endif
	}

    shfree( work );
    shfree( Target );

#if _DEBUG
	Rfprintf(stderr,"\n");fflush(stderr);
	shmem_barrier_all();
	RDprintf("%d(%d) Exit(%d)\n", my_pe, nProcs, failures);
#else
	shmem_barrier_all();
#endif

	return failures;
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
