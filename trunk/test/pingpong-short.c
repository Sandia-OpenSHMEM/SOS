/*
 * pingpong test  pingpong-short {-V} {numShorts-2-put(128)} {loop-cnt(10)}
 *
 * For yod -c X
 * Proc0 puts nShorts to Proc1 ... Proc(X-1)
 *   Proc1 ... Proc(X-1) verify put data to be correct.
 *   Proc1 ... Proc(X-1) put nShorts(proc_ID) back to proc0 in
 *     work[proc_ID*nLongs] ... work[proc_ID*(nLongs-1)]
 *   Proc 0 verifes data
 */
#include <mpp/shmem.h>

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

#define Rfprintf if (_my_pe() == 0) fprintf
#define Rprintf if (_my_pe() == 0)  printf

#define RDprintf if (Verbose && _my_pe() == 0)  printf
#define RDfprintf if (Verbose && _my_pe() == 0) fprintf

/* option flags */
#define OUTPUT_MOD 1	// output debug every X loops on -V
int output_mod = OUTPUT_MOD;
int Verbose;
int Slow;

#define DFLT_NWORDS 128
#define DFLT_LOOPS 100

#define TARGET_SZ 8192
#define VAL 0xCafe

#define DataType short

DataType Target[TARGET_SZ];
DataType src[TARGET_SZ];
DataType work[TARGET_SZ];

int
main(int argc, char* argv[])
{
	int c, j, loops, k, l;
	int proc, nProcs, nWorkers;
	int  nWords=1;
	int  failures=0;
	char *prog_name;
	DataType *wp;

	start_pes(0);
	proc = _my_pe();
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
				j, proc);
		}

	prog_name = strrchr(argv[0],'/');
	if ( prog_name )
		prog_name++;
	else
		prog_name = argv[0];

	while((c=getopt(argc,argv,"hVM:s")) != -1) {
		switch(c) {
		  case 's':
			Slow++;
			break;
		  case 'V':
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
				"usage: %s {nWords-2-put} {Loop-count}\n",
				prog_name);
			return 1;
		  default:
			return 1;
		}
	}

	if (optind == argc)
		nWords = DFLT_NWORDS;
	else {
		nWords = atoi(argv[optind++]);
		if (nWords <= 0 || nWords > TARGET_SZ) {
    			Rfprintf(stderr,
				"ERR - nWords arg out of bounds '%d' [1..%d?\n",
				 nWords, TARGET_SZ);
			return 1;
		}
	}

	if (optind == argc)
		loops = DFLT_LOOPS;
	else {
		loops = atoi(argv[optind++]);
		if (loops <= 0 || loops > 1000000) {
    			Rfprintf(stderr,
				"ERR - loops arg out of bounds '%d'?\n", loops);
			return 1;
		}
	}

	if (Verbose) Rprintf("%s: %d loops of %d shorts per put/get cycle\n",
		prog_name,loops,nWords);

	for(j=0; j < nWords; j++)
		src[j] = (DataType)VAL;

	for(j=0; j < loops; j++) {

		shmem_barrier_all();
#if _DEBUG
		if ( Verbose && (j==0 || (j % output_mod) == 0) )
    			fprintf(stderr,"[%d] +(%d)\n", _my_pe(),j);
#endif
		if ( proc == 0 ) {
			int p;
			for(p=1; p < nProcs; p++)
				shmem_short_put(Target, src, nWords, p);
		}
		else {
			if (Slow) {
				/* wait for each put to complete */
				for(k=0; k < nWords; k++)
					shmem_short_wait(&Target[k],proc);
			} else {
				/* wait for last word to be written */
				shmem_short_wait(&Target[nWords-1],proc);
			}
		}
#if _DEBUG
		if ( Verbose && (j==0 || (j % output_mod) == 0) )
    			fprintf(stderr,"[%d] -(%d)\n", _my_pe(),j);
#endif
		shmem_barrier_all();

		RDprintf("Workers[1 ... %d] verify Target data put by proc0\n",
			nWorkers);

		/* workers verify put data is expected */
		if ( proc != 0 ) {
			for(k=0; k < nWords; k++) {
				if (Target[k] != (DataType)VAL) {
					fprintf(stderr, "[%d] Target[%d] %#hx "
							"!= %#hx?\n",
							proc,k,Target[k],VAL);
					failures++;
				}
				assert(Target[k] == (DataType)VAL);
				Target[k] = proc;
			}
		}
		else	/* clear results buffer, workers will put here */
			memset(work, 0, sizeof(work));

		shmem_barrier_all();

		RDprintf("Workers[1 ... %d] put Target data to proc0 work "
			"vector\n",nWorkers);

		if ( proc != 0 ) {
			/* push nWords of val my_proc_num back to proc zero */
			shmem_short_put(&work[proc*nWords], Target, nWords, 0);
		}
		else {
			/* wait for procs 1 ... nProcs to complete put()s */
			for(l=1; l < nProcs; l++) {
				wp = &work[ l*nWords ]; // procs nWords chunk
#if 1
				/* wait for last DataType to be written */
				shmem_short_wait(&wp[nWords-1],0);
#else
				for(k=0; k < nWords; k++)
					shmem_short_wait(&wp[k],0);
#endif
			}
		}
		shmem_barrier_all();

		if ( proc == 0 ) {
			RDprintf("Loop(%d) proc0 verifing work data.\n",j);
			for(l=1; l < nProcs; l++) {
				wp = &work[ l*nWords ]; // procs nWords chunk
				for(k=0; k < nWords; k++) {
					if (wp[k] != l) {
						fprintf(stderr,
						"[0] proc(%d)_work[%d] %hd "
							"!= %hd?\n",
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
		shmem_barrier_all();
		if (loops > 1) {
			RDfprintf(stderr,".");
			RDprintf("Loop(%d) Pass.\n",j);
		}
	}
	RDfprintf(stderr,"\n");fflush(stderr);
	shmem_barrier_all();
	RDprintf("%d(%d) Exit(%d)\n", proc, nProcs, failures);

	return failures;
}
