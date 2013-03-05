/*
 * ping test  usage: ping {-V} {longs-per-put(128)} {loop-cnt(10)}
 *
 * For yod -c X ./ping
 * Proc0 puts nWords to Proc1 ... Proc(X-1)
 */
#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <assert.h>

#define Rfprintf if (_my_pe() == 0) fprintf
#define Rprintf if (_my_pe() == 0)  printf

#define RDprintf if (Verbose && _my_pe() == 0)  printf
#define RDfprintf if (Verbose && _my_pe() == 0) fprintf

/* option flags */
#define OUTPUT_MOD 1	// output debug every X loops
int output_mod = OUTPUT_MOD;
int Verbose;
int Slow;

#define DFLT_NWORDS 128
#define TARGET_SZ 8192
#define VAL 0xCafeBabe

long Target[TARGET_SZ];
long src[TARGET_SZ];

int
main(int argc, char* argv[])
{
	int c, j, loops, k;
	int proc, num_procs;
	int  nWords=1;
	int  failures=0;
	char *prog_name;

	start_pes(0);
	proc = _my_pe();
	num_procs = _num_pes();

	if (num_procs == 1) {
   		Rfprintf(stderr,
			"ERR - Requires > 1 PEs (yod -c X, where X > 1\n");
		return 1;
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
						"bounds '%d'?]\n", output_mod);
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
				"ERR - nWords arg out of bounds '%d' [1..%d]?\n",
				 nWords, TARGET_SZ);
			return 1;
		}
	}

	if (optind == argc)
		loops = 10;
	else {
		loops = atoi(argv[optind++]);
		if (loops <= 0 || loops > 1000000) {
    			Rfprintf(stderr,
				"ERR - loops arg out of bounds '%d'?\n", loops);
			return 1;
		}
	}

	//Rprintf("%s: %d loops of %d longs per put\n",prog_name,loops,nWords);

	for(j=0; j < nWords; j++)
		src[j] = VAL;

	for(j=0; j < loops; j++) {

		shmem_barrier_all();

		if ( Verbose && (j==0 || (j % output_mod) == 0) )
    			fprintf(stderr,"[%d] +(%d)\n", _my_pe(),j);

		if ( proc == 0 ) {
			int p;
			for(p=1; p < num_procs; p++)
				shmem_long_put(Target, src, nWords, p);
		}
		else {
			if (Slow) {
				/* wait for each put to complete */
				for(k=0; k < nWords; k++)
					shmem_wait(&Target[k],proc);
			} else {
				/* wait for last word to be written */
				shmem_wait(&Target[nWords-1],proc);
			}
		}

		if ( Verbose && (j==0 || (j % output_mod) == 0) )
    			fprintf(stderr,"[%d] -(%d)\n", _my_pe(),j);

		shmem_barrier_all();

		if ( proc != 0 ) {
			for(k=0; k < nWords; k++) {
				if (Target[k] != VAL) {
					fprintf(stderr, "[%d] Target[%d] %#lx "
							"!= %#x\?\n",
							proc,k,Target[k],VAL);
					failures++;
				}
				//assert(Target[proc] == VAL);
				Target[k] = 0;
			}
		}
		else
			memset(Target, 0, sizeof(Target));
	}

	shmem_barrier_all();

	if (failures || Verbose)
		Rprintf ("%d(%d) Exit(%d)\n", proc, num_procs, failures);

	return failures;
}
