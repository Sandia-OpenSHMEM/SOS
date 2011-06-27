/*
* reduce [0,1,2] + _my_pe() across 4 PEs with MAX()
*
* usage: big_reduce {-q|h}
*
* If run from a Makefile environment, be quiet, otherwise be verbose; -q forces quiet.
*/

#include <mpp/shmem.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

long pSync[_SHMEM_REDUCE_SYNC_SIZE];

#define N 128

long src[N];
long dst[N];

#define WRK_SIZE (N/2 + 1)	/* must be >= _SHMEM_REDUCE_MIN_WRKDATA_SIZE */
long pWrk[WRK_SIZE];

int
main(int argc, char* argv[])
{
    int i, Verbose=1;

	if (argc > 1) {
        if (strncmp(argv[1],"-q",3) == 0)
            Verbose=0;
        else if (strncmp(argv[1],"-h",3) == 0) {
            fprintf(stderr,"usage: %s {-q(quiet)|h(help)}\n",argv[0]);
            exit(1);
        }
    }

    for (i = 0; i < _SHMEM_REDUCE_SYNC_SIZE; i += 1) {
        pSync[i] = _SHMEM_SYNC_VALUE;
    }

    start_pes(0);

    for (i = 0; i < N; i += 1) {
        src[i] = _my_pe() + i;
    }
    shmem_barrier_all();

    shmem_long_max_to_all(dst, src, N, 0, 0, _num_pes(), pWrk, pSync);

    if (NULL == getenv("MAKELEVEL") &&  Verbose) {
        printf("%d/%d	dst =", _my_pe(), _num_pes() );
        for (i = 0; i < N; i+= 1) {
            printf(" %ld", dst[i]);
        }
        printf("\n");
    }

    for (i = 0; i < N; i+= 1) {
        if (dst[i] != _num_pes() - 1 + i) return 1;
    }

    return 0;
}
