/*
* min_to_all [0...num_pes] 
*
* usage: min_to_all {-v|h}
*/

#include <mpp/shmem.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

long pSync[_SHMEM_REDUCE_SYNC_SIZE];

#define N 128

long src[N];
long dst[N];

#if N < _SHMEM_REDUCE_MIN_WRKDATA_SIZE
#define WRK_SIZE _SHMEM_REDUCE_MIN_WRKDATA_SIZE
#else
#define WRK_SIZE (N/2 + 1)	/* must be >= _SHMEM_REDUCE_MIN_WRKDATA_SIZE */
#endif

long pWrk[WRK_SIZE];

int
main(int argc, char* argv[])
{
    int i, Verbose=0;
    int mpe, num_pes;
    char *pgm;

    start_pes(0);
    mpe = _my_pe();
    num_pes = _num_pes();

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

	if (argc > 1) {
        if (strncmp(argv[1],"-v",2) == 0)
            Verbose=1;
        else if (strncmp(argv[1],"-h",2) == 0) {
            if (mpe == 0)
                fprintf(stderr,"usage: %s {-v(verbose)|h(help)}\n",pgm);
            exit(1);
        }
    }

    for (i = 0; i < _SHMEM_REDUCE_SYNC_SIZE; i += 1) {
        pSync[i] = _SHMEM_SYNC_VALUE;
    }

    for (i = 0; i < N; i++) {
        src[i] = (long) mpe + i;
        dst[i] = -1;
    }

    shmem_barrier_all();

    shmem_long_min_to_all(dst, src, N, 0, 0, num_pes, pWrk, pSync);

    if (Verbose) {
        printf("%d/%d	dst =", mpe, num_pes );
        for (i = 0; i < N; i+= 1) {
            printf(" %ld", dst[i]);
        }
        printf("\n");
    }

    for (i = 0; i < N; i++) {
        if (dst[i] != i) {
            fprintf(stderr, "[%d] %s: dst[%d] %ld != expected %d\n",
                    mpe, pgm, i, dst[i], i);
            return 1;
        }
    }
    return 0;
}
