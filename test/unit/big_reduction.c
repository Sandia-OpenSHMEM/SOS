/*
 * reduce across PEs with shmem_max_to_all()
 *
 * usage: big_reduction {-v|h}
*/

#include <shmem.h>
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
    int i, Verbose=0;
    char *pgm;

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

	if (argc > 1) {
        if (strncmp(argv[1],"-v",3) == 0)
            Verbose=1;
        else if (strncmp(argv[1],"-h",3) == 0) {
            fprintf(stderr,"usage: %s {-v(verbose)|h(help)}\n",pgm);
            shmem_finalize();
            exit(1);
        }
    }

    for (i = 0; i < _SHMEM_REDUCE_SYNC_SIZE; i += 1) {
        pSync[i] = _SHMEM_SYNC_VALUE;
    }

    shmem_init();

    if (shmem_n_pes() > N) {
        if (shmem_my_pe() == 0) {
            printf("Error: %s can be run on at most %d PEs unless N is increased\n",
                   argv[0], N);
        }
        shmem_finalize();
        return 0;
    }

    for (i = 0; i < N; i += 1) {
        src[i] = shmem_my_pe() + i;
    }
    shmem_barrier_all();

    shmem_long_max_to_all(dst, src, N, 0, 0, shmem_n_pes(), pWrk, pSync);

    if (Verbose) {
        printf("%d/%d	dst =", shmem_my_pe(), shmem_n_pes() );
        for (i = 0; i < N; i+= 1) {
            printf(" %ld", dst[i]);
        }
        printf("\n");
    }

    for (i = 0; i < N; i+= 1) {
        if (dst[i] != shmem_n_pes() - 1 + i) {
            printf("[%3d] Error: dst[%d] == %ld, expected %ld\n",
                   shmem_my_pe(), i, dst[i], shmem_n_pes() - 1 + (long) i);
            shmem_global_exit(1);
        }
    }

    shmem_finalize();

    return 0;
}
