/*
 * broadcast [0...num_pes]
 *
 * usage: bcast {-v|h}
 */

#include <mpp/shmem.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

long pSync[_SHMEM_REDUCE_SYNC_SIZE];

#define N 128

long src[N];

int
main(int argc, char* argv[])
{
    int i, Verbose=0;
    char *pgm;
    int mpe, num_pes, loops=10, cloop;
    long *dst;

    start_pes(0);
    mpe = _my_pe();
    num_pes = _num_pes();

    if ((pgm=strrchr(argv[0],'/')))
        pgm++;
    else
        pgm = argv[0];

	if (argc > 1) {
        if (strncmp(argv[1],"-v",3) == 0)
            Verbose=1;
        else if (strncmp(argv[1],"-h",3) == 0) {
            fprintf(stderr,"usage: %s {-v(verbose)|h(help)}\n",pgm);
            exit(1);
        }
    }

    for (i = 0; i < _SHMEM_BCAST_SYNC_SIZE; i += 1)
        pSync[i] = _SHMEM_SYNC_VALUE;

    for (i = 1; i < N; i++)
        src[i] = i+1;

    if ( mpe == 0 && Verbose )
        fprintf(stderr,"%d loops",loops);

    for(cloop=1; cloop <= loops; cloop++) {
        dst = (long *)shmalloc(sizeof(src));
        if ( !dst ) {
            fprintf(stderr,"[%d] shmalloc(%ld) failed %s\n",
                            mpe,sizeof(src),strerror(errno));
            return 0;
        }
        memset( (void*)dst, 0, sizeof(src) );
        shmem_barrier_all();

        shmem_broadcast64(dst, src, N, 1, 0, 0, num_pes, pSync);

        for(i=0; i < N; i++) {
            if ( dst[i] != src[i] ) {
                fprintf(stderr,"[%d] dst[%d] %ld != expected %ld\n",
                        mpe, i, dst[i],src[i]);
                return 1;
            }
        }
        shmem_barrier_all();

        if ( mpe == 0 && Verbose && cloop <= loops )
            fprintf(stderr,".");
        shfree (dst);
    }
    if ( mpe == 0 && Verbose )
        fprintf(stderr,"\n");

    return 0;
}
