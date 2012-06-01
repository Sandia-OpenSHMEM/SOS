/*
 * broadcast [0...num_pes]
 *
 * usage: bcast {-v|h}
 *
 * Loop - shmem_broadcast_all() with increasing data amount.
 */

#include <shmem.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>

long pSync[_SHMEM_BCAST_SYNC_SIZE];

#define START_BCAST_SIZE 16
#define BCAST_INCR 1024

int
main(int argc, char* argv[])
{
    int i, Verbose=0;
    int mpe, num_pes, loops=10, cloop;
    char *pgm;
    long *dst, *src;
    int nBytes = START_BCAST_SIZE;
    int nLongs=0;

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

    if ( mpe == 0 && Verbose )
        fprintf(stderr,"%d loops\n",loops);

    for(cloop=1; cloop <= loops; cloop++) {

        nLongs = nBytes / sizeof(long);
        dst = (long *)shmalloc(nBytes*2);
        if ( !dst ) {
            fprintf(stderr,"[%d] shmalloc(%d) failed %s\n",
                            mpe,nBytes,strerror(errno));
            return 0;
        }
        memset( (void*)dst, 0, nBytes );
        src = &dst[nLongs];
        for (i = 1; i < nLongs; i++)
            src[i] = i+1;

        shmem_barrier_all();

        shmem_broadcast64(dst, src, nLongs, 1, 0, 0, num_pes, pSync);

        for(i=0; i < nLongs; i++) {
            if ( dst[i] != src[i] ) {
                fprintf(stderr,"[%d] dst[%d] %ld != expected %ld\n",
                        mpe, i, dst[i],src[i]);
                return 1;
            }
        }
        shmem_barrier_all();

        shfree (dst);
        if (Verbose && mpe ==0)
            fprintf(stderr,"loop %2d Bcast %d, Done.\n",cloop,nBytes);
        nBytes += BCAST_INCR;
    }

    return 0;
}
