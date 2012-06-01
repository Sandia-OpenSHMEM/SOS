/*
 * test shmem_set_lock() set_lock {-v|q} {loop-cnt}
 * where: -q == quiet, -v == verbose/debug
 * Loop for loop-cnt
 *   all pes call shmem_set_lock(), in fifo order a pe will return from
 *   shmem_set_lock() holding the lock and increment the global lock count.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <shmem.h>

#define Rfprintf if (_my_pe() == 0) fprintf
#define Rprintf  if (_my_pe() == 0)  printf
#define RDfprintf if (Verbose && _my_pe() == 0) fprintf
#define RDprintf if (Verbose && _my_pe() == 0)  printf
#define Vprintf  if (Verbose)  printf
#define Vfprintf if (Verbose) fprintf

int Verbose;
int lock_cnt;
long lock;

int
main(int argc, char* argv[])
{
    int pe, c, cloop, loops;
    int my_rank, num_ranks;
    int Announce = (NULL == getenv("MAKELEVEL")) ? 1 : 0;

    start_pes(0);
    my_rank = _my_pe();
    num_ranks = _num_pes();
    if (num_ranks == 1) {
        fprintf(stderr, "ERR - Requires > 1 PEs (yod -c X, where X > 1\n");
        return 1;
    }

    while((c=getopt(argc,argv,"vq")) != -1) {
        switch(c) {
          case 'v':
            Verbose++;
            break;
          case 'q':
            Announce = 0;
            break;
          default:
            Rfprintf(stderr,"ERR - unknown -%c ?\n",c);
            return 1;
        }
    }

    if (optind == argc)
        loops = 40;
    else {
        loops = atoi(argv[optind++]);
        if (loops <= 0 || loops > 1000000) {
                Rfprintf(stderr,
                "ERR - loops arg out of bounds '%d'?\n", loops);
            return 1;
        }
    }

    for(cloop=1; cloop <= loops; cloop++) {

        lock_cnt = 0;
        lock = 0;
    
        shmem_barrier_all();  /* sync all ranks */
        
        shmem_set_lock(&lock);

        for(pe=0; pe < num_ranks; pe++) {
            shmem_int_add( &lock_cnt, 1, pe );
        }
        if (Verbose)
            printf("[%d] locked: lock_cnt(%d)\n", my_rank, lock_cnt);

        shmem_clear_lock( &lock );
    
        shmem_int_wait_until( &lock_cnt, SHMEM_CMP_GE, num_ranks );

        shmem_barrier_all();  /* sync all ranks */

        if ( (cloop % 10) == 0 ) {
            if (my_rank == 0 && Announce)
                printf("%d ranks completed %d loops\n", num_ranks, cloop);
        }

        if (lock_cnt != num_ranks)
            printf ("[%d] loop %d: bad lock_cnt %d, expected %d?\n",
                    my_rank, cloop, lock_cnt, num_ranks);
    }
    Vprintf ("[%d] of %d, Exit: lock_cnt %d\n", my_rank, num_ranks, lock_cnt);

    return 0;
}
