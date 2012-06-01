/*
 * shmem_test_lock()  test_lock {-v|q|n} {loop-cnt}
 *  where:
 *      -v == verbose/debug
 *      -q == quiet
 *      -n x    x == Spin count threshold to announce excessive spin counts.
 *
 * For n loops:
 *   Each pe attempts to lock the global lock, if lock is taken, increment the
 *   lock count on all pes and then waits until the lock count reaches num_pes()
 *   which is the exit condition. 
 *   On a failed lock attempt, increment local lock_tries counter and repeat. 
 */
#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#define Rfprintf if (_my_pe() == 0) fprintf
#define Rprintf  if (_my_pe() == 0)  printf
#define Vfprintf if (Verbose) fprintf
#define Vprintf  if (Verbose) printf

int Verbose;
int Stats;
int Noise = 500;

int lock_cnt;
long lock;

typedef struct _lock_stat {
    int attempts;
} lock_stat_t;

lock_stat_t *lock_stats;

int
main(int argc, char* argv[])
{
    int pe, c, rc, cloop, loops;
    int  tries, lock_stats_sz;
    int my_rank, num_ranks;
    int Announce = (NULL == getenv("MAKELEVEL")) ? 1 : 0;

    start_pes(0);
    my_rank = _my_pe();
    num_ranks = _num_pes();
    if (num_ranks == 1) {
        fprintf(stderr, "ERR - Requires > 1 PEs (yod -c X, where X > 1\n");
        return 1;
    }

    while((c=getopt(argc,argv,"n:qv")) != -1) {
        switch(c) {
          case 'n':
            Noise = atoi(optarg); // threshold to announce spinloops
            break;
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
    if (Announce)
        Stats++;
    else
        Stats = 0;

    if (Stats) {
        lock_stats_sz = sizeof(lock_stat_t) * num_ranks; // lock stats
        lock_stats = shmalloc(lock_stats_sz);
        if ( !lock_stats ) {
            fprintf(stderr,"[%d] ERR: shmalloc(%d)\n",my_rank,lock_stats_sz);
            return 1;
        }
        memset( lock_stats, 0, lock_stats_sz );
    }

    for(cloop=1; cloop <= loops; cloop++) {
        lock = 0;
        lock_cnt = 0;
        tries = 0;
    
        shmem_barrier_all();  /* sync all ranks */

        while( *(&lock_cnt) < num_ranks ) {
            rc = shmem_test_lock(&lock);
            if ( rc == 0 ) {
                /* incr lock_cnt on all ranks */
                for(pe=0; pe < num_ranks; pe++) {
                    shmem_int_add( &lock_cnt, 1, pe );
                }
                Vprintf("[%d] locked: lock_cnt(%d)\n", my_rank, lock_cnt);
                shmem_clear_lock( &lock );
                shmem_int_wait_until( &lock_cnt, SHMEM_CMP_GE, num_ranks );
            }
            else {
                tries++;
                if ( Announce && ((tries % (num_ranks*Noise)) == 0) )
                    printf("[%d] unsuccessful lock attempts %d lock_cnt %d\n",
                            my_rank, tries, lock_cnt);
            }
        }
        shmem_barrier_all();  /* sync all ranks */

        if ( (cloop % 10) == 0 ) {
            if (my_rank == 0 && Announce)
                printf("%d ranks completed %d loops\n", num_ranks, cloop);
        }

        if (Stats)
            lock_stats[my_rank].attempts += tries;
    }

    if ( Stats && my_rank != 0 ) {
        shmem_int_put( &lock_stats[my_rank].attempts,
                        &lock_stats[my_rank].attempts, 1, 0 );
        shmem_fence();
    }
    shmem_barrier_all();  /* sync all ranks */

    if (Stats && my_rank == 0) {
        for(c=0; c < num_ranks; c++) {
            printf("[%d] lock attempts %d\n", c,
                    lock_stats[c].attempts);
        }
    }
    shmem_barrier_all();  /* sync all ranks */

    if (Stats)
        shfree(lock_stats);

    Vprintf ("[%d] of %d, Exit\n", my_rank, num_ranks);

    return 0;
}
