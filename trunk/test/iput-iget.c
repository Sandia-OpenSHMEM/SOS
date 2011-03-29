/*
 * adaptation of example from SGI man page for shmem_iput.
 */

#include <stdio.h>
#include <string.h>

#include <mpp/shmem.h>

short source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static short target[10];
static short results[15];

int
main(int argc, char **argv)
{
    int me, nProcs, rc=0, j, k;

    start_pes(0);
    me = _my_pe();
    nProcs = _num_pes();

    if (me == 0) {
        /* put 5 words into target on PE's [1 to (nProcs-1)] */
        for (j=1; j < nProcs; j++) {
            shmem_short_iput(target, source, 1, 2, 5, j);
        }
    }

    shmem_barrier_all(); /* sync sender and receiver */

    if (me != 0) {
        // Verify iput data is expected
        if (target[0] != 1 ||
            target[1] != 3 ||
            target[2] != 5 ||
            target[3] != 7 ||
            target[4] != 9) {
                printf("ERR: target on PE %d is %hd %hd %hd %hd %hd\n"
                       "  Expected 1,3,5,7,9?\n",
                       me, target[0], target[1], target[2],
                       target[3], target[4] );
                rc = 1;
            }
    }

    shmem_barrier_all();

    if (rc != 0)
        return rc;

    if (me == 0) {
        printf("[0] iget(targets...)\n");

        // Pull data from PE1
        // iget() Procs target[0...4] --> results[Proc*5 ...Proc*5+5]
        // short_iget() == short_get() in the following case.
        //shmem_short_iget(&results[k], target, 1, 1, 5, 1);
        shmem_short_get(results, target, 10, 1);
        shmem_barrier_all();

        for (j=1,k=0; j < nProcs; j++,k+=5) {
            if (results[k] != 1 ||
                results[k+1] != 3 ||
                results[k+2] != 5 ||
                results[k+3] != 7 ||
                results[k+4] != 9)
                {
                    printf("[%d] ERR: results %hd %hd %hd %hd %hd\n"
                           "  Expected 1,3,5,7,9?\n",
                           me, results[k], results[k+1], results[k+2],
                           results[k+3], results[k+4] );
                    rc = 1;
                }
        }
    }  else {
        printf("[%d] target is %hd %hd %hd %hd %hd\n",
               me, target[0], target[1], target[2],
               target[3], target[4] );
        shmem_barrier_all();
    }

    shmem_barrier_all(); /* sync before exiting */

    return rc;
}
