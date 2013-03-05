/*
 * adaptation of example from SGI man page for shmem_iput.
 * Enhanced to support 2..21 PEs.
 */

#include <stdio.h>
#include <string.h>

#include <shmem.h>

#define MAX_PE 256
#define WRDS 5

short source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static short target[10];
static short results[MAX_PE*WRDS];

int
main(int argc, char **argv)
{
    int me, nProcs, rc=0, j, k;

    start_pes(0);
    me = _my_pe();
    nProcs = _num_pes();
    if (nProcs > MAX_PE) {
        if (me == 0)
            printf("%s ERR: Too many PE, Max @ %d\n",argv[0], MAX_PE);
        return(1);
    }

    if (me == 0) {
        /* put 5 words into target on PE's [1 to (nProcs-1)] */
        for(j=1; j < nProcs; j++)
            shmem_short_iput(target, source, 1, 2, WRDS, j);
    }

    shmem_barrier_all(); /* sync sender and receiver */

    if (me != 0) {
        // Verify iput data is expected
        if (target[0] != 1 ||
            target[1] != 3 ||
            target[2] != 5 ||
            target[3] != 7 ||
            target[4] != 9)
        {
            printf("%s ERR: target on PE %d is %hd %hd %hd %hd %hd\n"
                "  Expected 1,3,5,7,9?\n", argv[0],
                me, target[0], target[1], target[2],
                target[3], target[4] );
            rc = 1;
        }
    }

    shmem_barrier_all();

    if (rc != 0)
        return rc;

    if (me == 0) {
        // Pull data from PE1...(nProcs-1)
        // iget() nProcs target[0...4] --> results[Proc*WRDS ...Proc*WRDS+WRDS]

        for(j=1,k=0; j < nProcs; j++,k+=WRDS)
            shmem_short_iget(&results[k], target, 1, 1, WRDS, j);

        for(j=1,k=0; j < nProcs; j++,k+=WRDS) {
            if (results[k] != 1 ||
                results[k+1] != 3 ||
                results[k+2] != 5 ||
                results[k+3] != 7 ||
                results[k+4] != 9)
            {
                printf("%s [%d] ERR: results %hd %hd %hd %hd %hd\n"
                    "  Expected 1,3,5,7,9?\n", argv[0],
                    me, results[k], results[k+1], results[k+2],
                    results[k+3], results[k+4] );
                rc = 1;
            }
        }
    }
#if _DEBUG
    else {
        printf("%s [%d] target is %hd %hd %hd %hd %hd\n", argv[0],
                me, target[0], target[1], target[2],
                target[3], target[4] );
    }
#endif

    shmem_barrier_all(); /* sync before exiting */

    return rc;
}
