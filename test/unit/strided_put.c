/*
* adaptation of example from SGI man page for shmem_iput.
*
* Code slightly cleaned up (removed cache call, fixed % format)
*
*/
#include <stdio.h>
#include <shmem.h>

int
main(int argc, char* argv[])
{
    short source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    static short target[10];
    int me;
    int ret = 0;

    start_pes(0);
    me = _my_pe();

    if (me == 0) {
        /* put 10 words into target on PE 1 */
        shmem_short_iput(target, source, 1, 2, 5, 1);
    }

    shmem_barrier_all();	/* sync sender and receiver */

    if (me == 1) {
        if (! (target[0] == source[0] &&
               target[1] == source[2] &&
               target[2] == source[4] &&
               target[3] == source[6] &&
               target[4] == source[8])) {
            printf("target on PE %d is %hd %hd %hd %hd %hd\n", me,
                   target[0], target[1], target[2],
                   target[3], target[4] );
            ret = 1;
        }
    }
    shmem_barrier_all();	/* sync before exiting */

    return ret;
}
