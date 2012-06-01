#include <shmem.h>
#include <string.h>
#include <stdio.h>

int
main(int argc, char* argv[])
{
    long source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    static long target[10] = { 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
    int i, bad = 0;

    start_pes(0);

    if (_my_pe() == 0) {
        for (i = 0 ; i < 10 ; ++i) {
            source[i] = shmem_long_swap(target + i, source[i], 1);
        }
    }

    shmem_barrier_all();  /* sync sender and receiver */

    if (_my_pe() == 0) {
        for (i = 0 ; i < 10 ; ++i) {
            if (source[i] != i + 11) bad = 1;
        }
    } else if (_my_pe() == 1) {
        for (i = 0 ; i < 10 ; ++i) {
            if (target[i] != i + 1) bad = 1;
        }
    }

    if (bad) {
        fprintf(stderr, "[%02d] error: ", _my_pe());
        for (i = 0 ; i < 10 ; ++i) {
            fprintf(stderr, "%ld ", (_my_pe() == 0) ? source[i] : target[i]);
        }
        fprintf(stderr, "\n");
    }

    shmem_barrier_all();

    return bad;
}
