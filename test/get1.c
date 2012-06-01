#include <shmem.h>

#include <string.h>
#include <stdio.h>

static long source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static long target[10];

int
main(int argc, char* argv[])
{
    int i;

    start_pes(0);

    if (_my_pe() == 0) {
        memset(target, 0, sizeof(target));
        /* put 10 elements into target on PE 1 */
        shmem_long_get(target, source, 10, 1);
    }

    shmem_barrier_all();  /* sync sender and receiver */

    if (_my_pe() == 0) {
        if (0 != memcmp(source, target, sizeof(long) * 10)) {
            fprintf(stderr,"[%d] Src & Target mismatch?\n",_my_pe());
            for (i = 0 ; i < 10 ; ++i) {
                printf("%ld,%ld ", source[i], target[i]);
            }
            printf("\n");
            return 1;
        }
    }
    shmem_barrier_all();

    return 0;
}
