#include <shmem.h>

#include <string.h>
#include <stdio.h>

static long source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
static long target[10];

int
main(int argc, char* argv[])
{
    int i;

    shmem_init();

    if (shmem_n_pes() == 1) {
        printf("%s: Requires number of PEs > 1\n", argv[0]);
        shmem_finalize();
        return 0;
    }

    if (shmem_my_pe() == 0) {
        memset(target, 0, sizeof(target));
        /* put 10 elements into target on PE 1 */
        shmem_long_get(target, source, 10, 1);
    }

    shmem_barrier_all();  /* sync sender and receiver */

    if (shmem_my_pe() == 0) {
        if (0 != memcmp(source, target, sizeof(long) * 10)) {
            fprintf(stderr,"[%d] Src & Target mismatch?\n",shmem_my_pe());
            for (i = 0 ; i < 10 ; ++i) {
                printf("%ld,%ld ", source[i], target[i]);
            }
            printf("\n");
            shmem_global_exit(1);
        }
    }

    shmem_finalize();

    return 0;
}
