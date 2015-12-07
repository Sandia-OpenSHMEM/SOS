/*
 * test if PE is accessible
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>

int
main(int argc, char* argv[])
{
    int me, npes;
    setbuf(stdout, NULL);
    shmem_init();
    me = shmem_my_pe();
    npes = shmem_n_pes();
    if (me == 0) {
        int i;
        int verbose = (NULL == getenv("MAKELEVEL")) ? 1 : 0;
        for (i = 1; i < npes; i += 1) {
            if (verbose) {
                printf("From %d: PE %d is ", me, i);
                printf("%s", shmem_pe_accessible(i) ? "" : "NOT ");
                printf("accessible\n");
            }
            if (! shmem_pe_accessible(i))
                shmem_global_exit(1);
        }
    }

    shmem_finalize();

    return 0;
}
