#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char* argv[], char *envp[])
{
    int me, myshmem_n_pes;
    /*
    ** Starts/Initializes SHMEM/OpenSHMEM
    */
    shmem_init();
    /*
    ** Fetch the number or processes
    ** Some implementations use num_pes();
    */
    myshmem_n_pes = shmem_n_pes();
    /*
    ** Assign my process ID to me
    */
    me = shmem_my_pe();

    if (NULL == getenv("MAKELEVEL")) {
        printf("Hello World from %d of %d\n",me,myshmem_n_pes);
    }

    shmem_finalize();

    return 0;
}
