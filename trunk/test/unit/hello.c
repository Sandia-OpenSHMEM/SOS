#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>

int
main(int argc, char* argv[], char *envp[])
{
    int me, my_num_pes;
    /*
    ** Starts/Initializes SHMEM/OpenSHMEM
    */
    start_pes(0);
    /*
    ** Fetch the number or processes
    ** Some implementations use num_pes();
    */
    my_num_pes = _num_pes();
    /*
    ** Assign my process ID to me
    */
    me = _my_pe();

    if (NULL == getenv("MAKELEVEL")) {
        printf("Hello World from %d of %d\n",me,my_num_pes);
    }

    return 0;
}
