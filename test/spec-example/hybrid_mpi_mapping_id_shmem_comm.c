/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <mpi.h>
#include <shmem.h>
#include <stdio.h>

int main(int argc, char *argv[]) {
    MPI_Init(&argc, &argv);
    shmem_init();

    int mype = shmem_my_pe();

    MPI_Comm shmem_comm;
    MPI_Comm_split(MPI_COMM_WORLD, 0, mype, &shmem_comm);

    int myrank;
    MPI_Comm_rank(shmem_comm, &myrank);
    printf("PE %d’s MPI rank is %d\n", mype, myrank);

    MPI_Comm_free(&shmem_comm);
    shmem_finalize();
    MPI_Finalize();

    return 0;
}
