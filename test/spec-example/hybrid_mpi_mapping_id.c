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

    int mype = shmem_team_my_pe(SHMEM_TEAM_WORLD);
    int npes = shmem_team_n_pes(SHMEM_TEAM_WORLD);

    static int myrank;
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

    int *mpi_ranks = shmem_calloc(npes, sizeof(int));

    shmem_int_collect(SHMEM_TEAM_WORLD, mpi_ranks, &myrank, 1);
    if (mype == 0)
        for (int i = 0; i < npes; i++)
            printf("PE %d’s MPI rank is %d\n", i, mpi_ranks[i]);

    shmem_free(mpi_ranks);

    shmem_finalize();
    MPI_Finalize();

    return 0;
}
