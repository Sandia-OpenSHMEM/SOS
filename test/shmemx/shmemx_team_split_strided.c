/*
 * OpenSHMEM shmem_team_split_strided example to create a team of all even
 * ranked PEs from SHMEMX_TEAM_WORLD
 */

#include <shmem.h>
#include <shmemx.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    int                  rank, npes;
    int                  t_pe, t_size;
    shmemx_team_t        new_team;
    shmemx_team_config_t *config;

    shmem_init();
    config = NULL;
    rank   = shmem_my_pe();
    npes   = shmem_n_pes();

    shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 2, npes / 2, config, 0,
                             &new_team);

    if (new_team != SHMEMX_TEAM_NULL) {
        t_size = shmemx_team_n_pes(new_team);
        t_pe   = shmemx_team_my_pe(new_team);

        if ((rank % 2 != 0) || (rank / 2 != t_pe) || (npes / 2 != t_size)) {
            shmem_global_exit(1);
        }
    }

    shmem_finalize();
    return 0;
}

