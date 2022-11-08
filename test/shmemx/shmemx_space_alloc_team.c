/*
 *  This test program is derived from an example program in the
 *  OpenSHMEM specification.
 */

#include <shmem.h>
#include <shmemx.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    int                  rank, npes;
    int                  t_pe, t_size;
    int                  ret;
    shmem_team_t         new_team;
    shmem_team_config_t  *config;

    shmem_init();
    config = NULL;
    rank   = shmem_my_pe();
    npes   = shmem_n_pes();

    if (npes < 2) {
        fprintf(stderr, "ERR - Requires at least 2 PEs\n");
        shmem_finalize();
        return 0;
    }

    ret = shmem_team_split_strided(SHMEM_TEAM_WORLD, 0, 2, (npes + 1) / 2,
                                    config, 0, &new_team);

    if (ret != 0) {
        shmem_global_exit(2);
    }

    t_size = shmem_team_n_pes(new_team);
    t_pe   = shmem_team_my_pe(new_team);

    if (new_team != SHMEM_TEAM_INVALID) {
        if ((rank % 2 != 0) || (rank / 2 != t_pe) || ((npes + 1) / 2 != t_size)) {
            shmem_global_exit(3);
        }
    } else {
        if ((rank % 2 == 0) || (t_pe != -1) || (t_size != -1)) {
            shmem_global_exit(4);
        }
    }

    if (t_pe != -1) {
        shmemx_space_t *spaces = NULL;
        shmemx_get_spaces_from_team(new_team, &spaces);
        if (!spaces) {
            fprintf(stderr, "Error in retrieving space from team\n");
	    shmem_global_exit(5);
        }
	int *team_space_alloc = shmemx_space_malloc(spaces[0], 512 * sizeof(int));
	if (!team_space_alloc) {
            fprintf(stderr, "Error in space allocation from team\n");
	    shmem_global_exit(6);
	}
	shmemx_space_free(spaces[0], team_space_alloc);
    }

    shmem_finalize();
    return 0;
}

