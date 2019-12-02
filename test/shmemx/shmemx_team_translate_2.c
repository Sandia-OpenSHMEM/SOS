/*
 * OpenSHMEM shmem_team_translate example to verify the team formed by even
 * ranked PEs from SHMEM_TEAM_WORLD using the team created from
 * shmem_team_split_stride operation
 */

#include <shmem.h>
#include <shmemx.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    int                  my_pe, npes;
    int                  t_pe_2, t_pe_3, t_pe_2_to_3, t_pe_3_to_2;
    shmemx_team_t        team_2s;
    shmemx_team_t        team_3s;
    shmemx_team_config_t *config;

    shmem_init();
    config = NULL;
    my_pe   = shmem_my_pe();
    npes   = shmem_n_pes();

    if (npes < 4) {
        fprintf(stderr, "ERR - Requires > 3 PEs\n");
        shmem_finalize();
        return 0;
    }

    shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 2, (npes/2)+1, config, 0,
                             &team_2s);
    shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 3, (npes/3)+1, config, 0,
                             &team_3s);

    t_pe_3 = shmemx_team_my_pe(team_3s);
    t_pe_2 = shmemx_team_my_pe(team_2s);
    t_pe_3_to_2 = shmemx_team_translate_pe(team_3s, t_pe_3, team_2s);
    t_pe_2_to_3 = shmemx_team_translate_pe(team_2s, t_pe_2, team_3s);

    if(my_pe != 0){
        if(my_pe % 2 == 0 && my_pe % 3 == 0 && my_pe != 0){
            if(t_pe_3 == -1 || t_pe_2 == -1){
                printf("ERROR: Unexpected behavior from SHMEMX_TEAM_TRANSLATE_PE\n");
            }
        } else if(my_pe % 2 == 0 && my_pe != 0){
            if(t_pe_3 != -1 || t_pe_2_to_3 != -1){
                printf("ERROR: Unexpected behavior from SHMEMX_TEAM_TRANSLATE_PE\n");
            }
        } else if(my_pe != 0){
            if(t_pe_2 != -1 || t_pe_3_to_2 != -1){
                printf("ERROR: Unexpected behavior from SHMEMX_TEAM_TRANSLATE_PE\n"); 
            }

        }
    }
    

    shmem_finalize();
    return 0;
}
