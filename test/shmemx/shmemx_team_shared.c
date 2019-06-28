#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>
#include <shmemx.h>

#include <sys/types.h>
#include <unistd.h>


int main(void)
{
    static int dest[10];

    shmem_init();
    int me = shmem_my_pe();
    int npes = shmem_n_pes();

    int team_shared_npes = shmemx_team_n_pes(SHMEMX_TEAM_SHARED);
    int team_shared_me   = shmemx_team_my_pe(SHMEMX_TEAM_SHARED);

    int *peers = malloc(team_shared_npes * sizeof(int));
    size_t num_peers = 0;

    /* Who are my team members on SHEMX_TEAM_SHARED? */
    printf("[PE: %d] TEAM_SHARED peers: { ", me);
    for (int i = 0; i < npes; i++) {
        if (shmemx_team_translate_pe(SHMEMX_TEAM_WORLD, i,
                                     SHMEMX_TEAM_SHARED) != -1) {
            peers[num_peers++] = i;
            printf("%d ", i);
        }
    }

    printf("} (num_peers: %zu)\n", num_peers);

    if (num_peers != team_shared_npes) {
        shmem_global_exit(1);
    }

    /* Check that shmem_ptr is valid for all peers */
    //for (int i = 0; i < team_shared_npes; i++) {
    //    if (shmem_ptr(dest, peers[i]) == NULL) {
    //        shmem_global_exit(1);
    //    }
    //}

    free(peers);
    shmem_finalize();
    return 0;
}
  
