#include <stdio.h>
#include <stdlib.h>
#include <shmem.h>
#include <shmemx.h>


int main(void)
{
    static long lock = 0;

    shmem_init();
    int me = shmem_my_pe();
    int npes = shmem_n_pes();

    int team_shared_npes = shmemx_team_n_pes(SHMEMX_TEAM_HOST);

    int *peers = malloc(team_shared_npes * sizeof(int));
    size_t num_peers = 0;

    /* Print the team members on SHMEMX_TEAM_HOST */
    /* Use a lock for cleaner output */
    shmem_set_lock(&lock);

    printf("[PE: %d] TEAM_HOST peers: { ", me);
    for (int i = 0; i < npes; i++) {
        if (shmemx_team_translate_pe(SHMEMX_TEAM_WORLD, i,
                                     SHMEMX_TEAM_HOST) != -1) {
            peers[num_peers++] = i;
            printf("%d ", i);
        }
    }

    printf("} (num_peers: %zu)\n", num_peers);

    fflush(NULL);

    shmem_clear_lock(&lock);

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
  
