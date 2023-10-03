/* Copyright (c) 2023 Intel Corporation.
 * SPDX-License-Identifier: BSD-3-Clause
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 */

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

    int team_shared_npes = shmem_team_n_pes(SHMEMX_TEAM_NODE);

    int *peers = malloc(team_shared_npes * sizeof(int));
    int num_peers = 0;

    /* Print the team members on SHMEMX_TEAM_NODE */
    /* Use a lock for cleaner output */
    shmem_set_lock(&lock);

    printf("[PE: %d] SHMEMX_TEAM_NODE peers: { ", me);
    for (int i = 0; i < npes; i++) {
        if (shmem_team_translate_pe(SHMEM_TEAM_WORLD, i,
                                    SHMEMX_TEAM_NODE) != -1) {
            peers[num_peers++] = i;
            printf("%d ", i);
        }
    }

    printf("} (num_peers: %d)\n", num_peers);

    fflush(NULL);

    shmem_clear_lock(&lock);

    if (num_peers != team_shared_npes) {
        shmem_global_exit(1);
    }

    free(peers);
    shmem_finalize();
    return 0;
}
