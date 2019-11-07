/*
 *  Copyright (c) 2019 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 *      Redistribution and use in source and binary forms, with or
 *      without modification, are permitted provided that the following
 *      conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
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

    int team_shared_npes = shmemx_team_n_pes(SHMEMX_TEAM_SHARED);

    int *peers = malloc(team_shared_npes * sizeof(int));
    size_t num_peers = 0;

    /* Print the team members on SHMEMX_TEAM_SHARED */
    /* Use a lock for cleaner output */
    shmem_set_lock(&lock);

    printf("[PE: %d] SHMEM_TEAM_SHARED peers: { ", me);
    for (int i = 0; i < npes; i++) {
        if (shmemx_team_translate_pe(SHMEMX_TEAM_WORLD, i,
                                     SHMEMX_TEAM_SHARED) != -1) {
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

    free(peers);
    shmem_finalize();
    return 0;
}
  
