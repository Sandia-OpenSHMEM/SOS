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
#include <shmem.h>
#include <shmemx.h>

#define NUM_TEAMS 64

int ret = 0;
int dest_ret = 0;

int main(void)
{
    int me, npes, i, j;
    int team_count = 0;

    shmem_init();

    me = shmem_my_pe();
    npes = shmem_n_pes();

    shmemx_team_t new_team[NUM_TEAMS];

    for (i = j = 0; i < NUM_TEAMS; ) {
        ret = shmemx_team_split_strided(SHMEMX_TEAM_WORLD, 0, 1, 1 + i % npes,
                                        NULL, 0, &new_team[i]);

        /* Wait for all PEs to fill in ret before starting the reduction */
        shmemx_sync(SHMEMX_TEAM_WORLD);
        shmemx_int_and_reduce(SHMEMX_TEAM_WORLD, &dest_ret, &ret, 1);

        /* If success was not global, free a team and retry */
        if (dest_ret != 0) {
            /* FIXME: The team is currently leaked in the case below */
            if (ret == 0)
                printf("%d: Local success and global failure on iteration %d\n",
                       me, i);

            /* No more teams to free */
            if (i == j)
                break;

            shmemx_team_destroy(new_team[j]);
            j++;
        } else {
            i++;
            team_count++;
        }
    }

    printf("The number of teams created for PE %d is : %d\n", me, team_count);

    shmem_finalize();

    return 0;
}