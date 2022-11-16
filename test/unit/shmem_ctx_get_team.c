/*
 *  Copyright (c) 2022 Intel Corporation. All rights reserved.
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

int main(void) {
    int ret;
    shmem_team_t retrieved_team;

    shmem_init();

    if (shmem_n_pes() < 2) {
        fprintf(stderr, "ERR - Requires at least 2 PEs\n");
        shmem_finalize();
        return 0;
    }

    /* test for invalid context */
    ret = shmem_ctx_get_team(SHMEM_CTX_INVALID, &retrieved_team);
    if (ret == 0 || retrieved_team != SHMEM_TEAM_INVALID) {
        fprintf(stderr, "Error in return values for SHMEM_CTX_INVALID\n");
        shmem_global_exit(1);
    }

    /* test for default context */
    ret = shmem_ctx_get_team(SHMEM_CTX_DEFAULT, &retrieved_team);
    if (ret != 0 || retrieved_team != SHMEM_TEAM_WORLD) {
        fprintf(stderr, "Error in return values for SHMEM_CTX_DEFAULT\n");
        shmem_global_exit(2);
    }

    /* test for a created context on a user defined team*/
    shmem_team_t         new_team;
    shmem_team_config_t  *config;

    config = NULL;
    int npes = shmem_n_pes();

    ret = shmem_team_split_strided(SHMEM_TEAM_WORLD, 0, 2, (npes + 1) / 2,
                                   config, 0, &new_team);
    if (new_team != SHMEM_TEAM_INVALID) {
        shmem_ctx_t team_ctx;
        if (!shmem_team_create_ctx(new_team, 0L, &team_ctx)) {
            ret = shmem_ctx_get_team(team_ctx, &retrieved_team);
            if (ret != 0 || retrieved_team != new_team) {
                fprintf(stderr, "Error in return values for a team context\n");
                shmem_global_exit(3);
            }
        }
    }

    shmem_finalize();

    return 0;
}
