/*
 *  Copyright (c) 2021 Intel Corporation. All rights reserved.
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

#include <shmem.h>
#include <stdio.h>

int main(void)
{
    int                 my_pe, npes, ret, errors = 0;
    shmem_team_t        even_team;
    shmem_team_config_t config;
    shmem_team_config_t new_config;

    shmem_init();
    my_pe  = shmem_my_pe();
    npes   = shmem_n_pes();


    ret = shmem_team_get_config(SHMEM_TEAM_WORLD, SHMEM_TEAM_NUM_CONTEXTS, &config);

    if (ret != 0) {
        printf("PE %d: unexpected return value (%d)\n", my_pe, ret);
        ++errors;
    }

    ret = shmem_team_get_config(SHMEM_TEAM_INVALID, 0, &config);

    if (ret == 0) {
        printf("PE %d: unexpected (SHMEM_TEAM_INVALID) return value (%d)\n", my_pe, ret);
        ++errors;
    }

    config.num_contexts = 3;

    shmem_team_split_strided(SHMEM_TEAM_WORLD, 0, 2, ((npes-1)/2)+1, &config, 0, &even_team);

    ret = shmem_team_get_config(even_team, SHMEM_TEAM_NUM_CONTEXTS, &new_config);

    if (shmem_team_my_pe(even_team) != -1 &&
           (new_config.num_contexts != 3  || ret != 0)) {
        printf("PE %d: unexpected num_contexts (%d)\n", my_pe, new_config.num_contexts);
        ++errors;
    }

    if (shmem_team_my_pe(even_team) == -1 && ret == 0) {
        printf("PE %d: unexpected return value from non-team members (%d)\n", my_pe, ret);
        ++errors;
    }

    shmem_finalize();
    return errors != 0;
}
