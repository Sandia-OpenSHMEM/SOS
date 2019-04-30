/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2019 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */


#include "config.h"

#include "shmemx.h"
#include "shmem_team.h"
#include "shmem_collectives.h"

shmem_internal_team_t shmem_internal_team_world;
shmemx_team_t SHMEMX_TEAM_WORLD = (shmemx_team_t) &shmem_internal_team_world;


long *shmem_internal_psync_pool;
static uint64_t *shmem_internal_psync_pool_reserved;

static int num_teams = 0;

int shmem_internal_teams_init(void)
{

    shmem_internal_team_world.team_id       = ++num_teams;
    shmem_internal_team_world.psync_idx     = 0;
    shmem_internal_team_world.start         = 0;
    shmem_internal_team_world.stride        = 1;
    shmem_internal_team_world.size          = shmem_internal_num_pes;
    shmem_internal_team_world.config_mask   = 0;
    memset(&shmem_internal_team_world.config, 0, sizeof(shmemx_team_config_t));

    SHMEMX_TEAM_WORLD = (shmemx_team_t) &shmem_internal_team_world;

    /* Allocate pSync pool, each with the maximum possible size requirement */
    const long max_teams = shmem_internal_params.TEAMS_MAX;

    /* Create two pSyncs per team for back-to-back collectives.
     * Array organization:
     *
     * [ (team_world) (1st team) (2nd team) ... (team_world) (1st team) (2nd team) ... ]
     *  <------------- group 1 --------------->|<------------- group 2 ---------------->
     * */
    shmem_internal_psync_pool = shmem_internal_shmalloc(2 * sizeof(long) * SHMEM_SYNC_SIZE * max_teams);
    if (NULL == shmem_internal_psync_pool) return -1;

    for (size_t i = 0; i < 2*SHMEM_SYNC_SIZE*max_teams; i++) {
        shmem_internal_psync_pool[i] = SHMEM_SYNC_VALUE;
    }

    if (max_teams > (sizeof(uint64_t) * CHAR_BIT)) {
        RAISE_ERROR_MSG("Requested %ld teams, but only %zu are supported\n",
                         max_teams, sizeof(uint64_t) * CHAR_BIT);
        /* TODO: more bits (using a bit array)... */
    }

    shmem_internal_psync_pool_reserved = shmem_internal_shmalloc(sizeof(uint64_t));

    /* Set all bits to 1 (except the 1st bit for SHMEM_TEAM_WORLD) */
    *shmem_internal_psync_pool_reserved = ~((uint64_t)0) << 1;

    return 0;
}

void shmem_internal_teams_fini(void)
{
    //TODO: why does this seg fault?
    //shmem_free(shmem_internal_psync_pool);
    //shmem_free(shmem_internal_psync_pool_reserved);
    return;
}

int shmem_internal_team_my_pe(shmemx_team_t team)
{
    if (team == SHMEMX_TEAM_NULL)
        return -1;
    else
        return ((shmem_internal_team_t *)team)->my_pe;

    /* NOTE: could calculate this instead...
    shmem_internal_team_t *myteam = (shmem_internal_team_t *)team;

    int in_set = (shmem_internal_my_pe - myteam->start) % myteam->stride;
    int n = (shmem_internal_my_pe - myteam->start) / myteam->stride;
    if (in_set || n >= myteam->size)
        return -1;
    else {
        return n;
    }
    */
}

int shmem_internal_team_n_pes(shmemx_team_t team)
{
    if (team == SHMEMX_TEAM_NULL)
        return -1;
    else
        return ((shmem_internal_team_t *)team)->size;
}

void shmem_internal_team_get_config(shmemx_team_t team, shmemx_team_config_t *config)
{
    return;
}

int shmem_internal_team_translate_pe(shmemx_team_t src_team, int src_pe, shmemx_team_t dest_team)
{
    int src_pe_world, dest_pe;

    if (src_team == SHMEMX_TEAM_NULL || dest_team == SHMEMX_TEAM_NULL)
        return -1;

    shmem_internal_team_t *team_dest = (shmem_internal_team_t *) dest_team;
    shmem_internal_team_t *team_src  = (shmem_internal_team_t *) src_team;

    if (src_pe > team_src->size)
        return -1;

    src_pe_world = team_src->start + src_pe * team_src->stride;

    if (src_pe_world < team_src->start || src_pe_world >= shmem_internal_num_pes)
        return -1;

    shmem_internal_pe_in_active_set(src_pe_world, team_dest->start, team_dest->stride,
                                    team_dest->size, &dest_pe);

    return dest_pe;
}

int shmem_internal_team_split_strided(shmem_internal_team_t *parent_team, int PE_start, int PE_stride,
                                      int PE_size, shmemx_team_config_t *config, long config_mask,
                                      shmem_internal_team_t **new_team)
{

    if (PE_size <= 0 || PE_stride < 1 || PE_start < 0)
        return -1;

    *new_team = SHMEMX_TEAM_NULL;

    shmem_internal_team_t *myteam = calloc(1, sizeof(shmem_internal_team_t));

    myteam->team_id     = ++num_teams;
    myteam->start       = PE_start;
    myteam->stride      = PE_stride;
    myteam->size        = PE_size;
    if (config) {
        myteam->config      = *config;
        myteam->config_mask = config_mask;
    }

    if (shmem_internal_pe_in_active_set(shmem_internal_my_pe, PE_start, PE_stride,
                                        PE_size, &myteam->my_pe)) {
        //TODO: will we need a pool of pWrk arrays?

        shmem_internal_op_to_all(shmem_internal_psync_pool_reserved,
                                 shmem_internal_psync_pool_reserved, 1, sizeof(uint64_t),
                                 PE_start, PE_stride, PE_size, NULL,
                                 &shmem_internal_psync_pool[parent_team->psync_idx*SHMEM_SYNC_SIZE],
                                 SHM_INTERNAL_BAND, SHM_INTERNAL_UINT64);

        //printf("reserved bits: ");
        //shmem_internal_print_bits(shmem_internal_psync_pool_reserved, sizeof(long));

        /* Select the least signficant nonzero bit, which corresponds to an available pSync. */
        myteam->psync_idx = shmem_internal_1st_nonzero_bit(shmem_internal_psync_pool_reserved, sizeof(long));
        if (myteam->psync_idx == -1) {
            RAISE_ERROR_MSG("No more teams available (max = %ld), try increasing SHMEM_TEAMS_MAX\n",
                            shmem_internal_params.TEAMS_MAX);
        }

        *new_team = myteam;
    }

    return 0;
}

int shmem_internal_team_split_2d(shmemx_team_t parent_team, int xrange, shmemx_team_config_t *xaxis_config, long xaxis_mask, shmemx_team_t *xaxis_team, shmemx_team_config_t *yaxis_config, long yaxis_mask, shmemx_team_t *yaxis_team)
{
    return -1;
}

int shmem_internal_team_destroy(shmemx_team_t *team)
{
    return -1;
}

int shmem_internal_team_create_ctx(shmemx_team_t team, long options, shmem_ctx_t *ctx)
{
    return -1;
}

int shmem_internal_ctx_get_team(shmem_ctx_t ctx, shmemx_team_t *team)
{
    return -1;
}
