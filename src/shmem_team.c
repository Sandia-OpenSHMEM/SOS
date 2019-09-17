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
#include "shmem_remote_pointer.h"

#include <math.h>

#define SHMEMX_TEAM_WORLD_INDEX   0
#define SHMEMX_TEAM_SHARED_INDEX  1
#define SHMEMX_TEAM_HOST_INDEX    2
#define SHMEMX_TEAM_LEADERS_INDEX 3
#define NUM_PREDEFINED_TEAMS      4

shmem_internal_team_t shmem_internal_team_world;
shmemx_team_t SHMEMX_TEAM_WORLD = (shmemx_team_t) &shmem_internal_team_world;

shmem_internal_team_t shmem_internal_team_shared;
shmemx_team_t SHMEMX_TEAM_SHARED = (shmemx_team_t) &shmem_internal_team_shared;

shmem_internal_team_t shmem_internal_team_host;
shmemx_team_t SHMEMX_TEAM_HOST = (shmemx_team_t) &shmem_internal_team_host;

shmem_internal_team_t shmem_internal_team_leaders;
shmemx_team_t SHMEMX_TEAM_LEADERS = (shmemx_team_t) &shmem_internal_team_leaders;

shmem_internal_team_t **shmem_internal_team_pool;
long *shmem_internal_psync_pool;
long *shmem_internal_psync_barrier_pool;
static uint64_t *psync_pool_avail;
static uint64_t *psync_pool_avail_reduced;


static inline
int check_stride(int pe, int *start, int *stride, int *size)
{
    if (*start < 0) {
        *start = pe;
        (*size)++;
    } else if (*stride < 0) {
        *stride = pe - *start;
        (*size)++;
    } else if ((pe - *start) % *stride != 0) {
        RAISE_WARN_STR("Detected non-uniform stride across on-node PEs");
        return -1;
    } else {
        (*size)++;
    }
    return 0;
}

/* Team Management Routines */

int shmem_internal_teams_init(void)
{

    /* Initialize SHMEM_TEAM_WORLD */
    shmem_internal_team_world.psync_idx      = SHMEMX_TEAM_WORLD_INDEX;
    shmem_internal_team_world.start          = 0;
    shmem_internal_team_world.stride         = 1;
    shmem_internal_team_world.size           = shmem_internal_num_pes;
    shmem_internal_team_world.my_pe          = shmem_internal_my_pe;
    shmem_internal_team_world.config_mask    = 0;
    shmem_internal_team_world.contexts_len   = 0;
    memset(&shmem_internal_team_world.config, 0, sizeof(shmemx_team_config_t));
    SHMEMX_TEAM_WORLD = (shmemx_team_t) &shmem_internal_team_world;

    /* Initialize SHMEM_TEAM_SHARED and SHMEMX_TEAM_HOST */
    shmem_internal_team_shared.psync_idx     = SHMEMX_TEAM_SHARED_INDEX;
    shmem_internal_team_shared.my_pe         = shmem_internal_my_pe;
    shmem_internal_team_shared.config_mask   = 0;
    shmem_internal_team_shared.contexts_len  = 0;
    memset(&shmem_internal_team_shared.config, 0, sizeof(shmemx_team_config_t));
    SHMEMX_TEAM_SHARED = (shmemx_team_t) &shmem_internal_team_shared;

    shmem_internal_team_host.psync_idx       = SHMEMX_TEAM_HOST_INDEX;
    shmem_internal_team_host.my_pe           = shmem_internal_my_pe;
    shmem_internal_team_host.config_mask     = 0;
    shmem_internal_team_host.contexts_len    = 0;
    memset(&shmem_internal_team_host.config, 0, sizeof(shmemx_team_config_t));
    SHMEMX_TEAM_HOST = (shmemx_team_t) &shmem_internal_team_host;

    shmem_internal_team_leaders.psync_idx    = SHMEMX_TEAM_LEADERS_INDEX;
    shmem_internal_team_leaders.my_pe        = shmem_internal_my_pe;
    shmem_internal_team_leaders.config_mask  = 0;
    shmem_internal_team_leaders.contexts_len = 0;
    memset(&shmem_internal_team_leaders.config, 0, sizeof(shmemx_team_config_t));
    SHMEMX_TEAM_LEADERS = (shmemx_team_t) &shmem_internal_team_leaders;
    /* If disabled, SHMEM_TEAM_SHARED only contains this (self) PE */
    if (shmem_internal_params.DISABLE_TEAM_SHARED) {
        shmem_internal_team_shared.start         = shmem_internal_my_pe;
        shmem_internal_team_shared.stride        = 1;
        shmem_internal_team_shared.size          = 1;
    } else { /* Search for on-node peer PEs while checking for a consistent stride */
        int start = -1, stride = -1, size = 0;

        for (int pe = 0; pe < shmem_internal_num_pes; pe++) {
            void *ret_ptr = shmem_internal_ptr(shmem_internal_heap_base, pe);
            if (ret_ptr == NULL) continue;

            int ret = check_stride(pe, &start, &stride, &size);
            if (ret < 0) return ret;
        }
        shmem_internal_assert(size > 0 && size <= shmem_runtime_get_node_size());

        shmem_internal_team_shared.start = start;
        shmem_internal_team_shared.stride = (stride == -1) ? 1 : stride;
        shmem_internal_team_shared.size = size;

        DEBUG_MSG("SHMEM_TEAM_SHARED: start=%d, stride=%d, size=%d\n",
                  shmem_internal_team_shared.start, shmem_internal_team_shared.stride,
                  shmem_internal_team_shared.size);
    }

    int start = -1, stride = -1, size = 0;

    for (int pe = 0; pe < shmem_internal_num_pes; pe++) {

        int ret = shmem_runtime_get_node_rank(pe);
        if (ret < 0) continue;

        ret = check_stride(pe, &start, &stride, &size);
        if (ret < 0) return ret;
    }
    shmem_internal_assert(size > 0 && size == shmem_runtime_get_node_size());

    shmem_internal_team_host.start = start;
    shmem_internal_team_host.stride = (stride == -1) ? 1 : stride;
    shmem_internal_team_host.size = size;

    DEBUG_MSG("SHMEMX_TEAM_HOST: start=%d, stride=%d, size=%d\n",
              shmem_internal_team_host.start, shmem_internal_team_host.stride,
              shmem_internal_team_host.size);

    /* Initialize SHMEM_TEAM_LEADERS */
    /* FIXME: Assumes 0,1,...,N PE ordering, but what about round robin or arbitrary ordering? */
    if (shmem_internal_pe_in_active_set(shmem_internal_my_pe, 0, shmem_runtime_get_node_size(),
                                    shmem_internal_num_pes / shmem_runtime_get_node_size(), NULL)) {
        shmem_internal_team_leaders.start = 0;
        shmem_internal_team_leaders.stride = shmem_runtime_get_node_size();
        shmem_internal_team_leaders.size = shmem_internal_num_pes / shmem_runtime_get_node_size();

        DEBUG_MSG("SHMEMX_TEAM_LEADERS: start=%d, stride=%d, size=%d\n",
                  shmem_internal_team_leaders.start, shmem_internal_team_leaders.stride,
                  shmem_internal_team_leaders.size);
    } else {
        SHMEMX_TEAM_LEADERS = SHMEMX_TEAM_INVALID;
    }

    const long max_teams = shmem_internal_params.TEAMS_MAX;

    shmem_internal_team_pool = malloc(max_teams * sizeof(shmem_internal_team_t*));

    /* Allocate pSync pool, each with the maximum possible size requirement */
    /* Create two pSyncs per team for back-to-back collectives.
     * Array organization:
     *
     * [ (world) (shared) (team 1) (team 2) ...  (world) (shared) (team 1) (team 2) ... ]
     *  <-------------- group-1 --------------->|<------------- group-2 ---------------->
     *  <--- (bcast, collect, reduce, etc.) --->|<------ (barriers and syncs) ---------->
     * */
    shmem_internal_psync_pool = shmem_internal_shmalloc(2 * sizeof(long) * SHMEM_SYNC_SIZE * max_teams);
    if (NULL == shmem_internal_psync_pool) return -1;

    for (size_t i = 0; i < 2 * SHMEM_SYNC_SIZE * max_teams; i++) {
        shmem_internal_psync_pool[i] = SHMEM_SYNC_VALUE;
    }

    /* Convenience pointer to the group-2 pSync array (for barriers and syncs): */
    shmem_internal_psync_barrier_pool = &shmem_internal_psync_pool[max_teams * SHMEM_SYNC_SIZE];

    if (max_teams > (sizeof(uint64_t) * CHAR_BIT)) {
        RAISE_ERROR_MSG("Requested %ld teams, but only %zu are supported\n",
                         max_teams, sizeof(uint64_t) * CHAR_BIT);
        /* TODO: support more than 64 bits (using a bit array)... */
    }

    psync_pool_avail = shmem_internal_shmalloc(2 * sizeof(uint64_t));
    psync_pool_avail_reduced = &psync_pool_avail[1];

    /* Set all psync bits to 1 (except the 1st bit for SHMEM_TEAM_WORLD and
                                the 2nd bit for SHMEM_TEAM_SHARED)
                                the 3rd bit for SHMEMX_TEAM_HOST)
                                the 4th bit for SHMEMX_TEAM_LEADERS) */
    *psync_pool_avail = ~((uint64_t)0) << NUM_PREDEFINED_TEAMS;

    shmem_internal_team_pool[0] = &shmem_internal_team_world;
    shmem_internal_team_pool[1] = &shmem_internal_team_shared;
    shmem_internal_team_pool[2] = &shmem_internal_team_host;
    shmem_internal_team_pool[3] = &shmem_internal_team_leaders;

    for (size_t i = NUM_PREDEFINED_TEAMS; i < max_teams; i++) {
        shmem_internal_team_pool[i] = NULL;
    }

    return 0;
}

void shmem_internal_teams_fini(void)
{
    /* Destroy all undestroyed teams */
    for (size_t i = 0; i < shmem_internal_params.TEAMS_MAX; i++) {
        if (shmem_internal_team_pool[i] != NULL)
            shmem_internal_team_destroy(&shmem_internal_team_pool[i]);
    }

    free(shmem_internal_team_pool);
    shmem_internal_free(shmem_internal_psync_pool);
    shmem_internal_free(psync_pool_avail);

    return;
}

int shmem_internal_team_my_pe(shmem_internal_team_t *team)
{
    if (team == SHMEMX_TEAM_INVALID)
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

int shmem_internal_team_n_pes(shmem_internal_team_t *team)
{
    if (team == SHMEMX_TEAM_INVALID)
        return -1;
    else
        return ((shmem_internal_team_t *)team)->size;
}

void shmem_internal_team_get_config(shmem_internal_team_t *team, shmemx_team_config_t *config)
{
    shmem_internal_team_t *myteam = (shmem_internal_team_t *)team;
    *config = myteam->config;
    return;
}

int shmem_internal_team_translate_pe(shmem_internal_team_t *src_team, int src_pe,
                                     shmem_internal_team_t *dest_team)
{
    int src_pe_world, dest_pe = -1;

    if (src_team == SHMEMX_TEAM_INVALID || dest_team == SHMEMX_TEAM_INVALID)
        return -1;

    shmem_internal_team_t *team_dest = (shmem_internal_team_t *)dest_team;
    shmem_internal_team_t *team_src  = (shmem_internal_team_t *)src_team;

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
                                      int PE_size, const shmemx_team_config_t *config, long config_mask,
                                      shmem_internal_team_t **new_team)
{

    *new_team = SHMEMX_TEAM_INVALID;

    if (PE_size <= 0 || PE_stride < 1 || PE_start < 0)
        return -1;

    if (parent_team == SHMEMX_TEAM_INVALID) {
        return 0;
    }

    shmem_internal_team_t *myteam = calloc(1, sizeof(shmem_internal_team_t));

    myteam->start       = PE_start;
    myteam->stride      = PE_stride;
    myteam->size        = PE_size;
    if (config) {
        myteam->config      = *config;
        myteam->config_mask = config_mask;
    }
    myteam->contexts_len = 0;

    if (shmem_internal_pe_in_active_set(shmem_internal_my_pe, PE_start, PE_stride,
                                        PE_size, &myteam->my_pe)) {
        //TODO: will we need a pool of pWrk arrays?

        shmem_internal_op_to_all(psync_pool_avail_reduced,
                                 psync_pool_avail, 1, sizeof(uint64_t),
                                 PE_start, PE_stride, PE_size, NULL,
                                 &shmem_internal_psync_pool[parent_team->psync_idx*SHMEM_SYNC_SIZE],
                                 SHM_INTERNAL_BAND, SHM_INTERNAL_UINT64);

        /* Select the least signficant nonzero bit, which corresponds to an available pSync. */
        myteam->psync_idx = shmem_internal_1st_nonzero_bit(psync_pool_avail_reduced, sizeof(uint64_t));
        if (myteam->psync_idx == -1) {
            RAISE_ERROR_MSG("No more teams available (max = %ld), try increasing SHMEM_TEAMS_MAX\n",
                            shmem_internal_params.TEAMS_MAX);
        }

        /* Set the selected psync bit to 0 */
        *psync_pool_avail ^= (uint64_t)1 << myteam->psync_idx;

        *new_team = myteam;

        shmem_internal_team_pool[myteam->psync_idx] = *new_team;
    }

    shmem_internal_barrier(parent_team->start, parent_team->stride, parent_team->size,
                           &shmem_internal_psync_barrier_pool[parent_team->psync_idx * SHMEM_SYNC_SIZE]);
    return 0;
}

int shmem_internal_team_split_2d(shmem_internal_team_t *parent_team, int xrange,
                                 const shmemx_team_config_t *xaxis_config, long xaxis_mask,
                                 shmem_internal_team_t **xaxis_team, const shmemx_team_config_t *yaxis_config,
                                 long yaxis_mask, shmem_internal_team_t **yaxis_team)
{
    const int parent_start = parent_team->start;
    const int parent_stride = parent_team->stride;
    const int parent_size = parent_team->size;
    const int num_xteams = ceil( parent_size / (float)xrange );
    const int num_yteams = xrange;

    int start = parent_start;
    int ret = 0;

    for (int i = 0; i < num_xteams; i++) {
	int xsize = (i == num_xteams - 1 && parent_size % xrange) ? parent_size % xrange : xrange;

        if (shmem_internal_pe_in_active_set(shmem_internal_my_pe, start,
                                            parent_stride, xsize, NULL)) {
            ret = shmem_internal_team_split_strided(parent_team, start, parent_stride,
                                            xsize, xaxis_config, xaxis_mask, xaxis_team);
            if (ret) {
                RAISE_ERROR_STR("x-axis 2D strided split failed");
            }
        }
        start += xrange * parent_stride;
    }

    start = parent_start;

    for (int i = 0; i < num_yteams; i++) {
        int remainder = parent_size % xrange;
        int yrange = parent_size / xrange;
        int ysize = (remainder && i < remainder) ? yrange + 1 : yrange;

        if (shmem_internal_pe_in_active_set(shmem_internal_my_pe, start,
                                            xrange*parent_stride, ysize, NULL)) {
            ret = shmem_internal_team_split_strided(parent_team, start, xrange*parent_stride,
                                            ysize, yaxis_config, yaxis_mask, yaxis_team);
            if (ret) {
                RAISE_ERROR_STR("y-axis 2D strided split failed");
            }
        }
        start += parent_stride;
    }

    shmem_internal_barrier(parent_start, parent_stride, parent_size,
                           &shmem_internal_psync_pool[parent_team->psync_idx * SHMEM_SYNC_SIZE]);
    return 0;
}

int shmem_internal_team_destroy(shmem_internal_team_t **team)
{

    if (*team == SHMEMX_TEAM_INVALID || *team == &shmem_internal_team_world ||
        *team == &shmem_internal_team_shared || *team == &shmem_internal_team_host ||
        *team == &shmem_internal_team_leaders) {
        return -1;
    } else if ((*psync_pool_avail >> (*team)->psync_idx) & (uint64_t)1) {
        RAISE_WARN_STR("Destroying a team without an active pSync");
    } else {
        /* Is it necessary to reset the psync values? */
        for (size_t i = 0; i < SHMEM_SYNC_SIZE; i++) {
            shmem_internal_psync_pool[(*team)->psync_idx * SHMEM_SYNC_SIZE + i] = SHMEM_SYNC_VALUE;
            shmem_internal_psync_barrier_pool[(*team)->psync_idx  * SHMEM_SYNC_SIZE + i] = SHMEM_SYNC_VALUE;
        }
        /* Set the the psync bit back to 1 */
        *psync_pool_avail ^= (uint64_t)1 << (*team)->psync_idx;
    }

    /* Destroy all undestroyed shareable contexts on this team */
    for (size_t i = 0; i < (*team)->contexts_len; i++) {
        if ((*team)->contexts[i] != NULL) {
            if ((*team)->contexts[i]->options & SHMEM_CTX_PRIVATE)
                RAISE_WARN_MSG("Shutting down with unfreed private context (%zu)\n", i);
            shmem_transport_quiet((*team)->contexts[i]);
            shmem_transport_ctx_destroy(shmem_internal_team_world.contexts[i]);
        }
    }
    free((*team)->contexts);
    free(*team);
    shmem_internal_team_pool[(*team)->psync_idx] = NULL;

    return 0;
}

int shmem_internal_team_create_ctx(shmem_internal_team_t *team, long options, shmem_ctx_t *ctx)
{
    int ret = shmem_transport_ctx_create(team, options, (shmem_transport_ctx_t **) ctx);
    SHMEM_ERR_CHECK_NULL(ctx, 0);
    return ret;
}

int shmem_internal_ctx_get_team(shmem_ctx_t ctx, shmem_internal_team_t **team)
{
    shmem_transport_ctx_t *ctxp = (shmem_transport_ctx_t *)ctx;
    *team = ctxp->team;
    return 0;
}

