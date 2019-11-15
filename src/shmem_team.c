/* -*- C -*-
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

#define N_PSYNC_BYTES             8
#define PSYNC_CHUNK_SIZE          (N_PSYNCS_PER_TEAM * SHMEM_SYNC_SIZE)


shmem_internal_team_t shmem_internal_team_world;
shmemx_team_t SHMEMX_TEAM_WORLD = (shmemx_team_t) &shmem_internal_team_world;

shmem_internal_team_t shmem_internal_team_shared;
shmemx_team_t SHMEMX_TEAM_SHARED = (shmemx_team_t) &shmem_internal_team_shared;

shmem_internal_team_t **shmem_internal_team_pool;
long *shmem_internal_psync_pool;
long *shmem_internal_psync_barrier_pool;
static unsigned char *psync_pool_avail;
static unsigned char *psync_pool_avail_reduced;


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

int shmem_internal_team_init(void)
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
    for (size_t i = 0; i < N_PSYNCS_PER_TEAM; i++)
        shmem_internal_team_world.psync_avail[i] = 1;
    SHMEMX_TEAM_WORLD = (shmemx_team_t) &shmem_internal_team_world;

    /* Initialize SHMEM_TEAM_SHARED */
    shmem_internal_team_shared.psync_idx     = SHMEMX_TEAM_SHARED_INDEX;
    shmem_internal_team_shared.my_pe         = shmem_internal_my_pe;
    shmem_internal_team_shared.config_mask   = 0;
    shmem_internal_team_shared.contexts_len  = 0;
    memset(&shmem_internal_team_shared.config, 0, sizeof(shmemx_team_config_t));
    for (size_t i = 0; i < N_PSYNCS_PER_TEAM; i++)
        shmem_internal_team_shared.psync_avail[i] = 1;
    SHMEMX_TEAM_SHARED = (shmemx_team_t) &shmem_internal_team_shared;

    if (shmem_internal_params.TEAM_SHARED_ONLY_SELF) {
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

    const unsigned long max_teams = shmem_internal_params.TEAMS_MAX;

    shmem_internal_team_pool = malloc(max_teams * sizeof(shmem_internal_team_t*));

    /* Allocate pSync pool, each with the maximum possible size requirement */
    /* Create two pSyncs per team for back-to-back collectives and one for barriers.
     * Array organization:
     *
     * [ (world) (shared) (team 1) (team 2) ...  (world) (shared) (team 1) (team 2) ... ]
     *  <----------- groups 1 & 2-------------->|<------------- group 3 ---------------->
     *  <--- (bcast, collect, reduce, etc.) --->|<------ (barriers and syncs) ---------->
     * */
    long psync_len = max_teams * (PSYNC_CHUNK_SIZE + SHMEM_SYNC_SIZE);
    shmem_internal_psync_pool = shmem_internal_shmalloc(sizeof(long) * psync_len);
    if (NULL == shmem_internal_psync_pool) return -1;

    for (long i = 0; i < psync_len; i++) {
        shmem_internal_psync_pool[i] = SHMEM_SYNC_VALUE;
    }

    /* Convenience pointer to the group-3 pSync array (for barriers and syncs): */
    shmem_internal_psync_barrier_pool = &shmem_internal_psync_pool[PSYNC_CHUNK_SIZE * max_teams];

    if (max_teams > N_PSYNC_BYTES * CHAR_BIT) {
        RAISE_ERROR_MSG("Requested %ld teams, but only %d are supported\n",
                         max_teams, N_PSYNC_BYTES * CHAR_BIT);
    }

    psync_pool_avail = shmem_internal_shmalloc(2 * N_PSYNC_BYTES);
    psync_pool_avail_reduced = &psync_pool_avail[N_PSYNC_BYTES];

    /* Initialize the psync bits to 1, making all slots available: */
    for (size_t i = 0; i < N_PSYNC_BYTES; i++) {
        psync_pool_avail[i] = ~0;
    }

    /* Set the bits for SHMEM_TEAM_WORLD and SHMEM_TEAM_SHARED to 0: */
    shmem_internal_bit_clear(psync_pool_avail, N_PSYNC_BYTES, SHMEMX_TEAM_WORLD_INDEX);
    shmem_internal_bit_clear(psync_pool_avail, N_PSYNC_BYTES, SHMEMX_TEAM_SHARED_INDEX);

    for (size_t i = 0; i < max_teams; i++) {
        shmem_internal_team_pool[i] = NULL;
    }
    shmem_internal_team_pool[SHMEMX_TEAM_WORLD_INDEX] = &shmem_internal_team_world;
    shmem_internal_team_pool[SHMEMX_TEAM_SHARED_INDEX] = &shmem_internal_team_shared;

    return 0;
}

void shmem_internal_team_fini(void)
{
    /* Destroy all undestroyed teams */
    for (long i = 0; i < shmem_internal_params.TEAMS_MAX; i++) {
        if (shmem_internal_team_pool[i] != NULL)
            shmem_internal_team_destroy(shmem_internal_team_pool[i]);
    }

    free(shmem_internal_team_pool);
    shmem_internal_free(shmem_internal_psync_pool);
    shmem_internal_free(psync_pool_avail);

    return;
}

int shmem_internal_team_translate_pe(shmem_internal_team_t *src_team, int src_pe,
                                     shmem_internal_team_t *dest_team)
{
    int src_pe_world, dest_pe = -1;

    if (src_team == SHMEMX_TEAM_INVALID || dest_team == SHMEMX_TEAM_INVALID)
        return -1;

    if (src_pe > src_team->size)
        return -1;

    src_pe_world = src_team->start + src_pe * src_team->stride;

    if (src_pe_world < src_team->start || src_pe_world >= shmem_internal_num_pes)
        return -1;

    dest_pe = shmem_internal_pe_in_active_set(src_pe_world, dest_team->start, dest_team->stride,
                                              dest_team->size);

    return dest_pe;
}

int shmem_internal_team_split_strided(shmem_internal_team_t *parent_team, int PE_start, int PE_stride,
                                      int PE_size, const shmemx_team_config_t *config, long config_mask,
                                      shmem_internal_team_t **new_team)
{

    *new_team = SHMEMX_TEAM_INVALID;

    if (parent_team == SHMEMX_TEAM_INVALID) {
        return 0;
    }

    int global_PE_start = shmem_internal_team_pe(parent_team, PE_start);
    int global_PE_end   = global_PE_start + PE_stride * (PE_size -1);

    if (PE_size <= 0 || PE_stride < 1 || global_PE_start >= shmem_internal_num_pes ||
        global_PE_end >= shmem_internal_num_pes) {
        RAISE_WARN_STR("Invalid start, stride, or size in team_split operation");
        return -1;
    }

    shmem_internal_team_t *myteam = calloc(1, sizeof(shmem_internal_team_t));

    myteam->start       = global_PE_start;
    myteam->stride      = PE_stride;
    myteam->size        = PE_size;
    if (config) {
        myteam->config      = *config;
        myteam->config_mask = config_mask;
    }
    myteam->contexts_len = 0;
    myteam->psync_idx = 0;

    myteam->my_pe = shmem_internal_pe_in_active_set(shmem_internal_my_pe, myteam->start, PE_stride, PE_size);

    if (myteam->my_pe != -1) {

        long *psync = shmem_internal_team_choose_psync(parent_team, REDUCE);

        shmem_internal_op_to_all(psync_pool_avail_reduced,
                                 psync_pool_avail, N_PSYNC_BYTES, 1,
                                 myteam->start, PE_stride, PE_size, NULL,
                                 psync, SHM_INTERNAL_BAND, SHM_INTERNAL_UCHAR);

        shmem_internal_team_release_psyncs(parent_team, REDUCE);

        /* Select the least signficant nonzero bit, which corresponds to an available pSync. */
        myteam->psync_idx = shmem_internal_bit_1st_nonzero(psync_pool_avail_reduced, N_PSYNC_BYTES);
        if (myteam->psync_idx == -1 || myteam->psync_idx >= shmem_internal_params.TEAMS_MAX) {
            RAISE_WARN_MSG("No more teams available (max = %ld), try increasing SHMEM_TEAMS_MAX\n",
                            shmem_internal_params.TEAMS_MAX);
            myteam->psync_idx = -1;
        } else {
            /* Set the selected psync bit to 0, reserving that slot */
            shmem_internal_bit_clear(psync_pool_avail, N_PSYNC_BYTES, myteam->psync_idx);

            for (size_t i = 0; i < N_PSYNCS_PER_TEAM; i++)
                myteam->psync_avail[i] = 1;

            *new_team = myteam;

            shmem_internal_team_pool[myteam->psync_idx] = *new_team;
        }
    }

    long *psync = shmem_internal_team_choose_psync(parent_team, SYNC);

    shmem_internal_barrier(parent_team->start, parent_team->stride, parent_team->size, psync);

    shmem_internal_team_release_psyncs(parent_team, SYNC);

    if (myteam->psync_idx == -1)
        return -1;
    else
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

        if (shmem_internal_pe_in_active_set(shmem_internal_my_pe, start, parent_stride, xsize) != -1) {
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

        if (shmem_internal_pe_in_active_set(shmem_internal_my_pe, start, xrange*parent_stride, ysize) != -1) {
            ret = shmem_internal_team_split_strided(parent_team, start, xrange*parent_stride,
                                            ysize, yaxis_config, yaxis_mask, yaxis_team);
            if (ret) {
                RAISE_ERROR_STR("y-axis 2D strided split failed");
            }
        }
        start += parent_stride;
    }

    long *psync = shmem_internal_team_choose_psync(parent_team, SYNC);

    shmem_internal_barrier(parent_start, parent_stride, parent_size, psync);

    shmem_internal_team_release_psyncs(parent_team, SYNC);

    return 0;
}

int shmem_internal_team_destroy(shmem_internal_team_t *team)
{

    if (team == SHMEMX_TEAM_INVALID || team == &shmem_internal_team_world ||
        team == &shmem_internal_team_shared) {
        return -1;
    } else if ((psync_pool_avail[team->psync_idx / CHAR_BIT] >> team->psync_idx) & 1) {
        RAISE_WARN_STR("Destroying a team without an active pSync");
    } else {
        for (size_t i = 0; i < PSYNC_CHUNK_SIZE; i++) {
            shmem_internal_psync_pool[team->psync_idx * PSYNC_CHUNK_SIZE+ i] = SHMEM_SYNC_VALUE;
        }
        for (size_t i = 0; i < SHMEM_SYNC_SIZE; i++) {
            shmem_internal_psync_barrier_pool[team->psync_idx * SHMEM_SYNC_SIZE + i] = SHMEM_SYNC_VALUE;
        }
        shmem_internal_bit_set(psync_pool_avail, N_PSYNC_BYTES, team->psync_idx);
    }

    /* Destroy all undestroyed shareable contexts on this team */
    for (size_t i = 0; i < team->contexts_len; i++) {
        if (team->contexts[i] != NULL) {
            if (team->contexts[i]->options & SHMEM_CTX_PRIVATE)
                RAISE_WARN_MSG("Shutting down with unfreed private context (%zu)\n", i);
            shmem_transport_quiet(team->contexts[i]);
            shmem_transport_ctx_destroy(shmem_internal_team_world.contexts[i]);
        }
    }
    free(team->contexts);
    free(team);
    shmem_internal_team_pool[team->psync_idx] = NULL;

    return 0;
}

/* Returns a psync from the given team that can be safely used for the
 * specified collective operation. */
long * shmem_internal_team_choose_psync(shmem_internal_team_t *team, shmem_internal_team_op_t op)
{

    switch (op) {
        case SYNC:
            return &shmem_internal_psync_barrier_pool[team->psync_idx * SHMEM_SYNC_SIZE];

        default:
            for (int i = 0; i < N_PSYNCS_PER_TEAM; i++) {
                if (team->psync_avail[i]) {
                    team->psync_avail[i] = 0;
                    return &shmem_internal_psync_pool[(team->psync_idx + i) * PSYNC_CHUNK_SIZE];
                }
            }

            size_t psync = team->psync_idx * SHMEM_SYNC_SIZE;
            shmem_internal_sync(team->start, team->stride, team->size,
                                &shmem_internal_psync_barrier_pool[psync]);

            for (int i = 0; i < N_PSYNCS_PER_TEAM; i++) {
                team->psync_avail[i] = 1;
            }
            team->psync_avail[0] = 0;

            return &shmem_internal_psync_pool[psync];
    }
}

void shmem_internal_team_release_psyncs(shmem_internal_team_t *team, shmem_internal_team_op_t op)
{
    switch (op) {
        case SYNC:
            for (size_t i = 0; i < N_PSYNCS_PER_TEAM; i++) {
                team->psync_avail[i] = 1;
            }
            break;
        default:
            break;
    }

    return;
}
