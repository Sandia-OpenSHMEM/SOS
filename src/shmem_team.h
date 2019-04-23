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

#ifndef SHMEM_TEAM_H
#define SHMEM_TEAM_H

#define SHMEM_INTERNAL_INCLUDE
#include "shmemx.h"
#include "transport.h"
#include "uthash.h"


struct shmem_internal_team_t {
    //shmemx_team_t          team_handle;
    int                           team_id;
    struct shmem_internal_team_t  *parent_team;
    long*                         pSync;
    int                           start, stride, size;
    shmemx_team_config_t          config;
    long                          config_mask;
    UT_hash_handle                hh;
};
typedef struct shmem_internal_team_t shmem_internal_team_t;
static shmem_internal_team_t* shmem_internal_teams_kvs = NULL;

static int num_teams = 1;

static inline
int shmem_internal_teams_init() {

    shmem_internal_team_t *team_world = calloc(1, sizeof(shmem_internal_team_t));

    team_world->team_id     = -1;

    team_world->parent_team = team_world;

    team_world->start       = 0;
    team_world->stride      = 1;
    team_world->size        = shmem_internal_num_pes;
    memset(&team_world->config, 0, sizeof(shmemx_team_config_t));
    team_world->config_mask = 0;

    /* Allocate pSync with the maximum possible size requirement */
    team_world->pSync = shmem_internal_shmalloc(sizeof(long) * SHMEM_SYNC_SIZE);
    if (NULL == team_world->pSync) return -1;

    HASH_ADD_INT(shmem_internal_teams_kvs, team_id, team_world);

    SHMEMX_TEAM_WORLD = (shmemx_team_t) team_world;

    return 0;
}


static inline
int shmem_internal_team_my_pe(shmemx_team_t team)
{
    return -1;
}

static inline
int shmem_internal_team_n_pes(shmemx_team_t team)
{
    return -1;
}

static inline
void shmemx_internal_team_get_config(shmemx_team_t team, shmemx_team_config_t *config)
{
    return;
}

static inline
int shmemx_internal_team_translate_pe(shmemx_team_t src_team, int src_pe, shmemx_team_t dest_team)
{
    return -1;
}

static inline
int shmem_internal_team_split_strided(shmem_internal_team_t *parent_team, int PE_start, int PE_stride,
                                      int PE_size, shmemx_team_config_t *config, long config_mask,
                                      shmem_internal_team_t **new_team)
{
    *new_team = SHMEMX_TEAM_NULL;

    shmem_internal_team_t *myteam = calloc(1, sizeof(shmem_internal_team_t));

    myteam->team_id     = ++num_teams;

    myteam->parent_team = parent_team;

    myteam->start       = PE_start;
    myteam->stride      = PE_stride;
    myteam->size        = PE_size;
    if (config) {
        myteam->config      = *config;
        myteam->config_mask = config_mask;
    }

    /* Allocate pSync with the maximum possible size requirement */
    myteam->pSync = shmem_internal_shmalloc(sizeof(long) * SHMEM_SYNC_SIZE);
    if (NULL == myteam->pSync) return -1;
   
    //HASH_ADD(hh, shmem_internal_teams_kvs, team_handle, sizeof(shmemx_team_t), &myteam);
    HASH_ADD_INT(shmem_internal_teams_kvs, team_id, myteam);

    new_team = &myteam;

    return -1;
}

static inline
int shmemx_internal_team_split_2d(shmemx_team_t parent_team, int xrange, shmemx_team_config_t *xaxis_config, long xaxis_mask, shmemx_team_t *xaxis_team, shmemx_team_config_t *yaxis_config, long yaxis_mask, shmemx_team_t *yaxis_team)
{
    return -1;
}

static inline
int shmemx_internal_team_destroy(shmemx_team_t *team)
{
    return -1;
}

static inline
int shmemx_internal_team_create_ctx(shmemx_team_t team, long options, shmem_ctx_t *ctx)
{
    return -1;
}

static inline
int shmemx_internal_ctx_get_team(shmem_ctx_t ctx, shmemx_team_t *team)
{
    return -1;
}

#endif
