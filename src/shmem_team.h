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

#include "shmemx.h"
#include "transport.h"
#include "uthash.h"


struct shmem_internal_team_t {
    int                   team_id;
    int                   my_pe;
    size_t                psync_idx;
    int                   start, stride, size;
    shmemx_team_config_t  config;
    long                  config_mask;
};
typedef struct shmem_internal_team_t shmem_internal_team_t;

extern shmem_internal_team_t shmem_internal_team_world;


int shmem_internal_teams_init(void);
void shmem_internal_teams_fini(void);

int shmem_internal_team_my_pe(shmemx_team_t team);

int shmem_internal_team_n_pes(shmemx_team_t team);

void shmem_internal_team_get_config(shmemx_team_t team, shmemx_team_config_t *config);

int shmem_internal_team_translate_pe(shmemx_team_t src_team, int src_pe, shmemx_team_t dest_team);

int shmem_internal_team_split_strided(shmem_internal_team_t *parent_team, int PE_start, int PE_stride,
                                      int PE_size, shmemx_team_config_t *config, long config_mask,
                                      shmem_internal_team_t **new_team);

int shmem_internal_team_split_2d(shmemx_team_t parent_team, int xrange,
                                  shmemx_team_config_t *xaxis_config, long xaxis_mask, shmemx_team_t *xaxis_team,
                                  shmemx_team_config_t *yaxis_config, long yaxis_mask, shmemx_team_t *yaxis_team);

int shmem_internal_team_destroy(shmemx_team_t *team);

int shmem_internal_team_create_ctx(shmemx_team_t team, long options, shmem_ctx_t *ctx);

int shmem_internal_ctx_get_team(shmem_ctx_t ctx, shmemx_team_t *team);

#endif
