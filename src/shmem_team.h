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


shmemx_team_t SHMEMX_TEAM_WORLD;

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
int shmem_internal_team_split_strided(shmemx_team_t parent_team, int PE_start, int PE_stride,
                                      int PE_size, shmemx_team_config_t *config, long config_mask,
                                      shmemx_team_t *new_team)
{
    new_team = SHMEMX_TEAM_NULL;
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
