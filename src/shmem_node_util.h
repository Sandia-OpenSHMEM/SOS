/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2018 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef SHMEM_NODE_UTIL_H
#define SHMEM_NODE_UTIL_H

#include <limits.h>
#include <sys/param.h>

#ifdef MAXHOSTNAMELEN
#define SHMEM_INTERNAL_MAX_HOSTNAME_LEN MAXHOSTNAMELEN
#else
#define SHMEM_INTERNAL_MAX_HOSTNAME_LEN HOST_NAME_MAX
#endif

#define SHMEM_INTERNAL_MAX_NPES_PER_NODE 255

extern int *shmem_internal_location_array;

int shmem_node_util_is_initialized(void);

int shmem_node_util_init(void);

void shmem_node_util_fini(void);

char* shmem_node_util_nodename(void);

int shmem_node_util_startup(void);

int shmem_node_util_n_local_pes(void);

static inline
int shmem_node_util_local_rank(int pe)
{
#ifdef USE_ON_NODE_COMMS
    return shmem_internal_location_array[pe];
#elif defined(USE_MEMCPY)
    return pe == shmem_internal_my_pe ? 0 : -1;
#else
    return -1;
#endif
}

static inline
void shmem_node_util_set_local_rank(int pe, int node_rank)
{
    shmem_internal_location_array[pe] = node_rank;
    return;
}

#endif
