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

int shmem_node_util_is_initialized(void);

int shmem_node_util_init(void);

int shmem_node_util_get_rank_same_node(int pe);

void shmem_node_util_set_node_rank(int pe, int node_rank);

int shmem_node_util_pe_on_same_node(int pe);

int shmem_node_util_count_local_pes(void);

#endif
