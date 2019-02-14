/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef SHMEM_INTERNAL_RUNTIME_H
#define SHMEM_INTERNAL_RUNTIME_H

#include "config.h"
#include "shmem_decl.h"

int shmem_runtime_init(int enable_node_ranks);
int shmem_runtime_fini(void);
void shmem_runtime_abort(int exit_code, const char msg[]) SHMEM_ATTRIBUTE_NORETURN ;

int shmem_runtime_get_rank(void);
int shmem_runtime_get_size(void);

/* Note: "node" API queries for PEs are on the caller's node.  Being on the
 * same node does not guarantee that a given PE is reachable via shared memory
 * (use shmem_internal_get_shr_rank/size for such queries). */
int shmem_runtime_get_node_rank(int pe);
int shmem_runtime_get_node_size(void);

int shmem_runtime_exchange(void);
int shmem_runtime_put(char *key, void *value, size_t valuelen);
int shmem_runtime_get(int pe, char *key, void *value, size_t valuelen);

void shmem_runtime_barrier(void);

/* Utility functions used to implement the runtime layer */
int shmem_runtime_util_put_hostname(void);
int shmem_runtime_util_populate_node(int *location_array, int size, int *node_size);

int shmem_runtime_util_encode(const void *inval, int invallen, char *outval, int outvallen);
int shmem_runtime_util_decode(const char *inval, void *outval, size_t outvallen);
#endif
