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

int shmem_runtime_init(void);
int shmem_runtime_fini(void);
void shmem_runtime_abort(int exit_code, const char msg[]) SHMEM_ATTRIBUTE_NORETURN ;

int shmem_runtime_get_rank(void);
int shmem_runtime_get_size(void);

/* "local" rank and size do not depend on the comms method used (i.e., the
 * rank and size are independent of USE_ON_NODE_COMMS and USE_MEMCPY).  */
int shmem_runtime_get_local_rank(int pe);
int shmem_runtime_get_local_size(void);

/* "node" rank and size depend on which comms method is used.  */
static inline
int
shmem_runtime_get_node_rank(int pe)
{
#ifdef USE_ON_NODE_COMMS
    return shmem_runtime_get_local_rank(pe);
#elif defined(USE_MEMCPY)
    return pe == myproc.rank ? 0 : -1;
#else
    return -1;
#endif
}

static inline
int
shmem_runtime_get_node_size(void)
{
#ifdef USE_ON_NODE_COMMS
    return shmem_runtime_get_local_size();
#elif defined(USE_MEMCPY)
    return 1;
#else
    return 0;
#endif
}

int shmem_runtime_exchange(int need_node_util);
int shmem_runtime_put(char *key, void *value, size_t valuelen);
int shmem_runtime_get(int pe, char *key, void *value, size_t valuelen);

void shmem_runtime_barrier(void);

#endif
