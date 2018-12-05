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

#include <unistd.h>
#include "shmem_internal.h"
#include "transport.h"

extern int *shmem_internal_location_array;

int shmem_node_util_is_initialized(void);

int shmem_node_util_init(void);

void shmem_node_util_fini(void);

int shmem_node_util_startup(void);

int shmem_node_util_get_local_size(void);

struct shmem_node_util_addr_t {
    size_t addrlen;
    char *addr;
};

typedef struct shmem_node_util_addr_t shmem_node_util_addr_t;


static inline
int shmem_node_util_same_node(shmem_node_util_addr_t *a1, shmem_node_util_addr_t *a2)
{
    if (a1->addrlen == a2->addrlen &&
        memcmp(a1->addr, a2->addr, a1->addrlen) == 0) {
        return 1;
    } else {
        return 0;
    }
}

static inline
int shmem_node_util_gethostname(char *hostname)
{
    int ret = gethostname(hostname, SHMEM_INTERNAL_MAX_HOSTNAME_LEN);
    if (ret != 0) {
        RETURN_ERROR_MSG("gethostname failed (%d)", ret);
        return ret;
    }
    /* gethostname() doesn't guarantee null-termination if truncation occurs */
    hostname[SHMEM_INTERNAL_MAX_HOSTNAME_LEN - 1] = '\0';
    return ret;
}


static inline
int shmem_node_util_get_local_rank(int pe)
{
    shmem_internal_assert(pe < shmem_internal_num_pes && pe >= 0 &&
                          shmem_node_util_get_local_size() > 0);
    return shmem_internal_location_array[pe];
}

static inline
void shmem_node_util_set_local_rank(int pe, int node_rank)
{
    shmem_internal_location_array[pe] = node_rank;
    return;
}

#endif
