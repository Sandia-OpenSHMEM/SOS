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

#ifndef SHMEM_REMOTE_POINTER_H
#define SHMEM_REMOTE_POINTER_H

#include "shmem_comm.h"
#include "shmem_node_util.h"


static inline void *
shmem_internal_ptr(const void *target, int pe)
{
    int node_rank;

    // Only if regular load/stores are used to implement put/get!
    if (-1 != (node_rank = shmem_runtime_get_local_rank(pe))) {
#if USE_XPMEM
        return shmem_transport_xpmem_ptr(target, pe, node_rank);
#else
        return NULL;
#endif
    } else {
        return NULL;
    }
}


#endif /* #ifndef SHMEM_REMOTE_POINTER_H */
