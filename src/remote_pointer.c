/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Portals SHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#include <portals4.h>
#include <stdio.h>
#include <stdlib.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"

void *
shmem_ptr(void *target, int pe)
{
    int node_rank;

    // Only if regular load/stores are used to implement put/get!
    if (-1 != (node_rank = SHMEM_GET_RANK_SAME_NODE(pe))) {
#if USE_XPMEM
        return shmem_transport_xpmem_ptr(target, pe, node_rank);
#else
        return NULL;
#endif
    } else {
        return NULL;
    }
}
