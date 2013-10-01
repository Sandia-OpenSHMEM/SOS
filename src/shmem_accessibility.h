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

#ifndef SHMEM_ACCESSIBILITY_H
#define SHMEM_ACCESSIBILITY_H

#include "shmem_comm.h"

static inline int 
shmem_internal_pe_accessible(int pe)
{
    return (pe >= 0 && pe < shmem_internal_num_pes) ? 1 : 0;
}


static inline int
shmem_internal_addr_accessible(void *addr, int pe)
{
    if (pe < 0 || pe >= shmem_internal_num_pes) {
        return 0;
    }

    if ((char*) addr > (char*) shmem_internal_heap_base &&
        (char*) addr < (char*) shmem_internal_heap_base + shmem_internal_heap_length) {
        return 1;
    }
    if ((char*) addr > (char*) shmem_internal_data_base &&
        (char*) addr < (char*) shmem_internal_data_base + shmem_internal_data_length) {
        return 1;
    }

    return 0;
}


#endif /* #ifndef SHMEM_ACCESSIBILITY_H */
