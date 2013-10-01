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

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_accessibility.h"

#ifdef ENABLE_PROFILING

#pragma weak shmem_pe_accessible = pshmem_pe_accessible
#define shmem_pe_accessible pshmem_pe_accessible

#pragma weak shmem_addr_accessible = pshmem_addr_accessible
#define shmem_addr_accessible pshmem_addr_accessible

#endif /* ENABLE_PROFILING */

int 
shmem_pe_accessible(int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_pe_accessible(pe);
}


int
shmem_addr_accessible(void *addr, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_addr_accessible(addr, pe);
}
