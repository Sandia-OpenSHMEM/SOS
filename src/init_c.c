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

#include "shmem.h"
#include "shmem_internal.h"

#ifdef ENABLE_PROFILING

#pragma weak start_pes = pstart_pes
#define start_pes pstart_pes

#pragma weak shmem_nodename = pshmem_nodename
#define shmem_nodename pshmem_nodename

#endif /* ENABLE_PROFILING */


void
start_pes(int npes)
{
    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init();
}


char *
shmem_nodename(void)
{
    return shmem_internal_nodename();
}
