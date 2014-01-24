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

#pragma weak shmem_init = pshmem_init
#define shmem_init pshmem_init

#pragma weak shmem_finalize = pshmem_finalize
#define shmem_finalize pshmem_finalize

#endif /* ENABLE_PROFILING */


void
start_pes(int npes)
{
    int tl_provided;

    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init(SHMEM_THREAD_SINGLE, &tl_provided);
}


char *
shmem_nodename(void)
{
    return shmem_internal_nodename();
}


void shmem_init(int tl_requested, int *tl_provided)
{
    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init(tl_requested, tl_provided);
}


void shmem_finalize(void)
{
    shmem_internal_finalize();
}
