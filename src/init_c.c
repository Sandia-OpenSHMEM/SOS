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

#pragma weak shmemx_nodename = pshmemx_nodename
#define shmemx_nodename pshmemx_nodename

#pragma weak shmemx_init = pshmemx_init
#define shmemx_init pshmemx_init

#pragma weak shmemx_finalize = pshmemx_finalize
#define shmemx_finalize pshmemx_finalize

#endif /* ENABLE_PROFILING */


void
start_pes(int npes)
{
    int tl_provided;

    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init(SHMEMX_THREAD_SINGLE, &tl_provided);
}


char *
shmemx_nodename(void)
{
    return shmem_internal_nodename();
}


void shmemx_init(int tl_requested, int *tl_provided)
{
    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init(tl_requested, tl_provided);
}


void shmemx_finalize(void)
{
    shmem_internal_finalize();
}
