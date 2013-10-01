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
#include <sys/time.h>

#include "shmem.h"
#include "shmem_internal.h"

#ifdef ENABLE_PROFILING

#pragma weak _num_pes = p_num_pes
#define _num_pes p_num_pes

#pragma weak shmem_n_pes = pshmem_n_pes
#define shmem_n_pes pshmem_n_pes

#pragma weak _my_pe = p_my_pe
#define _my_pe p_my_pe

#pragma weak shmem_my_pe = pshmem_my_pe
#define shmem_my_pe pshmem_my_pe

#pragma weak shmem_wtime = pshmem_wtime
#define shmem_wtime pshmem_wtime

#endif /* ENABLE_PROFILING */


int
_num_pes(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_num_pes;
}


int
shmem_n_pes(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_num_pes;
}


int
_my_pe(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_my_pe;
}


int
shmem_my_pe(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_my_pe;
}


double
shmem_wtime(void)
{
    double wtime;
    struct timeval tv;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
    return wtime;
}
