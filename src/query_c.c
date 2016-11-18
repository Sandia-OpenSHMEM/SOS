/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmemx.h"
#include "shmem_internal.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak _num_pes = p_num_pes
#define _num_pes p_num_pes

#pragma weak shmem_n_pes = pshmem_n_pes
#define shmem_n_pes pshmem_n_pes

#pragma weak _my_pe = p_my_pe
#define _my_pe p_my_pe

#pragma weak shmem_my_pe = pshmem_my_pe
#define shmem_my_pe pshmem_my_pe

#pragma weak shmemx_wtime = pshmemx_wtime
#define shmemx_wtime pshmemx_wtime

#endif /* ENABLE_PROFILING */


int
_num_pes(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    return shmem_internal_num_pes;
}


int
shmem_n_pes(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    return shmem_internal_num_pes;
}


int
_my_pe(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    return shmem_internal_my_pe;
}


int
shmem_my_pe(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    return shmem_internal_my_pe;
}


double
shmemx_wtime(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    return shmem_internal_wtime();
}
