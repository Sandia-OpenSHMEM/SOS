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

#include "shmem.h"
#include "shmem_internal.h"


void
shmem_clear_cache_inv(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


void
shmem_set_cache_inv(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


void
shmem_clear_cache_line_inv(void *target)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


void
shmem_set_cache_line_inv(void *target)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


void
shmem_udcflush(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


void
shmem_udcflush_line(void *target)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}
