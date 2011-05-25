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

#include "mpp/shmem.h"


void
shmem_clear_cache_inv(void)
{
    /* Intentionally a no-op */
}


void
shmem_set_cache_inv(void)
{
    /* Intentionally a no-op */
}


void
shmem_clear_cache_line_inv(void *target)
{
    /* Intentionally a no-op */
}


void
shmem_set_cache_line_inv(void *target)
{
    /* Intentionally a no-op */
}


void
shmem_udcflush(void)
{
    /* Intentionally a no-op */
}


void
shmem_udcflush_line(void *target)
{
    /* Intentionally a no-op */
}
