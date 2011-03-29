/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
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


void *
shmem_ptr(void *target, int pe)
{
    return NULL;
}
