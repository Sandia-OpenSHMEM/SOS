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
}


void
shmem_set_cache_inv(void)
{
}


void
shmem_clear_cache_line_inv(void *target)
{
}


void
shmem_set_cache_line_inv(void *target)
{
}


void
shmem_udcflush(void)
{
}


void
shmem_udcflush_line(void *target)
{
}


void *
shmem_ptr(void *target, int pe)
{
    return target;
}
