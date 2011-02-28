/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include <portals4.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"

long malloc_error = 0;

int
symmetric_init(void)
{
    /* BWB: Unimplemented */
    return 0;
}

void *
shmalloc(size_t size)
{
    /* BWB: Unimplemented */
    return NULL;
}


void
shfree(void *ptr)
{
    /* BWB: Unimplemented */
}


void *
shrealloc(void *ptr, size_t size)
{
    /* BWB: Unimplemented */
    return NULL;
}


void *
shmemalign(size_t alignment, size_t size)
{
    /* BWB: Unimplemented */
    return NULL;
}
