/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include <portals4.h>
#include <stdlib.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"

long malloc_error = 0;

void *shmem_heap_base = NULL;
long shmem_heap_length = 0;

int
symmetric_init(void)
{
    long req_len = 64 * 1024 * 1024;
    char *env = getenv("SHMEM_SYMMETRIC_HEAP_SIZE");
    if (NULL != env) req_len = atoi(env);

    shmem_heap_length = req_len;
    shmem_heap_base = malloc(shmem_heap_length);
    if (NULL == shmem_heap_base)  return -1;

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
