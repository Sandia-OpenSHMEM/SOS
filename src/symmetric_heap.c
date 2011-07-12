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
#include <stdlib.h>
#include <stdio.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"

long malloc_error = 0;

void *shmem_internal_heap_base = NULL;
long shmem_internal_heap_length = 0;
static char *shmem_internal_heap_curr = NULL;

void* shmem_internal_get_next(int incr);
void* dlmalloc(size_t);
void  dlfree(void*);
void* dlrealloc(void*, size_t);
void* dlmemalign(size_t, size_t);

void*
shmem_internal_get_next(int incr)
{
    char *orig = shmem_internal_heap_curr;
    shmem_internal_heap_curr += (incr > 0) ? incr : 0;
    if (shmem_internal_heap_curr - (char*) shmem_internal_heap_base > shmem_internal_heap_length) {
        printf("WARNING: top of symmetric heap found\n");
        return (void*) -1;
    }
    return orig;
}

int
shmem_internal_symmetric_init(void)
{
    long req_len = 64 * 1024 * 1024;
    char *env = getenv("SHMEM_SYMMETRIC_HEAP_SIZE");
    if (NULL != env) req_len = atoi(env);

    shmem_internal_heap_length = req_len;
    shmem_internal_heap_base = shmem_internal_heap_curr = malloc(shmem_internal_heap_length);
    if (NULL == shmem_internal_heap_base)  return -1;

    return 0;
}

int
shmem_internal_symmetric_fini(void)
{
    if (NULL != shmem_internal_heap_base) {
        shmem_internal_heap_length = 0;
        free(shmem_internal_heap_base);
        shmem_internal_heap_base = shmem_internal_heap_curr = NULL;
    }

    return 0;
}

void *
shmalloc(size_t size)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    malloc_error = 0;
    return dlmalloc(size);
}


void
shfree(void *ptr)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    malloc_error = 0;
    dlfree(ptr);
}


void *
shrealloc(void *ptr, size_t size)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    malloc_error = 0;
    return dlrealloc(ptr, size);
}


void *
shmemalign(size_t alignment, size_t size)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    malloc_error = 0;
    return dlmemalign(alignment, size);
}
