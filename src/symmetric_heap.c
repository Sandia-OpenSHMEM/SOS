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

#include <stdlib.h>
#include <stdio.h>
#include <sys/mman.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"

static char *shmem_internal_heap_curr = NULL;
static int shmem_internal_use_malloc = 0;

void* shmem_internal_get_next(int incr);
void* dlmalloc(size_t);
void  dlfree(void*);
void* dlrealloc(void*, size_t);
void* dlmemalign(size_t, size_t);

/* shmalloc and friends are defined to not be thread safe, so this is
   fine.  If they change that definition, this is no longer fine and
   needs to be made thread safe. */
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


/* alloc VM space starting @ '_end' + 1GB */
#define ONEGIG (1024UL*1024UL*1024UL)
static void *mmap_alloc(size_t bytes)
{
    void *requested_base = 
        (void*) (((unsigned long) shmem_internal_data_base + 
                  shmem_internal_data_length + 2 * ONEGIG) & ~(ONEGIG - 1));
    void *ret;

    ret = mmap(requested_base,
               bytes,
               PROT_READ | PROT_WRITE,
               MAP_ANON | MAP_PRIVATE,
               0,
               0);
    if (ret == MAP_FAILED) {
        perror("mmap()");
    }
    return ret;
}


int
shmem_internal_symmetric_init(size_t requested_length, int use_malloc)
{
    shmem_internal_use_malloc = use_malloc;

    /* add library overhead such that the max can be shmalloc()'ed */
    shmem_internal_heap_length = requested_length + (1024*1024);

    if (0 == shmem_internal_use_malloc) {
        shmem_internal_heap_base =
            shmem_internal_heap_curr = 
            mmap_alloc(shmem_internal_heap_length);
    } else {
        shmem_internal_heap_base = 
            shmem_internal_heap_curr = 
            malloc(shmem_internal_heap_length);
    }

    return (NULL == shmem_internal_heap_base) ? -1 : 0;
}


int
shmem_internal_symmetric_fini(void)
{
    if (NULL != shmem_internal_heap_base) {
        if (0 == shmem_internal_use_malloc) {
            munmap( (void*)shmem_internal_heap_base, (size_t)shmem_internal_heap_length );
        } else {
            free(shmem_internal_heap_base);
        }
        shmem_internal_heap_length = 0;
        shmem_internal_heap_base = shmem_internal_heap_curr = NULL;
    }

    return 0;
}


void*
shmalloc_init(size_t size)
{
    return dlmalloc(size);
}


void *
shmalloc(size_t size)
{
    void *ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = dlmalloc(size);

    shmem_barrier_all();

    return ret;
}


void
shfree(void *ptr)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    dlfree(ptr);

    shmem_barrier_all();
}


void *
shrealloc(void *ptr, size_t size)
{
    void *ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = dlrealloc(ptr, size);

    shmem_barrier_all();

    return ret;
}


void *
shmemalign(size_t alignment, size_t size)
{
    void *ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = dlmemalign(alignment, size);

    shmem_barrier_all();

    return ret;
}
