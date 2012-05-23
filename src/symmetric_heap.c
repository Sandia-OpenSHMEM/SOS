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

#define USE_MMAP 1

#if USE_MMAP

static void *mmap_alloc(long bytes);

#include <sys/mman.h>
#ifdef __APPLE__
#include <mach-o/getsect.h>
#endif

#define AGIG (1024UL*1024UL*1024UL)

/* alloc VM space starting @ '_end' + 1GB */
static void *mmap_alloc(long bytes)
{
    unsigned long base;
    void *symHeap;
#ifdef __APPLE__
    char *init_end = (char*) get_end();
#else
    extern char * _end;
    char *init_end = _end;
#endif

    base = (((unsigned long)init_end) + 2*AGIG) & ~(AGIG -1);  // round to next GB boundary.
    symHeap = mmap( (void*) base,
                    (size_t) bytes,
                    PROT_READ|PROT_WRITE,
                    MAP_ANON | MAP_PRIVATE,
                    -1,
                    0 );
    if (symHeap == MAP_FAILED) {
        perror("mmap()");
        symHeap = NULL;
    }
    return symHeap;
}
#endif


/* atol() + optional scaled suffix recognition: 1K, 2M, 3G, 1T */
static long
atol_scaled(char *s)
{
    long val;
    char *e;

    val = strtol(s,&e,0);
    if (e == NULL || *e =='\0')
        return val;

    if (*e == 'K')
        val *= 1024L;
    else if (*e == 'M')
        val *= 1024L*1024L;
    else if (*e == 'G')
        val *= 1024L*1024L*1024L;
    else if (*e == 'T')
        val *= 1024L*1024L*1024L*1024L;

    return val;
}

int
shmem_internal_symmetric_init(void)
{
    long req_len = 64 * 1024 * 1024;
    char *env = getenv("SHMEM_SYMMETRIC_HEAP_SIZE");

    if ( !env )
        env = getenv("SMA_SYMMETRIC_SIZE");

    if (NULL != env) req_len = atol_scaled(env);

    /* add library overhead such that the max can be shmalloc()'ed */
    shmem_internal_heap_length = req_len + (1024*1024);
#if USE_MMAP
    shmem_internal_heap_base =
    shmem_internal_heap_curr = mmap_alloc(shmem_internal_heap_length);
#else
    shmem_internal_heap_base = shmem_internal_heap_curr = malloc(shmem_internal_heap_length);
#endif
    if (NULL == shmem_internal_heap_base)  return -1;

    return 0;
}

int
shmem_internal_symmetric_fini(void)
{
    if (NULL != shmem_internal_heap_base) {
#if USE_MMAP
        munmap( (void*)shmem_internal_heap_base, (size_t)shmem_internal_heap_length );
#else
        free(shmem_internal_heap_base);
#endif
        shmem_internal_heap_length = 0;
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

    return dlmemalign(alignment, size);
}
