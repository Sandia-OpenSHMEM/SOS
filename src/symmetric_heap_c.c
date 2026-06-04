/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <inttypes.h>
#include <errno.h>
#ifdef __linux__
#include <mntent.h>
#include <sys/vfs.h>
#ifndef MADV_COLLAPSE
#define MADV_COLLAPSE 25  /* synchronous THP promotion, Linux 5.18+ */
#endif
#endif
#include <dlfcn.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"
#include "shmem_collectives.h"
#include "shmemx.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_malloc = pshmem_malloc
#define shmem_malloc pshmem_malloc

#pragma weak shmem_calloc = pshmem_calloc
#define shmem_calloc pshmem_calloc

#pragma weak shmem_align = pshmem_align
#define shmem_align pshmem_align

#pragma weak shmem_realloc = pshmem_realloc
#define shmem_realloc pshmem_realloc

#pragma weak shmem_free = pshmem_free
#define shmem_free pshmem_free

#pragma weak shmalloc = pshmalloc
#define shmalloc pshmalloc

#pragma weak shmemalign = pshmemalign
#define shmemalign pshmemalign

#pragma weak shrealloc = pshrealloc
#define shrealloc pshrealloc

#pragma weak shfree = pshfree
#define shfree pshfree

#pragma weak shmem_malloc_with_hints = pshmem_malloc_with_hints
#define shmem_malloc_with_hints pshmem_malloc_with_hints

#pragma weak shmemx_heap_create = pshmemx_heap_create
#define shmemx_heap_create pshmemx_heap_create

#endif /* ENABLE_PROFILING */

static char *shmem_internal_heap_curr = NULL;

void* dlmalloc(size_t);
void* dlcalloc(size_t, size_t);
void  dlfree(void*);
void* dlrealloc(void*, size_t);
void* dlmemalign(size_t, size_t);


/*
 * scan /proc/mounts for a huge page file system with the
 * requested page size - on most Linux systems there will
 * only be a single 2MB pagesize present.
 * On success return 0, else -1.
 */

#ifdef __linux__
static int find_hugepage_dir(size_t page_size, char **directory)
{
    int ret = -1;
    struct statfs pg_size;
    struct mntent *mntent;
    FILE *fd;
    char *path;

    if (!directory || !page_size) {
        return ret;
    }

    fd = setmntent ("/proc/mounts", "r");
    if (fd == NULL) {
        return ret;
    }

    while ((mntent = getmntent(fd)) != NULL) {

        if (strcmp (mntent->mnt_type, "hugetlbfs") != 0) {
            continue;
        }

        path = mntent->mnt_dir;
        if (statfs(path, &pg_size) == 0) {
            if ((size_t) pg_size.f_bsize == page_size) {
                *directory = strdup(path);
                ret = 0;
                break;
            }
        }
    }

    endmntent(fd);
    return ret;
}
#endif /* __linux__ */

/* DSMML runtime support — loaded via dlopen when SHMEM_SYMMETRIC_HEAP_USE_DSMML=1.
 * Types mirror dsmml.h exactly so dlopen works without a build-time dependency. */
#ifdef __linux__

typedef enum {
    DSMML_RC_SUCCESS        = 0,
    DSMML_RC_FAILURE        = 1,
    DSMML_RC_INVALID_PARAM  = 2,
    DSMML_RC_RESOURCE_ERROR = 3,
    DSMML_RC_NO_MEMORY      = 4,
    DSMML_RC_UNKNOWN_FAIL   = 5,
    DSMML_RC_NOOP           = 6,
    DSMML_RC_MEM_OVERLAP    = 7,
    DSMML_RC_MEM_CORRUPT    = 8
} dsmml_return_t;

typedef enum { DSMML_MODE_DEFAULT = 0, DSMML_MODE_PREFERRED, DSMML_MODE_BIND,
               DSMML_MODE_INTERLEAVE } dsmml_mode_t;
typedef enum { DSMML_MEM_SYS_DEFAULT = 0, DSMML_MEM_NORMAL = 1,
               DSMML_MEM_FAST = 2, DSMML_MEM_CUSTOM = 3 } dsmml_type_t;
typedef enum {
    DSMML_HPSIZE_DEFAULT = 1,   /* THP */
    DSMML_HPSIZE_4K      = 2,
    DSMML_HPSIZE_2M      = 3,
    DSMML_HPSIZE_4M      = 4,
} dsmml_hpsize_t;
typedef enum { DSMML_NNODE_INDEX1 = 0x01 } dsmml_nnode_t;

typedef struct {
    int            mype;
    int            smp_mype;
    int            smp_npes;
    int            smp_set;
} dsmml_init_info_t;

typedef struct {
    int            id;
    void          *act_addr;
    void          *base_addr;
    size_t         length;
    dsmml_type_t   type;
    dsmml_hpsize_t pagesize;
    dsmml_mode_t   mode;
    int            smp_mype;
    int            smp_npes;
    int            smp_set;
    dsmml_nnode_t  nnode;
} dsmml_sheap_seg_info_t;

static void  *dsmml_handle             = NULL;
static dsmml_return_t (*dsmml_init_fn)(dsmml_init_info_t *)              = NULL;
static dsmml_return_t (*dsmml_finalize_fn)(void)                         = NULL;
static dsmml_return_t (*dsmml_create_sheap_seg_fn)(dsmml_sheap_seg_info_t *) = NULL;

static void *dsmml_alloc(void *requested_base, size_t bytes)
{
    const char *libname = "libdsmml.so";

    dsmml_handle = dlopen(libname, RTLD_NOW | RTLD_GLOBAL);
    if (!dsmml_handle) {
        RAISE_WARN_MSG("SHMEM_SYMMETRIC_HEAP_USE_DSMML=1 but dlopen(%s) failed: %s\n",
                       libname, dlerror());
        return NULL;
    }

    dsmml_init_fn             = dlsym(dsmml_handle, "dsmml_init");
    dsmml_finalize_fn         = dlsym(dsmml_handle, "dsmml_finalize");
    dsmml_create_sheap_seg_fn = dlsym(dsmml_handle, "dsmml_create_sheap_seg");

    if (!dsmml_init_fn || !dsmml_finalize_fn || !dsmml_create_sheap_seg_fn) {
        RAISE_WARN_MSG("DSMML: required symbols not found in %s: %s\n", libname, dlerror());
        dlclose(dsmml_handle);
        dsmml_handle = NULL;
        return NULL;
    }

    dsmml_init_info_t init_info = {
        .mype     = shmem_internal_my_pe,
        .smp_mype = shmem_runtime_get_node_rank(shmem_internal_my_pe),
        .smp_npes = shmem_internal_get_shr_size(),
        .smp_set  = 0,
    };
    if (dsmml_init_fn(&init_info) != DSMML_RC_SUCCESS) {
        RAISE_WARN_STR("DSMML: dsmml_init failed");
        dlclose(dsmml_handle);
        dsmml_handle = NULL;
        return NULL;
    }

    /* Try 2MB explicit huge pages first; fall back to THP on failure. */
    dsmml_sheap_seg_info_t seg = {
        .base_addr  = requested_base,
        .length     = bytes,
        .type       = DSMML_MEM_SYS_DEFAULT,
        .mode       = DSMML_MODE_DEFAULT,
        .pagesize   = DSMML_HPSIZE_2M,
        .smp_mype   = init_info.smp_mype,
        .smp_npes   = init_info.smp_npes,
        .smp_set    = 0,
    };

    if (dsmml_create_sheap_seg_fn(&seg) != DSMML_RC_SUCCESS) {
        DEBUG_STR("DSMML: 2MB huge pages unavailable, falling back to THP");
        seg.act_addr = NULL;
        seg.pagesize = DSMML_HPSIZE_DEFAULT;
        if (dsmml_create_sheap_seg_fn(&seg) != DSMML_RC_SUCCESS) {
            RAISE_WARN_STR("DSMML: dsmml_create_sheap_seg failed");
            dsmml_finalize_fn();
            dlclose(dsmml_handle);
            dsmml_handle = NULL;
            return NULL;
        }
    }

    DEBUG_MSG("DSMML: heap at %p (requested %p), %zu bytes, pagesize %s\n",
              seg.act_addr, requested_base, bytes,
              seg.pagesize == DSMML_HPSIZE_2M ? "2MB" : "THP");

    return seg.act_addr;
}

static void dsmml_free(void)
{
    if (dsmml_finalize_fn) dsmml_finalize_fn();
    if (dsmml_handle)      dlclose(dsmml_handle);
    dsmml_handle      = NULL;
    dsmml_finalize_fn = NULL;
}

#endif /* __linux__ */

/* shmalloc and friends are defined to not be thread safe, so this is
   fine.  If they change that definition, this is no longer fine and
   needs to be made thread safe. */
void*
shmem_internal_get_next(intptr_t incr)
{
    char *orig = shmem_internal_heap_curr;

    shmem_internal_heap_curr += incr;
    if (shmem_internal_heap_curr < (char*) shmem_internal_heap_base) {
        RAISE_WARN_STR("symmetric heap pointer pushed below start");
        shmem_internal_heap_curr = (char*) shmem_internal_heap_base;
    } else if (shmem_internal_heap_curr - (char*) shmem_internal_heap_base >
               shmem_internal_heap_length) {
        RAISE_WARN_MSG("Out of symmetric memory, heap size %ld, overrun %"PRIdPTR"\n"
                       RAISE_PE_PREFIX "Try increasing SHMEM_SYMMETRIC_SIZE\n",
                       shmem_internal_heap_length, incr, shmem_internal_my_pe);
        shmem_internal_heap_curr = orig;
        orig = (void*) -1;
    }

    return orig;
}

#ifndef FLOOR
#define FLOOR(a,b)      ((uint64_t)(a) - ( ((uint64_t)(a)) % (uint64_t)(b)))
#endif
#ifndef CEILING
#define CEILING(a,b)    ((uint64_t)(a) <= 0LL ? 0 : (FLOOR((a)-1,b) + (b)))
#endif


/* alloc VM space starting @ '_end' + 1GB */
#define ONEGIG (1024UL*1024UL*1024UL)
static void *mmap_alloc(size_t bytes)
{
    char *file_name = NULL;
    int fd = 0;
    char *directory = NULL;
    void *requested_base =
        (void*) (((unsigned long) shmem_internal_data_base +
                  shmem_internal_data_length + 2 * ONEGIG) & ~(ONEGIG - 1));
    void *ret;

#ifdef __linux__
    /* huge page support only on Linux for now, default is to use 2MB large pages */
    if (shmem_internal_params.SYMMETRIC_HEAP_USE_HUGE_PAGES) {
        const char basename[] = "hugepagefile.SOS";

        /* check what /proc/mounts has for explicit huge page support */
        if (find_hugepage_dir(shmem_internal_params.SYMMETRIC_HEAP_PAGE_SIZE,
                             &directory) == 0)
        {
            int size = snprintf(NULL, 0, "%s/%s.%d", directory, basename, getpid());

            if (size < 0) {
                DEBUG_STR("snprintf returned error, cannot use huge pages");
            } else {
                file_name = malloc(size + 1);
                if (file_name) {
                    sprintf(file_name, "%s/%s.%d", directory, basename, getpid());
                    fd = open(file_name, O_CREAT | O_RDWR, 0755);
                    if (fd < 0) {
                        DEBUG_MSG("file open failed (%s), cannot use huge pages",
                                  strerror(errno));
                        fd = 0;
                    } else {
                        /* have to round up by the pagesize being used */
                        bytes = CEILING(bytes, shmem_internal_params.SYMMETRIC_HEAP_PAGE_SIZE);
                    }
                }
            }
        }
    }

    if (fd) {
        /* Map the hugetlbfs file directly; MAP_ANON must not be used here
         * because MAP_ANONYMOUS causes the kernel to ignore the fd, which
         * would silently fall back to regular pages. */
        if (ftruncate(fd, bytes) == -1) {
            RAISE_WARN_MSG("ftruncate on hugetlbfs file failed (%s), "
                           "falling back to transparent huge pages via madvise\n",
                           strerror(errno));
            unlink(file_name);
            close(fd);
            free(directory);
            free(file_name);
            ret = mmap(requested_base, bytes, PROT_READ | PROT_WRITE,
                       MAP_ANON | MAP_PRIVATE, -1, 0);
            if (ret != MAP_FAILED) {
                if (madvise(ret, bytes, MADV_HUGEPAGE) != 0) {
                    DEBUG_MSG("madvise(MADV_HUGEPAGE) failed (%s), using regular pages",
                              strerror(errno));
                } else {
                    if (madvise(ret, bytes, MADV_COLLAPSE) != 0) {
                        DEBUG_MSG("madvise(MADV_COLLAPSE) failed (%s), THP promotion deferred",
                                  strerror(errno));
                    }
                }
            }
        } else {
            ret = mmap(requested_base, bytes, PROT_READ | PROT_WRITE,
                       MAP_SHARED | MAP_HUGETLB, fd, 0);
            unlink(file_name);
            close(fd);
            free(directory);
            free(file_name);
        }
    } else if (shmem_internal_params.SYMMETRIC_HEAP_USE_HUGE_PAGES) {
        /* Try anonymous MAP_HUGETLB first (works with nr_overcommit_hugepages) */
        ret = mmap(requested_base, bytes, PROT_READ | PROT_WRITE,
                   MAP_ANON | MAP_PRIVATE | MAP_HUGETLB, -1, 0);
        if (ret == MAP_FAILED) {
            DEBUG_MSG("mmap(MAP_HUGETLB) failed (%s), falling back to THP via madvise",
                      strerror(errno));
            ret = mmap(requested_base, bytes, PROT_READ | PROT_WRITE,
                       MAP_ANON | MAP_PRIVATE, -1, 0);
            if (ret != MAP_FAILED) {
                if (madvise(ret, bytes, MADV_HUGEPAGE) != 0) {
                    RAISE_WARN_MSG("madvise(MADV_HUGEPAGE) failed (%s), using regular pages\n",
                                   strerror(errno));
                } else {
                    if (madvise(ret, bytes, MADV_COLLAPSE) != 0) {
                        DEBUG_MSG("madvise(MADV_COLLAPSE) failed (%s), THP promotion deferred",
                                  strerror(errno));
                    }
                }
            }
        } else {
            DEBUG_MSG("Allocated symmetric heap with explicit huge pages (MAP_HUGETLB), %zu bytes",
                      bytes);
        }
    } else {
        ret = mmap(requested_base, bytes, PROT_READ | PROT_WRITE, MAP_ANON | MAP_PRIVATE, -1, 0);
    }
#else
    ret = mmap(requested_base, bytes, PROT_READ | PROT_WRITE,
               MAP_ANON | MAP_PRIVATE, -1, 0);
#endif /* __linux__ */
    if (ret == MAP_FAILED) {
        RAISE_WARN_MSG("Unable to allocate sym. heap, size %zuB: %s\n"
                       RAISE_PE_PREFIX
                       "Try reducing SHMEM_SYMMETRIC_SIZE or number of PEs per node\n",
                       bytes, strerror(errno), shmem_internal_my_pe);
        ret = NULL;
    } else if (ret != NULL) {
#ifdef __linux__
        /* Try transparent huge pages via madvise on non-Linux fallback path too */
        if (shmem_internal_params.SYMMETRIC_HEAP_USE_HUGE_PAGES) {
            if (madvise(ret, bytes, MADV_HUGEPAGE) != 0) {
                RAISE_WARN_MSG("madvise(MADV_HUGEPAGE) failed (%s), using regular pages\n",
                               strerror(errno));
            } else {
                if (madvise(ret, bytes, MADV_COLLAPSE) != 0) {
                    DEBUG_MSG("madvise(MADV_COLLAPSE) failed (%s), THP promotion deferred",
                              strerror(errno));
                }
            }
        }
#endif
    }
    if (fd) {
        if (file_name)
            unlink(file_name);
        close(fd);
    }
    if (directory) {
        free(directory);
    }
    if (file_name) {
        free(file_name);
    }
    return ret;
}


int
shmem_internal_symmetric_init(void)
{
    /* add library overhead such that the max can be shmalloc()'ed */
    shmem_internal_heap_length = shmem_internal_params.SYMMETRIC_SIZE +
                                 SHMEM_INTERNAL_HEAP_OVERHEAD;

    if (shmem_internal_params.SYMMETRIC_HEAP_USE_MALLOC) {
        shmem_internal_heap_base =
            shmem_internal_heap_curr =
            malloc(shmem_internal_heap_length);
#ifdef __linux__
    } else if (shmem_internal_params.SYMMETRIC_HEAP_USE_DSMML) {
        void *requested_base =
            (void*) (((unsigned long) shmem_internal_data_base +
                      shmem_internal_data_length + 2 * ONEGIG) & ~(ONEGIG - 1));
        shmem_internal_heap_base =
            shmem_internal_heap_curr =
            dsmml_alloc(requested_base, shmem_internal_heap_length);
#endif
    } else {
        shmem_internal_heap_base =
            shmem_internal_heap_curr =
            mmap_alloc(shmem_internal_heap_length);
    }

    return (NULL == shmem_internal_heap_base) ? -1 : 0;
}


int
shmem_internal_symmetric_fini(void)
{
    if (NULL != shmem_internal_heap_base) {
        if (shmem_internal_params.SYMMETRIC_HEAP_USE_MALLOC) {
            free(shmem_internal_heap_base);
#ifdef __linux__
        } else if (shmem_internal_params.SYMMETRIC_HEAP_USE_DSMML) {
            dsmml_free();
#endif
        } else {
            munmap( (void*)shmem_internal_heap_base, (size_t)shmem_internal_heap_length );
        }
        shmem_internal_heap_length = 0;
        shmem_internal_heap_base = shmem_internal_heap_curr = NULL;
    }

    return 0;
}


void*
shmem_internal_shmalloc(size_t size)
{
    void *ret;

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    ret = dlmalloc(size);
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

    return ret;
}


void SHMEM_FUNCTION_ATTRIBUTES *
shmem_malloc(size_t size)
{
    void *ret = NULL;

    SHMEM_ERR_CHECK_INITIALIZED();

    if (size == 0) return ret;

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    ret = dlmalloc(size);
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

    shmem_internal_barrier_all();

    return ret;
}

void SHMEM_FUNCTION_ATTRIBUTES *
shmem_calloc(size_t count, size_t size)
{
    void *ret = NULL;

    SHMEM_ERR_CHECK_INITIALIZED();

    if (size == 0 || count == 0) return ret;

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    ret = dlcalloc(count, size);
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

    shmem_internal_barrier_all();

    return ret;
}

void SHMEM_FUNCTION_ATTRIBUTES
shmem_free(void *ptr)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    if (ptr != NULL) {
      SHMEM_ERR_CHECK_SYMMETRIC_HEAP(ptr);
    }

    shmem_internal_barrier_all();

    shmem_internal_free(ptr);
}


void SHMEM_FUNCTION_ATTRIBUTES *
shmem_realloc(void *ptr, size_t size)
{
    void *ret;

    SHMEM_ERR_CHECK_INITIALIZED();

    if (size == 0 && ptr == NULL) return ptr;
    if (ptr != NULL) {
      SHMEM_ERR_CHECK_SYMMETRIC_HEAP(ptr);
    }

    shmem_internal_barrier_all();

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    if (size == 0 && ptr != NULL) {
        dlfree(ptr);
        ret = NULL;
    } else {
        ret = dlrealloc(ptr, size);
    }
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

    shmem_internal_barrier_all();

    return ret;
}


void SHMEM_FUNCTION_ATTRIBUTES *
shmem_align(size_t alignment, size_t size)
{
    void *ret = NULL;

    SHMEM_ERR_CHECK_INITIALIZED();

    if (size == 0) return ret;
    if (alignment == 0)
        return NULL;

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    ret = dlmemalign(alignment, size);
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

    shmem_internal_barrier_all();

    return ret;
}


/* The following functions were renamed in OpenSHMEM 1.2 and the old names were
 * deprecated.  Note that if PROFILING_ENABLED, the profiling macros will cause
 * these functions to directly call the corresponding pshmem_* routine which
 * should make the up-call invsible to a profiling tool.
 */

void SHMEM_FUNCTION_ATTRIBUTES * shmalloc(size_t size)
{
    return shmem_malloc(size);
}


void SHMEM_FUNCTION_ATTRIBUTES shfree(void *ptr)
{
    shmem_free(ptr);
}


void SHMEM_FUNCTION_ATTRIBUTES * shrealloc(void *ptr, size_t size)
{
    return shmem_realloc(ptr, size);
}


void SHMEM_FUNCTION_ATTRIBUTES * shmemalign(size_t alignment, size_t size)
{
    return shmem_align(alignment, size);
}


void SHMEM_FUNCTION_ATTRIBUTES *
shmem_malloc_with_hints(size_t size, long hints)
{
    void *ret = NULL;

    SHMEM_ERR_CHECK_INITIALIZED();

    if (size == 0) return ret;

    // Check for valid hints
    if(hints > SHMEM_MALLOC_MAX_HINTS || hints < 0) {
        RAISE_WARN_MSG("Ignoring invalid hint for shmem_malloc_with_hints(%ld)\n", hints);
    }

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    ret = dlmalloc(size);
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

    if (!(hints & SHMEMX_MALLOC_NO_BARRIER))
        shmem_internal_barrier_all();

    return ret;
}

void SHMEM_FUNCTION_ATTRIBUTES
shmemx_heap_create(void *base, size_t size, int device_type, int device_index) {

    if (shmem_internal_initialized) {
        RAISE_WARN_MSG("Ignoring pre-setup. Heap already initialized\n");
        return;
    }

    shmem_internal_assert(size > 0);
#ifndef USE_FI_HMEM
    shmem_internal_assert(device_index == -1);
#else
    shmem_internal_assert(device_index != -1);
#endif

    shmem_external_heap_base         = base;
    shmem_external_heap_length       = size;

    shmem_internal_assert(device_type == SHMEMX_EXTERNAL_HEAP_ZE ||
                          device_type == SHMEMX_EXTERNAL_HEAP_CUDA);

    shmem_external_heap_device_type  = device_type;
    shmem_external_heap_device       = device_index;

    shmem_external_heap_pre_initialized = 1;
}
