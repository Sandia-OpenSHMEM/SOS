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
#endif

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"

static char *shmem_internal_heap_curr = NULL;

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
                RAISE_WARN_STR("snprintf returned error, cannot use huge pages");
            } else {
                file_name = malloc(size + 1);
                if (file_name) {
                    sprintf(file_name, "%s/%s.%d", directory, basename, getpid());
                    fd = open(file_name, O_CREAT | O_RDWR, 0755);
                    if (fd < 0) {
                        RAISE_WARN_STR("file open failed, cannot use huge pages");
                        fd = 0;
                    } else {
                        /* have to round up by the pagesize being used */
                        bytes = CEILING(bytes, shmem_internal_params.SYMMETRIC_HEAP_PAGE_SIZE);
                    }
                }
            }
        }
    }
#endif /* __linux__ */

    ret = mmap(requested_base,
               bytes,
               PROT_READ | PROT_WRITE,
               MAP_ANON | MAP_PRIVATE,
               fd,
               0);
    if (ret == MAP_FAILED) {
        RAISE_WARN_MSG("Unable to allocate sym. heap, size %zuB: %s\n"
                       RAISE_PE_PREFIX
                       "Try reducing SHMEM_SYMMETRIC_SIZE or number of PEs per node\n",
                       bytes, strerror(errno), shmem_internal_my_pe);
        ret = NULL;
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

    if (!shmem_internal_params.SYMMETRIC_HEAP_USE_MALLOC) {
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
        if (!shmem_internal_params.SYMMETRIC_HEAP_USE_MALLOC) {
            munmap( (void*)shmem_internal_heap_base, (size_t)shmem_internal_heap_length );
        } else {
            free(shmem_internal_heap_base);
        }
        shmem_internal_heap_length = 0;
        shmem_internal_heap_base = shmem_internal_heap_curr = NULL;
    }

    return 0;
}
