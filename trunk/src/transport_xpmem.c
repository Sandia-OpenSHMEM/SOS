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

#include <xpmem.h>
#include <unistd.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"
#include "runtime.h"

static xpmem_segid_t segs[2];

int
shmem_transport_xpmem_init(void)
{
    long page_size = sysconf(_SC_PAGESIZE);
    char *base;
    size_t len;
    int ret;

    /* setup data region */
    base = (char*) (((uintptr_t) shmem_internal_data_base / page_size) * 
                    page_size);
    len = 
        (((char*) shmem_internal_data_base - base + 
          shmem_internal_data_length - 1) / page_size + 1) * page_size;
    segs[0] = xpmem_make(base, len, XPMEM_PERMIT_MODE, (void*)0666);
    if (-1 == segs[0]) {
        fprintf(stderr, "[%03d] ERROR: xpmem_make failed: %d\n",
                shmem_internal_my_pe, ret);
        return 1;
    }

    /* setup heap region */
    base = (char*) (((uintptr_t) shmem_internal_heap_base / page_size) *
                    page_size);
    len = 
        (((char*) shmem_internal_heap_base - base + 
          shmem_internal_heap_length - 1) / page_size + 1) * page_size;
    segs[1] = xpmem_make(base, len, XPMEM_PERMIT_MODE, (void*)0666);
    if (-1 == segs[1]) {
        fprintf(stderr, "[%03d] ERROR: xpmem_make failed: %d\n",
                shmem_internal_my_pe, ret);
        return 1;
    }

    ret = shmem_runtime_put("xpmem-segids", segs, 
                            sizeof(xpmem_segid_t) * 2);
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: runtime_put failed: %d\n",
                shmem_internal_my_pe, ret);
        return 1;
    }

    return 0;
}


int
shmem_transport_xpmem_startup(void)
{
    return 0;
}


int
shmem_transport_xpmem_fini(void)
{
    return 0;
}

