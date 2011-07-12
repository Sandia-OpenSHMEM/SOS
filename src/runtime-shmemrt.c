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
 *
 * Run-time support for the built-in runtime that is part of the
 * shared memory implementation Portals
 */

#include "config.h"

#include <stdlib.h>
#include <portals4.h>
#include <portals4_runtime.h>

#include "runtime.h"


int
shmem_internal_runtime_init(void)
{
    return 0;
}

int
shmem_internal_runtime_fini(void)
{
    return 0;
}

ptl_process_t*
shmem_internal_runtime_get_mapping(void)
{
    int size, i;
    ptl_process_t *ret;
    struct runtime_proc_t *procs;

    size = runtime_get_size();
    ret = malloc(sizeof(ptl_process_t) * size);
    if (NULL == ret) return NULL;

    runtime_get_nidpid_map(&procs);

    for (i = 0 ; i < size ; ++i) {
        ret[i].phys.nid = procs[i].nid;
        ret[i].phys.pid = procs[i].pid;
    }

    return ret;    
}


int
shmem_internal_runtime_get_rank(void)
{
    return runtime_get_rank();
}


int
shmem_internal_runtime_get_size(void)
{
    return runtime_get_size();
}


void
shmem_internal_runtime_barrier(void)
{
    runtime_barrier();
}
