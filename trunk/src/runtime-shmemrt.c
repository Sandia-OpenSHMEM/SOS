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

static ptl_process_t *mapping = NULL;

int
shmem_internal_runtime_init(void)
{
    int size, i;
    struct runtime_proc_t *procs;

    size = runtime_get_size();
    mapping = malloc(sizeof(ptl_process_t) * size);
    if (NULL == mapping) return 0;

    runtime_get_nidpid_map(&procs);

    for (i = 0 ; i < size ; ++i) {
        mapping[i].phys.nid = procs[i].nid;
        mapping[i].phys.pid = procs[i].pid;
    }

    return 0;
}

int
shmem_internal_runtime_fini(void)
{
    if (NULL != mapping) free(mapping);
    return 0;
}

ptl_process_t*
shmem_internal_runtime_get_mapping(void)
{
    return mapping;
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
