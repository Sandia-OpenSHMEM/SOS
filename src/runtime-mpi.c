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
#include <stdio.h>
#include <mpi.h>
#include <portals4.h>

#include "runtime.h"

static int rank = -1;
static int size = -1;
static ptl_process_t *mapping = NULL;
static ptl_handle_ni_t phys_ni_h;

int
shmem_internal_runtime_init(void)
{
    int ret;
    ptl_process_t my_id;

    MPI_Initialized(&ret);
    if (!ret) {
        if (MPI_SUCCESS != MPI_Init(NULL, NULL)) {
            return 1;
        }
    }

    if (MPI_SUCCESS != MPI_Comm_size(MPI_COMM_WORLD, &size)) {
        return 1;
    }
    if (MPI_SUCCESS != MPI_Comm_rank(MPI_COMM_WORLD, &rank)) {
        return 1;
    }

    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_NO_MATCHING | PTL_NI_PHYSICAL,
                    PTL_PID_ANY,
                    NULL,
                    NULL,
                    &phys_ni_h);
    if (PTL_OK != ret) return 1;

    ret = PtlGetId(phys_ni_h, &my_id);
    if (PTL_OK != ret) return 1;

    mapping = malloc(sizeof(ptl_process_t) * size);
    if (NULL == mapping) return 1;

    if (MPI_SUCCESS != MPI_Allgather(&my_id, sizeof(my_id), MPI_BYTE,
                                     mapping, sizeof(my_id), MPI_BYTE,
                                     MPI_COMM_WORLD)) {
        return 1;
    }

    return 0;
}


int
shmem_internal_runtime_fini(void)
{
    int ret;

    if (NULL != mapping) free(mapping);

    MPI_Finalized(&ret);
    if (!ret) {
        MPI_Finalize();
    }

    PtlNIFini(phys_ni_h);

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
    return rank;
}


int
shmem_internal_runtime_get_size(void)
{
    return size;
}


void
shmem_internal_runtime_barrier(void)
{
    MPI_Barrier(MPI_COMM_WORLD);
}
