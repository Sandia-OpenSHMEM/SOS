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
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"


#define FC_SHMEM_FLOAT_PUT FC_FUNC_(shmem_float_put, SHMEM_FLOAT_PUT)
void FC_SHMEM_FLOAT_PUT(float *target, float *source, int len, int pe);
void
FC_SHMEM_FLOAT_PUT(float *target, float *source, int len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, sizeof(float) * len, pe);
    shmem_internal_put_wait(ret);
}
