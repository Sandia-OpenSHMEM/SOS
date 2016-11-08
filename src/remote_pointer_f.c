/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_remote_pointer.h"


#define FC_SHMEM_PTR FC_FUNC_(shmem_ptr, SHMEM_PTR)
void* FC_SHMEM_PTR(void **target, fortran_integer_t *pe);
void*
FC_SHMEM_PTR(void **target, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target, 1);

    return shmem_internal_ptr(*target, *pe);
}
