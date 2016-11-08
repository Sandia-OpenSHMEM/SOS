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

#include <stdio.h>
#include <stdlib.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_remote_pointer.h"


#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_ptr = pshmem_ptr
#define shmem_ptr pshmem_ptr

#endif /* ENABLE_PROFILING */


void *
shmem_ptr(const void *target, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target, 1);

    return shmem_internal_ptr(target, pe);
}
