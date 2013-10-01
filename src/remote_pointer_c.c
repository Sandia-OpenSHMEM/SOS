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

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_remote_pointer.h"


#ifdef ENABLE_PROFILING

#pragma weak shmem_ptr = pshmem_ptr
#define shmem_ptr pshmem_ptr

#endif /* ENABLE_PROFILING */


void *
shmem_ptr(void *target, int pe)
{
    return shmem_internal_ptr(target, pe);
}
