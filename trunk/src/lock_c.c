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
#include "shmem_lock.h"

#ifdef ENABLE_PROFILING

#pragma weak shmem_clear_lock = pshmem_clear_lock
#define shmem_clear_lock pshmem_clear_lock

#pragma weak shmem_set_lock = pshmem_set_lock
#define shmem_set_lock pshmem_set_lock

#pragma weak shmem_test_lock = pshmem_test_lock
#define shmem_test_lock pshmem_test_lock

#endif /* ENABLE_PROFILING */


void
shmem_clear_lock(long *lockp)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_clear_lock(lockp);
}


void
shmem_set_lock(long *lockp)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_set_lock(lockp);
}


int
shmem_test_lock(long *lockp)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_test_lock(lockp);
}
