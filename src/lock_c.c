/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2016 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
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
#include "shmem_lock.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_clear_lock = pshmem_clear_lock
#define shmem_clear_lock pshmem_clear_lock

#pragma weak shmem_set_lock = pshmem_set_lock
#define shmem_set_lock pshmem_set_lock

#pragma weak shmem_test_lock = pshmem_test_lock
#define shmem_test_lock pshmem_test_lock

#endif /* ENABLE_PROFILING */


void
shmem_clear_lock(volatile long *lockp)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(lockp, sizeof(long));

    shmem_internal_clear_lock(lockp);
}


void
shmem_set_lock(volatile long *lockp)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(lockp, sizeof(long));

    shmem_internal_set_lock(lockp);
}


int
shmem_test_lock(volatile long *lockp)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(lockp, sizeof(long));

    return shmem_internal_test_lock(lockp);
}
