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

#include <stdlib.h>
#include <stdio.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_atomic.h"
#include "shmem_synchronization.h"
#include "synchronization_c_pshmem.h"

void
shmem_quiet(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_quiet();
}


void
shmem_fence(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_fence();
}


void
shmem_wait(volatile long *ivar, long cmp_value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(ivar, sizeof(long));

    SHMEM_WAIT(ivar, cmp_value);
}


void
shmem_wait_until(volatile long *ivar, int cmp, long value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(ivar, sizeof(long));
    SHMEM_ERR_CHECK_CMP_OP(cmp);

    SHMEM_WAIT_UNTIL(ivar, cmp, value);
}


#define SHMEM_DEF_WAIT(STYPE,TYPE)                              \
    void shmem_##STYPE##_wait(volatile TYPE *var, TYPE value)   \
    {                                                           \
        SHMEM_ERR_CHECK_INITIALIZED();                          \
        SHMEM_ERR_CHECK_SYMMETRIC(var, sizeof(TYPE));           \
                                                                \
        SHMEM_WAIT(var, value);                                 \
    }

SHMEM_DEFINE_FOR_INTS(SHMEM_DEF_WAIT)

#define SHMEM_DEF_WAIT_UNTIL(STYPE,TYPE)                                        \
    void shmem_##STYPE##_wait_until(volatile TYPE *var, int cond, TYPE value)   \
    {                                                                           \
        SHMEM_ERR_CHECK_INITIALIZED();                                          \
        SHMEM_ERR_CHECK_SYMMETRIC(var, sizeof(TYPE));                           \
        SHMEM_ERR_CHECK_CMP_OP(cond);                                           \
                                                                                \
        SHMEM_WAIT_UNTIL(var, cond, value);                                     \
    }

SHMEM_DEFINE_FOR_INTS(SHMEM_DEF_WAIT_UNTIL)
