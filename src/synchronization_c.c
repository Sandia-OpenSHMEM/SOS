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

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_quiet = pshmem_quiet
#define shmem_quiet pshmem_quiet

#pragma weak shmem_fence = pshmem_fence
#define shmem_fence pshmem_fence

#pragma weak shmem_short_wait = pshmem_short_wait
#define shmem_short_wait pshmem_short_wait

#pragma weak shmem_short_wait_until = pshmem_short_wait_until
#define shmem_short_wait_until pshmem_short_wait_until

#pragma weak shmem_int_wait = pshmem_int_wait
#define shmem_int_wait pshmem_int_wait

#pragma weak shmem_int_wait_until = pshmem_int_wait_until
#define shmem_int_wait_until pshmem_int_wait_until

#pragma weak shmem_long_wait = pshmem_long_wait
#define shmem_long_wait pshmem_long_wait

#pragma weak shmem_long_wait_until = pshmem_long_wait_until
#define shmem_long_wait_until pshmem_long_wait_until

#pragma weak shmem_longlong_wait = pshmem_longlong_wait
#define shmem_longlong_wait pshmem_longlong_wait

#pragma weak shmem_longlong_wait_until = pshmem_longlong_wait_until
#define shmem_longlong_wait_until pshmem_longlong_wait_until

#pragma weak shmem_wait = pshmem_wait
#define shmem_wait pshmem_wait

#pragma weak shmem_wait_until = pshmem_wait_until
#define shmem_wait_until pshmem_wait_until

#endif /* ENABLE_PROFILING */

void
shmem_quiet(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);
}


void
shmem_fence(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmemx_ctx_fence(SHMEMX_CTX_DEFAULT);
}


void
shmem_short_wait(volatile short *var, short value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT(var, value);
}


void
shmem_short_wait_until(volatile short *var, int cond, short value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_int_wait(volatile int *var, int value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT(var, value);
}


void
shmem_int_wait_until(volatile int *var, int cond, int value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_long_wait(volatile long *var, long value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT(var, value);
}


void
shmem_long_wait_until(volatile long *var, int cond, long value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_longlong_wait(volatile long long *var, long long value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT(var, value);
}


void
shmem_longlong_wait_until(volatile long long *var, int cond, long long value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_wait(volatile long *ivar, long cmp_value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT(ivar, cmp_value);
}


void
shmem_wait_until(volatile long *ivar, int cmp, long value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    SHMEM_WAIT_UNTIL(ivar, cmp, value);
}
