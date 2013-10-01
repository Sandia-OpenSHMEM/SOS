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
#include <stdlib.h>
#include <stdio.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_atomic.h"
#include "shmem_synchronization.h"

#ifdef ENABLE_PROFILING

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_quiet();
}


void
shmem_fence(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fence();
}


void
shmem_short_wait(short *var, short value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, value);
}


void
shmem_short_wait_until(short *var, int cond, short value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_int_wait(int *var, int value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, value);
}


void
shmem_int_wait_until(int *var, int cond, int value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_long_wait(long *var, long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, value);
}


void
shmem_long_wait_until(long *var, int cond, long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_longlong_wait(long long *var, long long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, value);
}


void
shmem_longlong_wait_until(long long *var, int cond, long long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, cond, value);
}


void
shmem_wait(long *ivar, long cmp_value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(ivar, cmp_value);
}


void
shmem_wait_until(long *ivar, int cmp, long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(ivar, cmp, value);
}
