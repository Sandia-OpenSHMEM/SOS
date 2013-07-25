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
#include "shmem_comm.h"
#include "shmem_atomic.h"
#include "shmem_synchronization.h"

void
shmem_quiet(void)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

#ifdef USE_PORTALS4
    ret = shmem_transport_portals4_quiet();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_quiet();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
}


void
shmem_fence(void)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

#ifdef USE_PORTALS4
    ret = shmem_transport_portals4_fence();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_fence();
    if (0 != ret) { RAISE_ERROR(ret); }
#endif
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
