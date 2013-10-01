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


#define FC_SHMEM_QUIET FC_FUNC_(shmem_quiet, SHMEM_QUIET)
void FC_SHMEM_QUIET(void);
void
FC_SHMEM_QUIET(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_quiet();
}


#define FC_SHMEM_FENCE FC_FUNC_(shmem_fence, SHMEM_FENCE)
void FC_SHMEM_FENCE(void);
void
FC_SHMEM_FENCE(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fence();
}


#define FC_SHMEM_INT4_WAIT FC_FUNC_(shmem_int4_wait, SHMEM_INT4_WAIT)
void FC_SHMEM_INT4_WAIT(volatile int32_t *var, 
                        int32_t *value);
void
FC_SHMEM_INT4_WAIT(volatile int32_t *var,
                   int32_t *value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, *value);
}


#define FC_SHMEM_INT8_WAIT FC_FUNC_(shmem_int8_wait, SHMEM_INT8_WAIT)
void FC_SHMEM_INT8_WAIT(volatile int64_t *var, 
                        int64_t *value);
void
FC_SHMEM_INT8_WAIT(volatile int64_t *var,
                   int64_t *value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, *value);
}


#define FC_SHMEM_WAIT FC_FUNC_(shmem_wait, SHMEM_WAIT)
void FC_SHMEM_WAIT(volatile fortran_integer_t *var, 
                   fortran_integer_t *value);
void
FC_SHMEM_WAIT(volatile fortran_integer_t *var,
              fortran_integer_t *value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT(var, *value);
}


#define FC_SHMEM_INT4_WAIT_UNTIL FC_FUNC_(shmem_int4_wait_until, SHMEM_INT4_WAIT_UNTIL)
void FC_SHMEM_INT4_WAIT_UNTIL(volatile int32_t *var, 
                              fortran_integer_t *cond,
                              int32_t *value);
void
FC_SHMEM_INT4_WAIT_UNTIL(volatile int32_t *var,
                         fortran_integer_t *cond,
                         int32_t *value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, *cond, *value);
}


#define FC_SHMEM_INT8_WAIT_UNTIL FC_FUNC_(shmem_int8_wait_until, SHMEM_INT8_WAIT_UNTIL)
void FC_SHMEM_INT8_WAIT_UNTIL(volatile int64_t *var, 
                              fortran_integer_t *cond,
                              int64_t *value);
void
FC_SHMEM_INT8_WAIT_UNTIL(volatile int64_t *var,
                         fortran_integer_t *cond,
                         int64_t *value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, *cond, *value);
}


#define FC_SHMEM_WAIT_UNTIL FC_FUNC_(shmem_wait_until, SHMEM_WAIT_UNTIL)
void FC_SHMEM_WAIT_UNTIL(volatile fortran_integer_t *var, 
                         fortran_integer_t *cond,
                         fortran_integer_t *value);
void
FC_SHMEM_WAIT_UNTIL(volatile fortran_integer_t *var,
                    fortran_integer_t *cond,
                    fortran_integer_t *value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    SHMEM_WAIT_UNTIL(var, *cond, *value);
}
