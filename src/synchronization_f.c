/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
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
#include "shmem_comm.h"
#include "shmem_atomic.h"
#include "shmem_synchronization.h"


#define FC_SHMEM_QUIET FC_FUNC_(shmem_quiet, SHMEM_QUIET)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_QUIET(void);
void
FC_SHMEM_QUIET(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_quiet(SHMEM_CTX_DEFAULT);
}

#define FC_SHMEM_PE_QUIET FC_FUNC_(shmem_pe_quiet, SHMEM_PE_QUIET)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_PE_QUIET(const int *target_pes, int npes);
void
FC_SHMEM_PE_QUIET(const int *target_pes, int npes)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_pe_quiet(SHMEM_CTX_DEFAULT, target_pes, npes);
}

#define FC_SHMEM_CTX_QUIET FC_FUNC_(shmem_ctx_pe_quiet, SHMEM_CTX_PE_QUIET)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_CTX_PE_QUIET(shmem_ctx_t ctx, const int *target_pes, int npes);
void
FC_SHMEM_CTX_PE_QUIET(shmem_ctx_t ctx, const int *target_pes, int npes)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_pe_quiet(ctx, target_pes, npes);
}


#define FC_SHMEM_FENCE FC_FUNC_(shmem_fence, SHMEM_FENCE)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_FENCE(void);
void
FC_SHMEM_FENCE(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_fence(SHMEM_CTX_DEFAULT);
}


#define FC_SHMEM_INT4_WAIT FC_FUNC_(shmem_int4_wait, SHMEM_INT4_WAIT)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_INT4_WAIT(int32_t *var,
                   int32_t *value);
void
FC_SHMEM_INT4_WAIT(int32_t *var,
                   int32_t *value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(var, 4);

    SHMEM_WAIT(var, *value);
}


#define FC_SHMEM_INT8_WAIT FC_FUNC_(shmem_int8_wait, SHMEM_INT8_WAIT)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_INT8_WAIT(int64_t *var,
                   int64_t *value);
void
FC_SHMEM_INT8_WAIT(int64_t *var,
                   int64_t *value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(var, 8);

    SHMEM_WAIT(var, *value);
}


#define FC_SHMEM_WAIT FC_FUNC_(shmem_wait, SHMEM_WAIT)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_WAIT(fortran_integer_t *var,
              fortran_integer_t *value);
void
FC_SHMEM_WAIT(fortran_integer_t *var,
              fortran_integer_t *value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(var, SIZEOF_FORTRAN_INTEGER);

    SHMEM_WAIT(var, *value);
}


#define FC_SHMEM_INT4_WAIT_UNTIL FC_FUNC_(shmem_int4_wait_until, SHMEM_INT4_WAIT_UNTIL)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_INT4_WAIT_UNTIL(int32_t *var,
                         fortran_integer_t *cond,
                         int32_t *value);
void
FC_SHMEM_INT4_WAIT_UNTIL(int32_t *var,
                         fortran_integer_t *cond,
                         int32_t *value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(var, 4);
    SHMEM_ERR_CHECK_CMP_OP(*cond);

    SHMEM_WAIT_UNTIL(var, *cond, *value);
}


#define FC_SHMEM_INT8_WAIT_UNTIL FC_FUNC_(shmem_int8_wait_until, SHMEM_INT8_WAIT_UNTIL)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_INT8_WAIT_UNTIL(int64_t *var,
                         fortran_integer_t *cond,
                         int64_t *value);
void
FC_SHMEM_INT8_WAIT_UNTIL(int64_t *var,
                         fortran_integer_t *cond,
                         int64_t *value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(var, 8);
    SHMEM_ERR_CHECK_CMP_OP(*cond);

    SHMEM_WAIT_UNTIL(var, *cond, *value);
}


#define FC_SHMEM_WAIT_UNTIL FC_FUNC_(shmem_wait_until, SHMEM_WAIT_UNTIL)
void SHMEM_FUNCTION_ATTRIBUTES
FC_SHMEM_WAIT_UNTIL(fortran_integer_t *var,
                    fortran_integer_t *cond,
                    fortran_integer_t *value);
void
FC_SHMEM_WAIT_UNTIL(fortran_integer_t *var,
                    fortran_integer_t *cond,
                    fortran_integer_t *value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC(var, SIZEOF_FORTRAN_INTEGER);
    SHMEM_ERR_CHECK_CMP_OP(*cond);

    SHMEM_WAIT_UNTIL(var, *cond, *value);
}
