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
#include "shmem_comm.h"

#define SHMEM_DEFINE_FOR_FC_AMO(DECL) \
  DECL(int4, INT4, int32_t, 4) \
  DECL(int8, INT8, int64_t, 8)

#define SHMEM_DEFINE_FOR_FC_EXTENDED_AMO(DECL) \
  SHMEM_DEFINE_FOR_FC_AMO(DECL) \
  DECL(real4, REAL4, float,  4) \
  DECL(real8, REAL8, double, 8)

#define SHMEM_FC(FC_type,FC_TYPE,SHM_op,SHM_OP) \
  FC_FUNC_(shmem_##FC_type##_##SHM_op,SHMEM_##FC_TYPE##_##SHM_OP)

#define CAT(x,y) x ## y
#define SHM_INTERNAL_FC_T(STYPE) CAT(SHM_INTERNAL_FC_T_,STYPE)
#define SHM_INTERNAL_FC_T_int4    SHM_INTERNAL_INT32
#define SHM_INTERNAL_FC_T_int8    SHM_INTERNAL_INT64
#define SHM_INTERNAL_FC_T_real4   SHM_INTERNAL_FLOAT
#define SHM_INTERNAL_FC_T_real8   SHM_INTERNAL_DOUBLE


/* The only fortran atomic without an explicit type. I wonder, whose
 * legacy program requires it? -jpdoyle
 */
#define FC_SHMEM_SWAP FC_FUNC_(shmem_swap, SHMEM_SWAP)
fortran_integer_t FC_SHMEM_SWAP(fortran_integer_t *target,
                                fortran_integer_t *value,
                                fortran_integer_t *pe);
fortran_integer_t
FC_SHMEM_SWAP(fortran_integer_t *target,
              fortran_integer_t *value,
              fortran_integer_t *pe)
{
    fortran_integer_t newval;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target, SIZEOF_FORTRAN_INTEGER);

    shmem_internal_swap(target, value, &newval,
        SIZEOF_FORTRAN_INTEGER, *pe, SHM_INTERNAL_FORTRAN_INTEGER,
        SHMEMX_CTX_DEFAULT);
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);
    return newval;
}

#define SHMEM_DEF_FC_SWAP(FC_type,FC_TYPE,TYPE,NBYTES) \
  TYPE SHMEM_FC(FC_type,FC_TYPE,swap,SWAP)(TYPE *target, TYPE *source, \
      fortran_integer_t *pe);                \
  TYPE SHMEM_FC(FC_type,FC_TYPE,swap,SWAP)(TYPE *target, TYPE *source, \
      fortran_integer_t *pe) {                                         \
    TYPE newval;                                                       \
                                                                       \
    SHMEM_ERR_CHECK_INITIALIZED();                                     \
    SHMEM_ERR_CHECK_PE(*pe);                                           \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                              \
    shmem_internal_assert(sizeof(TYPE) == NBYTES);                     \
                                                                       \
    shmem_internal_swap(target, source, &newval, sizeof(TYPE), *pe,    \
        SHM_INTERNAL_FC_T(FC_type), SHMEMX_CTX_DEFAULT);               \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                              \
    return newval;                                                     \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_AMO(SHMEM_DEF_FC_SWAP)

#define SHMEM_DEF_FC_CSWAP(FC_type,FC_TYPE,TYPE,NBYTES) \
  TYPE SHMEM_FC(FC_type,FC_TYPE,cswap,CSWAP)(TYPE *target, TYPE *cond, \
      TYPE *value, fortran_integer_t *pe);                \
  TYPE SHMEM_FC(FC_type,FC_TYPE,cswap,CSWAP)(TYPE *target, TYPE *cond, \
      TYPE *value, fortran_integer_t *pe) {                          \
    TYPE newval;                                                       \
                                                                       \
    SHMEM_ERR_CHECK_INITIALIZED();                                     \
    SHMEM_ERR_CHECK_PE(*pe);                                           \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                              \
    shmem_internal_assert(sizeof(TYPE) == NBYTES);                     \
                                                                       \
    shmem_internal_cswap(target, value, &newval, cond, sizeof(TYPE),   \
        *pe, SHM_INTERNAL_FC_T(FC_type), SHMEMX_CTX_DEFAULT);          \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                              \
    return newval;                                                     \
  }

SHMEM_DEFINE_FOR_FC_AMO(SHMEM_DEF_FC_CSWAP)


#define SHMEM_DEF_FC_FADD(FC_type,FC_TYPE,TYPE,NBYTES) \
  TYPE SHMEM_FC(FC_type,FC_TYPE,fadd,FADD)(TYPE *target, TYPE *value, \
      fortran_integer_t *pe);                \
  TYPE SHMEM_FC(FC_type,FC_TYPE,fadd,FADD)(TYPE *target, TYPE *value, \
      fortran_integer_t *pe) {                                         \
    TYPE oldval;                                                       \
                                                                       \
    SHMEM_ERR_CHECK_INITIALIZED();                                     \
    SHMEM_ERR_CHECK_PE(*pe);                                           \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                              \
    shmem_internal_assert(sizeof(TYPE) == NBYTES);                     \
                                                                       \
    shmem_internal_fetch_atomic(target, value, &oldval, sizeof(TYPE),   \
        *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_FC_T(FC_type),              \
        SHMEMX_CTX_DEFAULT);                                            \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                              \
    return oldval;                                                     \
  }

SHMEM_DEFINE_FOR_FC_AMO(SHMEM_DEF_FC_FADD)


#define SHMEM_DEF_FC_FINC(FC_type,FC_TYPE,TYPE,NBYTES) \
  TYPE SHMEM_FC(FC_type,FC_TYPE,finc,FINC)(TYPE *target, \
      fortran_integer_t *pe);                \
  TYPE SHMEM_FC(FC_type,FC_TYPE,finc,FINC)(TYPE *target, \
      fortran_integer_t *pe) {                                         \
    TYPE oldval, tmp = 1;                                              \
                                                                       \
    SHMEM_ERR_CHECK_INITIALIZED();                                     \
    SHMEM_ERR_CHECK_PE(*pe);                                           \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                              \
    shmem_internal_assert(sizeof(TYPE) == NBYTES);                     \
                                                                       \
    shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(TYPE),   \
        *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_FC_T(FC_type),              \
        SHMEMX_CTX_DEFAULT);                                            \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                              \
    return oldval;                                                     \
  }

SHMEM_DEFINE_FOR_FC_AMO(SHMEM_DEF_FC_FINC)


#define SHMEM_DEF_FC_ADD(FC_type,FC_TYPE,TYPE,NBYTES) \
  void SHMEM_FC(FC_type,FC_TYPE,add,ADD)(TYPE *target, TYPE *value, \
      fortran_integer_t *pe);                \
  void SHMEM_FC(FC_type,FC_TYPE,add,ADD)(TYPE *target, TYPE *value, \
      fortran_integer_t *pe) {                                         \
                                                                       \
    SHMEM_ERR_CHECK_INITIALIZED();                                     \
    SHMEM_ERR_CHECK_PE(*pe);                                           \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                              \
    shmem_internal_assert(sizeof(TYPE) == NBYTES);                     \
                                                                       \
    shmem_internal_atomic_small(target, value, sizeof(TYPE),           \
        *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_FC_T(FC_type),    \
        SHMEMX_CTX_DEFAULT);                                  \
  }

SHMEM_DEFINE_FOR_FC_AMO(SHMEM_DEF_FC_ADD)


#define SHMEM_DEF_FC_INC(FC_type,FC_TYPE,TYPE,NBYTES) \
  void SHMEM_FC(FC_type,FC_TYPE,inc,INC)(TYPE *target, \
      fortran_integer_t *pe);                \
  void SHMEM_FC(FC_type,FC_TYPE,inc,INC)(TYPE *target, \
      fortran_integer_t *pe) {                                \
    TYPE tmp = 1;                                             \
                                                              \
    SHMEM_ERR_CHECK_INITIALIZED();                            \
    SHMEM_ERR_CHECK_PE(*pe);                                           \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                              \
    shmem_internal_assert(sizeof(TYPE) == NBYTES);            \
                                                              \
    shmem_internal_atomic_small(target, &tmp, sizeof(TYPE),   \
        *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_FC_T(FC_type),    \
        SHMEMX_CTX_DEFAULT);                                  \
  }

SHMEM_DEFINE_FOR_FC_AMO(SHMEM_DEF_FC_INC)


#define SHMEM_DEF_FC_FETCH(FC_type,FC_TYPE,TYPE,NBYTES) \
  TYPE SHMEM_FC(FC_type,FC_TYPE,fetch,FETCH)(TYPE *source, \
      fortran_integer_t *pe);                \
  TYPE SHMEM_FC(FC_type,FC_TYPE,fetch,FETCH)(TYPE *source, \
      fortran_integer_t *pe) {                                         \
    TYPE val;                                              \
                                                                       \
    SHMEM_ERR_CHECK_INITIALIZED();                                     \
    SHMEM_ERR_CHECK_PE(*pe);                                           \
    shmem_internal_assert(sizeof(TYPE) == NBYTES);                     \
                                                                       \
    shmem_internal_atomic_fetch(&val, (void*)source, sizeof(TYPE),  \
        *pe, SHM_INTERNAL_FC_T(FC_type), SHMEMX_CTX_DEFAULT);          \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                              \
    return val;                                                     \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_AMO(SHMEM_DEF_FC_FETCH)


#define SHMEM_DEF_FC_SET(FC_type,FC_TYPE,TYPE,NBYTES) \
  void SHMEM_FC(FC_type,FC_TYPE,set,SET)(TYPE *target, TYPE *value, \
      fortran_integer_t *pe);                \
  void SHMEM_FC(FC_type,FC_TYPE,set,SET)(TYPE *target, TYPE *value, \
      fortran_integer_t *pe) {                                \
                                                              \
    SHMEM_ERR_CHECK_INITIALIZED();                            \
    SHMEM_ERR_CHECK_PE(*pe);                                           \
    SHMEM_ERR_CHECK_SYMMETRIC(target, sizeof(TYPE));                              \
    shmem_internal_assert(sizeof(TYPE) == (NBYTES));            \
                                                              \
    shmem_internal_atomic_set((void*) target, (const void*)value, \
        sizeof(TYPE), *pe, SHM_INTERNAL_FC_T(FC_type),        \
        SHMEMX_CTX_DEFAULT);                                  \
  }

SHMEM_DEFINE_FOR_FC_EXTENDED_AMO(SHMEM_DEF_FC_SET)
