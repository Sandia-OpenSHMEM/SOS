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

#include <stdio.h>
#include <stdlib.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_float_swap = pshmem_float_swap
#define shmem_float_swap pshmem_float_swap

#pragma weak shmem_double_swap = pshmem_double_swap
#define shmem_double_swap pshmem_double_swap

#pragma weak shmem_int_swap = pshmem_int_swap
#define shmem_int_swap pshmem_int_swap

#pragma weak shmem_long_swap = pshmem_long_swap
#define shmem_long_swap pshmem_long_swap

#pragma weak shmem_longlong_swap = pshmem_longlong_swap
#define shmem_longlong_swap pshmem_longlong_swap

#if !defined(__cplusplus) && !(defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(SHMEM_INTERNAL_INCLUDE))

#pragma weak shmem_swap = pshmem_swap
#define shmem_swap pshmem_swap

#endif

#pragma weak shmem_int_cswap = pshmem_int_cswap
#define shmem_int_cswap pshmem_int_cswap

#pragma weak shmem_long_cswap = pshmem_long_cswap
#define shmem_long_cswap pshmem_long_cswap

#pragma weak shmem_longlong_cswap = pshmem_longlong_cswap
#define shmem_longlong_cswap pshmem_longlong_cswap

#pragma weak shmem_int_inc = pshmem_int_inc
#define shmem_int_inc pshmem_int_inc

#pragma weak shmem_long_inc = pshmem_long_inc
#define shmem_long_inc pshmem_long_inc

#pragma weak shmem_longlong_inc = pshmem_longlong_inc
#define shmem_longlong_inc pshmem_longlong_inc

#pragma weak shmem_int_finc = pshmem_int_finc
#define shmem_int_finc pshmem_int_finc

#pragma weak shmem_long_finc = pshmem_long_finc
#define shmem_long_finc pshmem_long_finc

#pragma weak shmem_longlong_finc = pshmem_longlong_finc
#define shmem_longlong_finc pshmem_longlong_finc

#pragma weak shmem_int_add = pshmem_int_add
#define shmem_int_add pshmem_int_add

#pragma weak shmem_long_add = pshmem_long_add
#define shmem_long_add pshmem_long_add

#pragma weak shmem_longlong_add = pshmem_longlong_add
#define shmem_longlong_add pshmem_longlong_add

#pragma weak shmem_int_fadd = pshmem_int_fadd
#define shmem_int_fadd pshmem_int_fadd

#pragma weak shmem_long_fadd = pshmem_long_fadd
#define shmem_long_fadd pshmem_long_fadd

#pragma weak shmem_longlong_fadd = pshmem_longlong_fadd
#define shmem_longlong_fadd pshmem_longlong_fadd

#pragma weak shmem_int_fetch = pshmem_int_fetch
#define shmem_int_fetch pshmem_int_fetch

#pragma weak shmem_long_fetch = pshmem_long_fetch
#define shmem_long_fetch pshmem_long_fetch

#pragma weak shmem_longlong_fetch = pshmem_longlong_fetch
#define shmem_longlong_fetch pshmem_longlong_fetch

#pragma weak shmem_float_fetch = pshmem_float_fetch
#define shmem_float_fetch pshmem_float_fetch

#pragma weak shmem_double_fetch = pshmem_double_fetch
#define shmem_double_fetch pshmem_double_fetch

#pragma weak shmem_int_set = pshmem_int_set
#define shmem_int_set pshmem_int_set

#pragma weak shmem_long_set = pshmem_long_set
#define shmem_long_set pshmem_long_set

#pragma weak shmem_longlong_set = pshmem_longlong_set
#define shmem_longlong_set pshmem_longlong_set

#pragma weak shmem_float_set = pshmem_float_set
#define shmem_float_set pshmem_float_set

#pragma weak shmem_double_set = pshmem_double_set
#define shmem_double_set pshmem_double_set

#endif /* ENABLE_PROFILING */

#define CAT(x,y) x ## y
#define SHM_INTERNAL_T(STYPE) CAT(SHM_INTERNAL_T_,STYPE)
#define SHM_INTERNAL_T_short      SHM_INTERNAL_SHORT
#define SHM_INTERNAL_T_int        SHM_INTERNAL_INT
#define SHM_INTERNAL_T_long       SHM_INTERNAL_LONG
#define SHM_INTERNAL_T_longlong   SHM_INTERNAL_LONG_LONG
#define SHM_INTERNAL_T_float      SHM_INTERNAL_FLOAT
#define SHM_INTERNAL_T_double     SHM_INTERNAL_DOUBLE
#define SHM_INTERNAL_T_longdouble SHM_INTERNAL_LONG_DOUBLE
#define SHM_INTERNAL_T_complexf   SHM_INTERNAL_FLOAT_COMPLEX
#define SHM_INTERNAL_T_complexd   SHM_INTERNAL_DOUBLE_COMPLEX

#if !defined(__cplusplus) && !(defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L && !defined(SHMEM_INTERNAL_INCLUDE))

long shmemx_ctx_swap(long *target, long value, int pe,
    shmemx_ctx_t c)
{
  long newval;
  SHMEM_ERR_CHECK_INITIALIZED();
  shmem_internal_swap(target, &value, &newval, sizeof(long), pe,
      SHM_INTERNAL_T(long), c);
  shmemx_ctx_quiet(c);
  return newval;
}

#endif

#define SHMEM_DEF_SWAP(STYPE,TYPE) \
  TYPE shmemx_ctx_##STYPE##_swap(TYPE *target, TYPE value, int pe,     \
        shmemx_ctx_t c)                                              \
  {                                                                  \
    TYPE newval;                                                     \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
    shmem_internal_swap(target, &value, &newval, sizeof(TYPE), pe,   \
        SHM_INTERNAL_T(STYPE), c);                                   \
    shmemx_ctx_quiet(c);                                             \
    return newval;                                                   \
  }

SHMEM_DEFINE_FOR_EXTENDED_AMO(SHMEM_DEF_SWAP)


#define SHMEM_DEF_CSWAP(STYPE,TYPE) \
  TYPE shmemx_ctx_##STYPE##_cswap(TYPE *target, TYPE cond,           \
      TYPE value, int pe, shmemx_ctx_t c)                            \
  {                                                                  \
    TYPE newval;                                                     \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
    shmem_internal_cswap(target, &value, &newval, &cond,             \
        sizeof(TYPE), pe, SHM_INTERNAL_T(STYPE), c);                 \
    shmemx_ctx_quiet(c);                                             \
    return newval;                                                   \
  }

SHMEM_DEFINE_FOR_AMO(SHMEM_DEF_CSWAP)

#define SHMEM_DEF_INC(STYPE,TYPE) \
  void shmemx_ctx_##STYPE##_inc(TYPE *target, int pe, shmemx_ctx_t c)  \
  {                                                                  \
    TYPE tmp = 1;                                                    \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
    shmem_internal_atomic_small(target, &tmp, sizeof(TYPE), pe,      \
        SHM_INTERNAL_SUM, SHM_INTERNAL_T(STYPE), c);                 \
  }

SHMEM_DEFINE_FOR_AMO(SHMEM_DEF_INC)


#define SHMEM_DEF_FINC(STYPE,TYPE) \
  TYPE shmemx_ctx_##STYPE##_finc(TYPE *target, int pe, shmemx_ctx_t c) \
  {                                                                  \
    TYPE oldval, tmp = 1;                                            \
                                                                     \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_fetch_atomic(target, &tmp, &oldval, sizeof(TYPE), \
        pe, SHM_INTERNAL_SUM, SHM_INTERNAL_T(STYPE), c);             \
    shmemx_ctx_quiet(c);                                             \
    return oldval;                                                   \
  }

SHMEM_DEFINE_FOR_AMO(SHMEM_DEF_FINC)


#define SHMEM_DEF_ADD(STYPE,TYPE) \
  void shmemx_ctx_##STYPE##_add(TYPE *target, TYPE value, int pe,    \
      shmemx_ctx_t c)                                              \
  {                                                                \
    SHMEM_ERR_CHECK_INITIALIZED();                                 \
    shmem_internal_atomic_small(target, &value, sizeof(TYPE), pe,  \
        SHM_INTERNAL_SUM, SHM_INTERNAL_T(STYPE), c);               \
  }

SHMEM_DEFINE_FOR_AMO(SHMEM_DEF_ADD)


#define SHMEM_DEF_FADD(STYPE,TYPE) \
  TYPE shmemx_ctx_##STYPE##_fadd(TYPE *target, TYPE value, int pe,     \
      shmemx_ctx_t c)                                                \
  {                                                                  \
    TYPE oldval;                                                     \
                                                                     \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_fetch_atomic(target, &value, &oldval,             \
        sizeof(TYPE), pe, SHM_INTERNAL_SUM, SHM_INTERNAL_T(STYPE),   \
        c);                                                          \
    shmemx_ctx_quiet(c);                                             \
    return oldval;                                                   \
  }

SHMEM_DEFINE_FOR_AMO(SHMEM_DEF_FADD)

/** Generate atomic fetch C bindings */
#define SHMEM_CAPI_ATOMIC_FETCH(STYPE, TYPE) \
TYPE                                                                 \
shmemx_ctx_##STYPE##_fetch(const TYPE *source, int pe,               \
    shmemx_ctx_t c)                                                  \
{                                                                    \
    TYPE val;                                                        \
                                                                     \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_atomic_fetch(&val, (void *) source,               \
        sizeof(TYPE), pe, SHM_INTERNAL_T(STYPE), c);                 \
    shmemx_ctx_quiet(SHMEMX_CTX_DEFAULT);                            \
    return val;                                                      \
}

SHMEM_DEFINE_FOR_EXTENDED_AMO(SHMEM_CAPI_ATOMIC_FETCH)

/* Generate atomic set C bindings
 *
 * Note: value is passed to the transport from the stack.  Transport must be
 * finished with value argument when shmem_internal_atomic_set returns.
 */
#define SHMEM_CAPI_ATOMIC_SET(STYPE, TYPE) \
void                                                                 \
shmemx_ctx_##STYPE##_set(TYPE *dest, TYPE value, int pe,             \
    shmemx_ctx_t c)                                                  \
{                                                                    \
    SHMEM_ERR_CHECK_INITIALIZED();                                   \
                                                                     \
    shmem_internal_atomic_set((void *) dest, &value,                 \
        sizeof(TYPE), pe, SHM_INTERNAL_T(STYPE), c);                 \
}

SHMEM_DEFINE_FOR_EXTENDED_AMO(SHMEM_CAPI_ATOMIC_SET)

