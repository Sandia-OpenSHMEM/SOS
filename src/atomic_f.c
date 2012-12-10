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
#include "shmem_comm.h"


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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, value, &newval, SIZEOF_FORTRAN_INTEGER, 
			*pe, DTYPE_FORTRAN_INTEGER);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_INT4_SWAP FC_FUNC_(shmem_int4_swap, SHMEM_INT4_SWAP)
int32_t FC_SHMEM_INT4_SWAP(int32_t *target, 
                           int32_t *value,
                           fortran_integer_t *pe);
int32_t
FC_SHMEM_INT4_SWAP(int32_t *target,
                   int32_t *value, 
                   fortran_integer_t *pe)
{
    int32_t newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, value, &newval, 4, 
			*pe, PTL_INT32_T);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_INT8_SWAP FC_FUNC_(shmem_int8_swap, SHMEM_INT8_SWAP)
int64_t FC_SHMEM_INT8_SWAP(int64_t *target, 
                           int64_t *value,
                           fortran_integer_t *pe);
int64_t
FC_SHMEM_INT8_SWAP(int64_t *target,
                   int64_t *value, 
                   fortran_integer_t *pe)
{
    int64_t newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, value, &newval, 8, 
			*pe, PTL_INT64_T);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_REAL4_SWAP FC_FUNC_(shmem_real4_swap, SHMEM_REAL4_SWAP)
float FC_SHMEM_REAL4_SWAP(float *target, 
                          float *value,
                          fortran_integer_t *pe);
float
FC_SHMEM_REAL4_SWAP(float *target,
                    float *value, 
                    fortran_integer_t *pe)
{
    float newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, value, &newval, 4,
			*pe, PTL_FLOAT);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_REAL8_SWAP FC_FUNC_(shmem_real8_swap, SHMEM_REAL8_SWAP)
double FC_SHMEM_REAL8_SWAP(double *target, 
                           double *value,
                           fortran_integer_t *pe);
double
FC_SHMEM_REAL8_SWAP(double *target,
                    double *value, 
                    fortran_integer_t *pe)
{
    double newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, value, &newval, 8,
			*pe, PTL_DOUBLE);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_INT4_CSWAP FC_FUNC_(shmem_int4_cswap, SHMEM_INT4_CSWAP)
int32_t FC_SHMEM_INT4_CSWAP(int32_t *target, 
                            int32_t *cond,
                            int32_t *value,
                            fortran_integer_t *pe);
int32_t
FC_SHMEM_INT4_CSWAP(int32_t *target,
                    int32_t *cond, 
                    int32_t *value, 
                    fortran_integer_t *pe)
{
    int32_t newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_cswap(target, value, &newval, cond, 
                         4, 
                         *pe, PTL_INT32_T);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_INT8_CSWAP FC_FUNC_(shmem_int8_cswap, SHMEM_INT8_CSWAP)
int64_t FC_SHMEM_INT8_CSWAP(int64_t *target, 
                            int64_t *cond,
                            int64_t *value,
                            fortran_integer_t *pe);
int64_t
FC_SHMEM_INT8_CSWAP(int64_t *target,
                    int64_t *cond, 
                    int64_t *value, 
                    fortran_integer_t *pe)
{
    int64_t newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_cswap(target, value, &newval, cond,
                         8, 
                         *pe, PTL_INT64_T);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_INT4_FADD FC_FUNC_(shmem_int4_fadd, SHMEM_INT4_FADD)
int32_t FC_SHMEM_INT4_FADD(int32_t *target, 
                           int32_t *value,
                           fortran_integer_t *pe);
int32_t
FC_SHMEM_INT4_FADD(int32_t *target,
                   int32_t *value, 
                   fortran_integer_t *pe)
{
    int32_t oldval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, value, &oldval, 4, 
                                *pe, PTL_SUM, PTL_INT32_T);
    shmem_internal_get_wait();
    return oldval;
}


#define FC_SHMEM_INT8_FADD FC_FUNC_(shmem_int8_fadd, SHMEM_INT8_FADD)
int64_t FC_SHMEM_INT8_FADD(int64_t *target, 
                           int64_t *value,
                           fortran_integer_t *pe);
int64_t
FC_SHMEM_INT8_FADD(int64_t *target,
                   int64_t *value, 
                   fortran_integer_t *pe)
{
    int64_t oldval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, value, &oldval, 8, 
                                *pe, PTL_SUM, PTL_INT64_T);
    shmem_internal_get_wait();
    return oldval;
}


#define FC_SHMEM_INT4_FINC FC_FUNC_(shmem_int4_finc, SHMEM_INT4_FINC)
int32_t FC_SHMEM_INT4_FINC(int32_t *target, 
                           fortran_integer_t *pe);
int32_t
FC_SHMEM_INT4_FINC(int32_t *target,
                   fortran_integer_t *pe)
{
    int32_t oldval, tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, &tmp, &oldval, 4, 
                                *pe, PTL_SUM, PTL_INT32_T);
    shmem_internal_get_wait();
    return oldval;
}


#define FC_SHMEM_INT8_FINC FC_FUNC_(shmem_int8_finc, SHMEM_INT8_FINC)
int64_t FC_SHMEM_INT8_FINC(int64_t *target, 
                           fortran_integer_t *pe);
int64_t
FC_SHMEM_INT8_FINC(int64_t *target,
                   fortran_integer_t *pe)
{
    int64_t oldval, tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fetch_atomic(target, &tmp, &oldval, 8, 
                                *pe, PTL_SUM, PTL_INT64_T);
    shmem_internal_get_wait();
    return oldval;
}


#define FC_SHMEM_INT4_ADD FC_FUNC_(shmem_int4_add, SHMEM_INT4_ADD)
void FC_SHMEM_INT4_ADD(int32_t *target, 
                       int32_t *value,
                       fortran_integer_t *pe);
void
FC_SHMEM_INT4_ADD(int32_t *target,
                  int32_t *value, 
                  fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, value, 4, 
                                 *pe, PTL_SUM, PTL_INT32_T);
}


#define FC_SHMEM_INT8_ADD FC_FUNC_(shmem_int8_add, SHMEM_INT8_ADD)
void FC_SHMEM_INT8_ADD(int64_t *target, 
                       int64_t *value,
                       fortran_integer_t *pe);
void
FC_SHMEM_INT8_ADD(int64_t *target,
                  int64_t *value, 
                  fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, value, 8, 
                                 *pe, PTL_SUM, PTL_INT64_T);
}


#define FC_SHMEM_INT4_INC FC_FUNC_(shmem_int4_inc, SHMEM_INT4_INC)
void FC_SHMEM_INT4_INC(int32_t *target, 
                       fortran_integer_t *pe);
void
FC_SHMEM_INT4_INC(int32_t *target,
                  fortran_integer_t *pe)
{
    int32_t tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, &tmp, 4, 
                                 *pe, PTL_SUM, PTL_INT32_T);
}


#define FC_SHMEM_INT8_INC FC_FUNC_(shmem_int8_inc, SHMEM_INT8_INC)
void FC_SHMEM_INT8_INC(int64_t *target, 
                       fortran_integer_t *pe);
void
FC_SHMEM_INT8_INC(int64_t *target,
                  fortran_integer_t *pe)
{
    int64_t tmp = 1;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_atomic_small(target, &tmp, 8, 
                                 *pe, PTL_SUM, PTL_INT64_T);
}
