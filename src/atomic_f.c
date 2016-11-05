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

    shmem_internal_swap(target, value, &newval, SIZEOF_FORTRAN_INTEGER, 
			*pe, SHM_INTERNAL_FORTRAN_INTEGER);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_swap(target, value, &newval, 4, 
			*pe, SHM_INTERNAL_INT32);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_swap(target, value, &newval, 8, 
			*pe, SHM_INTERNAL_INT64);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_assert(sizeof(float) == 4);

    shmem_internal_swap(target, value, &newval, 4,
			*pe, SHM_INTERNAL_FLOAT);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_assert(sizeof(double) == 8);

    shmem_internal_swap(target, value, &newval, 8,
			*pe, SHM_INTERNAL_DOUBLE);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_cswap(target, value, &newval, cond, 
                         4, 
                         *pe, SHM_INTERNAL_INT32);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_cswap(target, value, &newval, cond,
                         8, 
                         *pe, SHM_INTERNAL_INT64);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_fetch_atomic(target, value, &oldval, 4, 
                                *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT32);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_fetch_atomic(target, value, &oldval, 8, 
                                *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT64);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_fetch_atomic(target, &tmp, &oldval, 4, 
                                *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT32);
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

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_fetch_atomic(target, &tmp, &oldval, 8, 
                                *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT64);
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
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_atomic_small(target, value, 4, 
                                 *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT32);
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
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_atomic_small(target, value, 8, 
                                 *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT64);
}


#define FC_SHMEM_INT4_INC FC_FUNC_(shmem_int4_inc, SHMEM_INT4_INC)
void FC_SHMEM_INT4_INC(int32_t *target, 
                       fortran_integer_t *pe);
void
FC_SHMEM_INT4_INC(int32_t *target,
                  fortran_integer_t *pe)
{
    int32_t tmp = 1;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_atomic_small(target, &tmp, 4, 
                                 *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT32);
}


#define FC_SHMEM_INT8_INC FC_FUNC_(shmem_int8_inc, SHMEM_INT8_INC)
void FC_SHMEM_INT8_INC(int64_t *target, 
                       fortran_integer_t *pe);
void
FC_SHMEM_INT8_INC(int64_t *target,
                  fortran_integer_t *pe)
{
    int64_t tmp = 1;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_atomic_small(target, &tmp, 8, 
                                 *pe, SHM_INTERNAL_SUM, SHM_INTERNAL_INT64);
}


#define FC_SHMEM_INT4_FETCH FC_FUNC_(shmem_int4_fetch, SHMEM_INT4_FETCH)
int32_t FC_SHMEM_INT4_FETCH(int32_t *source,
                            fortran_integer_t *pe);
int32_t
FC_SHMEM_INT4_FETCH(int32_t *source,
                    fortran_integer_t *pe)
{
    int32_t val;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_atomic_fetch(&val, (void *) source, 4, *pe, SHM_INTERNAL_INT32);
    shmem_internal_get_wait();

    return val;
}


#define FC_SHMEM_INT8_FETCH FC_FUNC_(shmem_int8_fetch, SHMEM_INT8_FETCH)
int64_t FC_SHMEM_INT8_FETCH(int64_t *source,
                            fortran_integer_t *pe);
int64_t
FC_SHMEM_INT8_FETCH(int64_t *source,
                    fortran_integer_t *pe)
{
    int64_t val;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_atomic_fetch(&val, (void *) source, 8, *pe, SHM_INTERNAL_INT64);
    shmem_internal_get_wait();

    return val;
}


#define FC_SHMEM_REAL4_FETCH FC_FUNC_(shmem_real4_fetch, SHMEM_REAL4_FETCH)
float FC_SHMEM_REAL4_FETCH(float *source,
                           fortran_integer_t *pe);
float
FC_SHMEM_REAL4_FETCH(float *source,
                     fortran_integer_t *pe)
{
    float val;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_assert(sizeof(float) == 4);

    shmem_internal_atomic_fetch(&val, (void *) source, 4, *pe, SHM_INTERNAL_INT32);
    shmem_internal_get_wait();

    return val;
}


#define FC_SHMEM_REAL8_FETCH FC_FUNC_(shmem_real8_fetch, SHMEM_REAL8_FETCH)
double FC_SHMEM_REAL8_FETCH(double *source,
                            fortran_integer_t *pe);
double
FC_SHMEM_REAL8_FETCH(double *source,
                     fortran_integer_t *pe)
{
    double val;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_assert(sizeof(double) == 8);

    shmem_internal_atomic_fetch(&val, (void *) source, 8, *pe, SHM_INTERNAL_INT64);
    shmem_internal_get_wait();

    return val;
}


#define FC_SHMEM_INT4_SET FC_FUNC_(shmem_int4_set, SHMEM_INT4_SET)
void FC_SHMEM_INT4_SET(int32_t *dest,
                       int32_t *value,
                       fortran_integer_t *pe);
void
FC_SHMEM_INT4_SET(int32_t *dest,
                   int32_t *value,
                   fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_atomic_set((void *) dest, (const void *) value, 4, *pe,
                              SHM_INTERNAL_INT32);
}


#define FC_SHMEM_INT8_SET FC_FUNC_(shmem_int8_set, SHMEM_INT8_SET)
void FC_SHMEM_INT8_SET(int64_t *dest,
                       int64_t *value,
                       fortran_integer_t *pe);
void
FC_SHMEM_INT8_SET(int64_t *dest,
                   int64_t *value,
                   fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_atomic_set((void *) dest, (const void *) value, 8, *pe,
                              SHM_INTERNAL_INT64);
}


#define FC_SHMEM_REAL4_SET FC_FUNC_(shmem_real4_set, SHMEM_REAL4_SET)
void FC_SHMEM_REAL4_SET(float *dest,
                        float *value,
                        fortran_integer_t *pe);
void
FC_SHMEM_REAL4_SET(float *dest,
                   float *value,
                   fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_assert(sizeof(float) == 4);

    shmem_internal_atomic_set((void *) dest, (const void *) value, 4, *pe,
                              SHM_INTERNAL_INT32);
}


#define FC_SHMEM_REAL8_SET FC_FUNC_(shmem_real8_set, SHMEM_REAL8_SET)
void FC_SHMEM_REAL8_SET(double *dest,
                        double *value,
                        fortran_integer_t *pe);
void
FC_SHMEM_REAL8_SET(double *dest,
                   double *value,
                   fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

    shmem_internal_assert(sizeof(double) == 8);

    shmem_internal_atomic_set((void *) dest, (const void *) value, 8, *pe,
                              SHM_INTERNAL_INT64);
}
