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
#include <unistd.h>

#include "shmem.h"
#include "shmemx.h"
#include "shmem_internal.h"
#include "shmem_comm.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_float_p = pshmem_float_p
#define shmem_float_p pshmem_float_p

#pragma weak shmem_double_p = pshmem_double_p
#define shmem_double_p pshmem_double_p

#pragma weak shmem_longdouble_p = pshmem_longdouble_p
#define shmem_longdouble_p pshmem_longdouble_p

#pragma weak shmem_char_p = pshmem_char_p
#define shmem_char_p pshmem_char_p

#pragma weak shmem_short_p = pshmem_short_p
#define shmem_short_p pshmem_short_p

#pragma weak shmem_int_p = pshmem_int_p
#define shmem_int_p pshmem_int_p

#pragma weak shmem_long_p = pshmem_long_p
#define shmem_long_p pshmem_long_p

#pragma weak shmem_longlong_p = pshmem_longlong_p
#define shmem_longlong_p pshmem_longlong_p

#pragma weak shmem_float_g = pshmem_float_g
#define shmem_float_g pshmem_float_g

#pragma weak shmem_double_g = pshmem_double_g
#define shmem_double_g pshmem_double_g

#pragma weak shmem_char_g = pshmem_char_g
#define shmem_char_g pshmem_char_g

#pragma weak shmem_short_g = pshmem_short_g
#define shmem_short_g pshmem_short_g

#pragma weak shmem_int_g = pshmem_int_g
#define shmem_int_g pshmem_int_g

#pragma weak shmem_long_g = pshmem_long_g
#define shmem_long_g pshmem_long_g

#pragma weak shmem_longdouble_g = pshmem_longdouble_g
#define shmem_longdouble_g pshmem_longdouble_g

#pragma weak shmem_longlong_g = pshmem_longlong_g
#define shmem_longlong_g pshmem_longlong_g

#pragma weak shmem_float_put = pshmem_float_put
#define shmem_float_put pshmem_float_put

#pragma weak shmem_double_put = pshmem_double_put
#define shmem_double_put pshmem_double_put

#pragma weak shmem_longdouble_put = pshmem_longdouble_put
#define shmem_longdouble_put pshmem_longdouble_put

#pragma weak shmem_char_put = pshmem_char_put
#define shmem_char_put pshmem_char_put

#pragma weak shmem_short_put = pshmem_short_put
#define shmem_short_put pshmem_short_put

#pragma weak shmem_int_put = pshmem_int_put
#define shmem_int_put pshmem_int_put

#pragma weak shmem_long_put = pshmem_long_put
#define shmem_long_put pshmem_long_put

#pragma weak shmem_longlong_put = pshmem_longlong_put
#define shmem_longlong_put pshmem_longlong_put

#pragma weak shmem_put8 = pshmem_put8
#define shmem_put8 pshmem_put8

#pragma weak shmem_put16 = pshmem_put16
#define shmem_put16 pshmem_put16

#pragma weak shmem_put32 = pshmem_put32
#define shmem_put32 pshmem_put32

#pragma weak shmem_put64 = pshmem_put64
#define shmem_put64 pshmem_put64

#pragma weak shmem_put128 = pshmem_put128
#define shmem_put128 pshmem_put128

#pragma weak shmem_putmem = pshmem_putmem
#define shmem_putmem pshmem_putmem

#pragma weak shmem_float_get = pshmem_float_get
#define shmem_float_get pshmem_float_get

#pragma weak shmem_double_get = pshmem_double_get
#define shmem_double_get pshmem_double_get

#pragma weak shmem_longdouble_get = pshmem_longdouble_get
#define shmem_longdouble_get pshmem_longdouble_get

#pragma weak shmem_char_get = pshmem_char_get
#define shmem_char_get pshmem_char_get

#pragma weak shmem_short_get = pshmem_short_get
#define shmem_short_get pshmem_short_get

#pragma weak shmem_int_get = pshmem_int_get
#define shmem_int_get pshmem_int_get

#pragma weak shmem_long_get = pshmem_long_get
#define shmem_long_get pshmem_long_get

#pragma weak shmem_longlong_get = pshmem_longlong_get
#define shmem_longlong_get pshmem_longlong_get

#pragma weak shmem_get8 = pshmem_get8
#define shmem_get8 pshmem_get8

#pragma weak shmem_get16 = pshmem_get16
#define shmem_get16 pshmem_get16

#pragma weak shmem_get32 = pshmem_get32
#define shmem_get32 pshmem_get32

#pragma weak shmem_get64 = pshmem_get64
#define shmem_get64 pshmem_get64

#pragma weak shmem_get128 = pshmem_get128
#define shmem_get128 pshmem_get128

#pragma weak shmem_getmem = pshmem_getmem
#define shmem_getmem pshmem_getmem

#pragma weak shmem_float_iput = pshmem_float_iput
#define shmem_float_iput pshmem_float_iput

#pragma weak shmem_double_iput = pshmem_double_iput
#define shmem_double_iput pshmem_double_iput

#pragma weak shmem_longdouble_iput = pshmem_longdouble_iput
#define shmem_longdouble_iput pshmem_longdouble_iput

#pragma weak shmem_short_iput = pshmem_short_iput
#define shmem_short_iput pshmem_short_iput

#pragma weak shmem_int_iput = pshmem_int_iput
#define shmem_int_iput pshmem_int_iput

#pragma weak shmem_long_iput = pshmem_long_iput
#define shmem_long_iput pshmem_long_iput

#pragma weak shmem_longlong_iput = pshmem_longlong_iput
#define shmem_longlong_iput pshmem_longlong_iput

#pragma weak shmem_iput32 = pshmem_iput32
#define shmem_iput32 pshmem_iput32

#pragma weak shmem_iput64 = pshmem_iput64
#define shmem_iput64 pshmem_iput64

#pragma weak shmem_iput128 = pshmem_iput128
#define shmem_iput128 pshmem_iput128

#pragma weak shmem_float_iget = pshmem_float_iget
#define shmem_float_iget pshmem_float_iget

#pragma weak shmem_double_iget = pshmem_double_iget
#define shmem_double_iget pshmem_double_iget

#pragma weak shmem_longdouble_iget = pshmem_longdouble_iget
#define shmem_longdouble_iget pshmem_longdouble_iget

#pragma weak shmem_short_iget = pshmem_short_iget
#define shmem_short_iget pshmem_short_iget

#pragma weak shmem_int_iget = pshmem_int_iget
#define shmem_int_iget pshmem_int_iget

#pragma weak shmem_long_iget = pshmem_long_iget
#define shmem_long_iget pshmem_long_iget

#pragma weak shmem_longlong_iget = pshmem_longlong_iget
#define shmem_longlong_iget pshmem_longlong_iget

#pragma weak shmem_iget32 = pshmem_iget32
#define shmem_iget32 pshmem_iget32

#pragma weak shmem_iget64 = pshmem_iget64
#define shmem_iget64 pshmem_iget64

#pragma weak shmem_iget128 = pshmem_iget128
#define shmem_iget128 pshmem_iget128

#pragma weak shmem_float_put_nbi = pshmem_float_put_nbi
#define shmem_float_put_nbi pshmem_float_put_nbi

#pragma weak shmem_double_put_nbi = pshmem_double_put_nbi
#define shmem_double_put_nbi pshmem_double_put_nbi

#pragma weak shmem_longdouble_put_nbi = pshmem_longdouble_put_nbi
#define shmem_longdouble_put_nbi pshmem_longdouble_put_nbi

#pragma weak shmem_char_put_nbi = pshmem_char_put_nbi
#define shmem_char_put_nbi pshmem_char_put_nbi

#pragma weak shmem_putmem_nbi = pshmem_putmem_nbi
#define shmem_putmem_nbi pshmem_putmem_nbi

#pragma weak shmem_short_put_nbi = pshmem_short_put_nbi
#define shmem_short_put_nbi pshmem_short_put_nbi

#pragma weak shmem_int_put_nbi = pshmem_int_put_nbi
#define shmem_int_put_nbi pshmem_int_put_nbi

#pragma weak shmem_long_put_nbi = pshmem_long_put_nbi
#define shmem_long_put_nbi pshmem_long_put_nbi

#pragma weak shmem_longlong_put_nbi = pshmem_longlong_put_nbi
#define shmem_longlong_put_nbi pshmem_longlong_put_nbi

#pragma weak shmem_put8_nbi = pshmem_put8_nbi
#define shmem_put8_nbi pshmem_put8_nbi

#pragma weak shmem_put16_nbi = pshmem_put16_nbi
#define shmem_put16_nbi pshmem_put16_nbi

#pragma weak shmem_put32_nbi = pshmem_put32_nbi
#define shmem_put32_nbi pshmem_put32_nbi

#pragma weak shmem_put64_nbi =- pshmem_put64_nbi
#define shmem_put64_nbi pshmem_put64_nbi

#pragma weak shmem_put128_nbi = pshmem_put128_nbi
#define shmem_put128_nbi pshmem_put128_nbi

#pragma weak shmem_float_get_nbi = pshmem_float_get_nbi
#define shmem_float_get_nbi pshmem_float_get_nbi

#pragma weak shmem_double_get_nbi = pshmem_double_get_nbi
#define shmem_double_get_nbi pshmem_double_get_nbi

#pragma weak shmem_longdouble_get_nbi = pshmem_longdouble_get_nbi
#define shmem_longdouble_get_nbi pshmem_longdouble_get_nbi

#pragma weak shmem_char_get_nbi = pshmem_char_get_nbi
#define shmem_char_get_nbi pshmem_char_get_nbi

#pragma weak shmem_getmem_nbi = pshmem_getmem_nbi
#define shmem_getmem_nbi pshmem_getmem_nbi

#pragma weak shmem_short_get_nbi = pshmem_short_get_nbi
#define shmem_short_get_nbi pshmem_short_get_nbi

#pragma weak shmem_int_get_nbi = pshmem_int_get_nbi
#define shmem_int_get_nbi pshmem_int_get_nbi

#pragma weak shmem_long_get_nbi = pshmem_long_get_nbi
#define shmem_long_get_nbi pshmem_long_get_nbi

#pragma weak shmem_longlong_get_nbi = pshmem_longlong_get_nbi
#define shmem_longlong_get_nbi pshmem_longlong_get_nbi

#pragma weak shmem_get8_nbi = pshmem_get8_nbi
#define shmem_get8_nbi pshmem_get8_nbi

#pragma weak shmem_get16_nbi = pshmem_get16_nbi
#define shmem_get16_nbi pshmem_get16_nbi

#pragma weak shmem_get32_nbi = pshmem_get32_nbi
#define shmem_get32_nbi pshmem_get32_nbi

#pragma weak shmem_get64_nbi = pshmem_get64_nbi
#define shmem_get64_nbi pshmem_get64_nbi

#pragma weak shmem_get128_nbi = pshmem_get128_nbi
#define shmem_get128_nbi pshmem_get128_nbi

#pragma weak shmemx_putmem_ct = pshmemx_putmem_ct
#define shmemx_putmem_ct pshmemx_putmem_ct

#pragma weak shmemx_getmem_ct = pshmemx_getmem_ct
#define shmemx_getmem_ct pshmemx_getmem_ct

#pragma weak shmemx_ct_create = pshmemx_ct_create
#define shmemx_ct_create pshmemx_ct_create

#pragma weak shmemx_ct_free = pshmemx_ct_free
#define shmemx_ct_free pshmemx_ct_free

#pragma weak shmemx_ct_get = pshmemx_ct_get
#define shmemx_ct_get pshmemx_ct_get

#pragma weak shmemx_ct_set = pshmemx_ct_set
#define shmemx_ct_set pshmemx_ct_set

#pragma weak shmemx_ct_wait = pshmemx_ct_wait
#define shmemx_ct_wait pshmemx_ct_wait

#endif /* ENABLE_PROFILING */


void
shmem_float_p(float *addr, float value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_double_p(double *addr, double value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_longdouble_p(long double *addr, long double value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_char_p(char *addr, char value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_short_p(short *addr, short value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_int_p(int *addr, int value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_small(addr, &value, sizeof(value), pe);    
}


void
shmem_long_p(long *addr, long value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_longlong_p(long long *addr, long long value, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


float
shmem_float_g(const float *addr, int pe)
{
    float tmp = 0.0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


double
shmem_double_g(const double *addr, int pe)
{
    double tmp = 0.0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}

long double
shmem_longdouble_g(const long double *addr, int pe)
{
    long double tmp = 0.0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


char
shmem_char_g(const char *addr, int pe)
{
    char tmp = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


short
shmem_short_g(const short *addr, int pe)
{
    short tmp = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


int
shmem_int_g(const int *addr, int pe)
{
    int tmp = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


long
shmem_long_g(const long *addr, int pe)
{
    long tmp = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


long long
shmem_longlong_g(const long long *addr, int pe)
{
    long long tmp = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


void
shmem_float_put(float *target, const float *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, sizeof(float) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_double_put(double *target, const double *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, sizeof(double) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_longdouble_put(long double *target, const long double *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, sizeof(long double) * len, pe,
                          &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_char_put(char *dest, const char *source, size_t nelems, int pe)
{
  long completion = 0;

  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, sizeof(char)*nelems, pe, &completion);
  shmem_internal_put_wait(&completion);
}


void
shmem_short_put(short *target, const short *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, sizeof(short) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_int_put(int *target, const int *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, sizeof(int) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_long_put(long *target, const long *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, sizeof(long) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_longlong_put(long long *target, const long long *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, sizeof(long long) * len, pe,
                          &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_put8(void *dest, const void *source, size_t nelems, int pe)
{
  long completion = 0;

  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, nelems, pe, &completion);
  shmem_internal_put_wait(&completion);
}


void
shmem_put16(void *dest, const void *source, size_t nelems, int pe)
{
  long completion = 0;

  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, 2 * nelems, pe, &completion);
  shmem_internal_put_wait(&completion);
}


void
shmem_put32(void *target, const void *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, 4 * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_put64(void *target, const void *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, 8 * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_put128(void *target, const void *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, 16 * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_putmem(void *target, const void *source, size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_nb(target, source, len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_float_put_nbi(float *dest, const float *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, sizeof(float)*nelems, pe, NULL);
}


void
shmem_double_put_nbi(double *dest, const double *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, sizeof(double)*nelems, pe, NULL);
}


void
shmem_longdouble_put_nbi(long double *dest, const long double *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, sizeof(long double)*nelems, pe, NULL);
}


void
shmem_char_put_nbi(char *dest, const char *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, sizeof(char)*nelems, pe, NULL);
}


void
shmem_putmem_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, nelems, pe, NULL);
}


void
shmem_short_put_nbi(short *dest, const short *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, sizeof(short)*nelems, pe, NULL);
}


void
shmem_int_put_nbi(int *dest, const int *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, sizeof(int)*nelems, pe, NULL);
}


void
shmem_long_put_nbi(long *dest, const long *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, sizeof(long)*nelems, pe, NULL);
}


void
shmem_longlong_put_nbi(long long *dest, const long long *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, sizeof(long long)*nelems, pe, NULL);
}


void
shmem_put8_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, nelems, pe, NULL);
}


void
shmem_put16_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, 2 * nelems, pe, NULL);
}


void
shmem_put32_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, 4*nelems, pe, NULL);
}


void
shmem_put64_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, 8*nelems, pe, NULL);
}


void
shmem_put128_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_put_nb(dest, source, 16*nelems, pe, NULL);
}


void
shmem_float_get(float *target, const float *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, sizeof(float) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_double_get(double *target, const double *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, sizeof(double) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_longdouble_get(long double *target, const long double *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, sizeof(long double) * len, pe);
    shmem_internal_get_wait();
}



void
shmem_char_get(char *target, const char *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, sizeof(char) * len, pe);
    shmem_internal_get_wait();
}



void
shmem_short_get(short *target, const short *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, sizeof(short) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_int_get(int *target, const int *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, sizeof(int) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_long_get(long *target, const long *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, sizeof(long) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_longlong_get(long long *target, const long long *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, sizeof(long long) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_get8(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, nelems, pe);
  shmem_internal_get_wait();
}


void
shmem_get16(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, 2 * nelems, pe);
  shmem_internal_get_wait();
}


void
shmem_get32(void *target, const void *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, 4 * len, pe);
    shmem_internal_get_wait();
}


void
shmem_get64(void *target, const void *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, 8 * len, pe);
    shmem_internal_get_wait();
}


void
shmem_get128(void *target, const void *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, 16 * len, pe);
    shmem_internal_get_wait();
}


void
shmem_getmem(void *target, const void *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get(target, source, len, pe);
    shmem_internal_get_wait();
}


void
shmem_float_get_nbi(float *dest, const float *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, sizeof(float)*nelems, pe);
}


void
shmem_double_get_nbi(double *dest, const double *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, sizeof(double)*nelems, pe);
}


void
shmem_longdouble_get_nbi(long double *dest, const long double *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, sizeof(long double)*nelems, pe);
}


void
shmem_char_get_nbi(char *dest, const char *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, sizeof(char)*nelems, pe);
}


void
shmem_getmem_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, nelems, pe);
}


void
shmem_short_get_nbi(short *dest, const short *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, sizeof(short)*nelems, pe);
}


void
shmem_int_get_nbi(int *dest, const int *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, sizeof(int)*nelems, pe);
}


void
shmem_long_get_nbi(long *dest, const long *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, sizeof(long)*nelems, pe);
}


void
shmem_longlong_get_nbi(long long *dest, const long long *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, sizeof(long long)*nelems, pe);
}


void
shmem_get8_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, nelems, pe);
}


void
shmem_get16_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, 2 * nelems, pe);
}


void
shmem_get32_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, 4*nelems, pe);
}


void
shmem_get64_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, 8*nelems, pe);
}


void
shmem_get128_nbi(void *dest, const void *source, size_t nelems, int pe)
{
  SHMEM_ERR_CHECK_INITIALIZED();

  shmem_internal_get(dest, source, 16*nelems, pe);
}


void
shmemx_getmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_get_ct(ct, target, source, len, pe);
    shmem_internal_get_wait();
}


void
shmem_float_iput(float *target, const float *source, ptrdiff_t tst, ptrdiff_t sst,
                 size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(float), pe);
	target += tst;
	source += sst;
    }
}


void
shmem_double_iput(double *target, const double *source, ptrdiff_t tst,
                  ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(double), pe);
	target += tst;
	source += sst;
    }
}


void
shmem_longdouble_iput(long double *target, const long double *source,
                      ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(long double), pe);
	target += tst;
	source += sst;
    }
}


void
shmem_short_iput(short *target, const short *source, ptrdiff_t tst, ptrdiff_t sst,
                 size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(short), pe);
	target += tst;
	source += sst;
    }
}


void
shmem_int_iput(int *target, const int *source, ptrdiff_t tst, ptrdiff_t sst,
               size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(int), pe);
	target += tst;
	source += sst;
    }
}


void
shmem_long_iput(long *target, const long *source, ptrdiff_t tst,
                ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(long), pe);
	target += tst;
	source += sst;
    }
}


void
shmem_longlong_iput(long long *target, const long long *source, ptrdiff_t tst,
                    ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(long long), pe);
	target += tst;
	source += sst;
    }
}


void
shmem_iput32(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst,
             size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(uint32_t), pe);
	target = (uint32_t*)target + tst;
	source = (uint32_t*)source + sst;
    }
}


void
shmem_iput64(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst,
             size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(uint64_t), pe);
	target = (uint64_t*)target + tst;
	source = (uint64_t*)source + sst;
    }
}


void
shmem_iput128(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    tst *= 16;
    sst *= 16;
    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, 16, pe);
	target = (uint8_t*)target + tst;
	source = (uint8_t*)source + sst;
    }
}


void
shmem_float_iget(float *target, const float *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, sizeof(float), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_get_wait();
}


void
shmem_double_iget(double *target, const double *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, sizeof(double), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_get_wait();
}


void
shmem_longdouble_iget(long double *target, const long double *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, sizeof(long double), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_get_wait();
}


void
shmem_short_iget(short *target, const short *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, sizeof(short), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_get_wait();
}


void
shmem_int_iget(int *target, const int *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, sizeof(int), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_get_wait();
}


void
shmem_long_iget(long *target, const long *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, sizeof(long), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_get_wait();
}


void
shmem_longlong_iget(long long *target, const long long *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, sizeof(long long), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_get_wait();
}


void
shmem_iget32(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, sizeof(uint32_t), pe);
	target = (uint32_t*)target + tst;
	source = (uint32_t*)source + sst;
    }
    shmem_internal_get_wait();
}


void
shmem_iget64(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, sizeof(uint64_t), pe);
	target = (uint64_t*)target + tst;
	source = (uint64_t*)source + sst;
    }
    shmem_internal_get_wait();
}


void
shmem_iget128(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    tst *= 16;
    sst *= 16;
    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, 16, pe);
	target = (uint8_t*)target + tst;
	source = (uint8_t*)source + sst;
    }
    shmem_internal_get_wait();
}


void shmemx_putmem_ct(shmemx_ct_t ct, void *target, const void *source,
                     size_t len, int pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_put_ct_nb(ct, target, source, len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void shmemx_ct_create(shmemx_ct_t *ct)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_ct_create(ct);
}


void shmemx_ct_free(shmemx_ct_t *ct)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_ct_free(ct);
}


long shmemx_ct_get(shmemx_ct_t ct)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    return shmem_internal_ct_get(ct);
}


void shmemx_ct_set(shmemx_ct_t ct, long value)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_ct_set(ct, value);
}


void shmemx_ct_wait(shmemx_ct_t ct, long wait_for)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_ct_wait(ct, wait_for);
}
