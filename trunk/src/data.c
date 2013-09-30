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
#include <unistd.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"

#ifdef ENABLE_PROFILING

#pragma weak shmem_float_p = pshmem_float_p
#define shmem_float_p pshmem_float_p

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

#pragma weak shmem_complexf_put = pshmem_complexf_put
#define shmem_complexf_put pshmem_complexf_put

#pragma weak shmem_complexd_put = pshmem_complexd_put
#define shmem_complexd_put pshmem_complexd_put

#pragma weak shmem_complexf_get = pshmem_complexf_get
#define shmem_complexf_get pshmem_complexf_get

#pragma weak shmem_complexd_get = pshmem_complexd_get
#define shmem_complexd_get pshmem_complexd_get

#pragma weak shmem_putmem_ct = pshmem_putmem_ct
#define shmem_putmem_ct pshmem_putmem_ct

#pragma weak shmem_ct_create = pshmem_ct_create
#define shmem_ct_create pshmem_ct_create

#pragma weak shmem_ct_free = pshmem_ct_free
#define shmem_ct_free pshmem_ct_free

#pragma weak shmem_ct_get = pshmem_ct_get
#define shmem_ct_get pshmem_ct_get

#pragma weak shmem_ct_set = pshmem_ct_set
#define shmem_ct_set pshmem_ct_set

#pragma weak shmem_ct_wait = pshmem_ct_wait
#define shmem_ct_wait pshmem_ct_wait

#endif /* ENABLE_PROFILING */


void
shmem_float_p(float *addr, float value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_double_p(double *addr, double value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_longdouble_p(long double *addr, long double value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_char_p(char *addr, char value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_short_p(short *addr, short value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_int_p(int *addr, int value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_small(addr, &value, sizeof(value), pe);    
}


void
shmem_long_p(long *addr, long value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


void
shmem_longlong_p(long long *addr, long long value, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_small(addr, &value, sizeof(value), pe);
}


float
shmem_float_g(float *addr, int pe)
{
    float tmp = 0.0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


double
shmem_double_g(double *addr, int pe)
{
    double tmp = 0.0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}

long double
shmem_longdouble_g(long double *addr, int pe)
{
    long double tmp = 0.0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


char
shmem_char_g(char *addr, int pe)
{
    char tmp = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


short
shmem_short_g(short *addr, int pe)
{
    short tmp = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


int
shmem_int_g(int *addr, int pe)
{
    int tmp = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


long
shmem_long_g(long *addr, int pe)
{
    long tmp = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


long long
shmem_longlong_g(long long *addr, int pe)
{
    long long tmp = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(&tmp, addr, sizeof(tmp), pe);
    shmem_internal_get_wait();
    return tmp;
}


void
shmem_float_put(float *target, const float *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(float) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_double_put(double *target, const double *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(double) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_longdouble_put(long double *target, const long double *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(long double) * len, pe,
                          &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_char_put(char *target, const char *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(char) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_short_put(short *target, const short *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(short) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_int_put(int *target, const int *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(int) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_long_put(long *target, const long *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(long) * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_longlong_put(long long *target, const long long *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(long long) * len, pe,
                          &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_put32(void *target, const void *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, 4 * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_put64(void *target, const void *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, 8 * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_put128(void *target, const void *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, 16 * len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_putmem(void *target, const void *source, size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void
shmem_float_get(float *target, const float *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(float) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_double_get(double *target, const double *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(double) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_longdouble_get(long double *target, const long double *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(long double) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_char_get(char *target, const char *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(char) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_short_get(short *target, const short *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(short) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_int_get(int *target, const int *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(int) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_long_get(long *target, const long *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(long) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_longlong_get(long long *target, const long long *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(long long) * len, pe);
    shmem_internal_get_wait();
}


void
shmem_get32(void *target, const void *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, 4 * len, pe);
    shmem_internal_get_wait();
}


void
shmem_get64(void *target, const void *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, 8 * len, pe);
    shmem_internal_get_wait();
}


void
shmem_get128(void *target, const void *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, 16 * len, pe);
    shmem_internal_get_wait();
}


void
shmem_getmem(void *target, const void *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, len, pe);
    shmem_internal_get_wait();
}


void
shmem_float_iput(float *target, const float *source, ptrdiff_t tst, ptrdiff_t sst,
                 size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, sizeof(uint64_t), pe);
	target = (uint64_t*)target + tst;
	source = (uint64_t*)source + sst;
    }
}


void
shmem_iput128(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    tst *= 16;
    sst *= 16;
    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, 16, pe);
	target = (uint8_t*)target + tst;
	source = (uint8_t*)source + sst;
    }
    shmem_internal_get_wait();
}


void shmem_complexf_get(float complex * target,
                        const float complex * source, size_t nelems, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(float complex) * nelems, pe);
    shmem_internal_get_wait();
}


void shmem_complexd_get(double complex * target,
                        const double complex * source, size_t nelems, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, sizeof(complex double) * nelems, pe);
    shmem_internal_get_wait();
}


void shmem_complexf_put(float complex * target,
                        const float complex * source, size_t nelems, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(complex float) * nelems,
                          pe, &completion);
    shmem_internal_put_wait(&completion);
}


void shmem_complexd_put(double complex * target,
                        const double complex * source, size_t nelems, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, sizeof(double complex) * nelems,
                          pe, &completion);
    shmem_internal_put_wait(&completion);
}


void shmem_putmem_ct(shmem_ct_t ct, void *target, const void *source,
                     size_t len, int pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_ct_nb(ct, target, source, len, pe, &completion);
    shmem_internal_put_wait(&completion);
}


void shmem_ct_create(shmem_ct_t *ct)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_ct_create(ct);
}


void shmem_ct_free(shmem_ct_t *ct)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_ct_free(ct);
}


long shmem_ct_get(shmem_ct_t ct)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_ct_get(ct);
}


void shmem_ct_set(shmem_ct_t ct, long value)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_ct_set(ct, value);
}


void shmem_ct_wait(shmem_ct_t ct, long wait_for)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_ct_wait(ct, wait_for);
}
