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

#include "mpp/shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"


void
shmem_float_p(float *addr, float value, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(addr, &value, sizeof(value), pe);
    shmem_internal_put_wait(ret);
}


void
shmem_double_p(double *addr, double value, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(addr, &value, sizeof(value), pe);
    shmem_internal_put_wait(ret);
}


void
shmem_longdouble_p(long double *addr, long double value, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(addr, &value, sizeof(value), pe);
    shmem_internal_put_wait(ret);
}


void
shmem_char_p(char *addr, char value, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(addr, &value, sizeof(value), pe);
    shmem_internal_put_wait(ret);
}


void
shmem_short_p(short *addr, short value, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(addr, &value, sizeof(value), pe);
    shmem_internal_put_wait(ret);
}


void
shmem_int_p(int *addr, int value, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(addr, &value, sizeof(value), pe);    
    shmem_internal_put_wait(ret);
}


void
shmem_long_p(long *addr, long value, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(addr, &value, sizeof(value), pe);
    shmem_internal_put_wait(ret);
}


void
shmem_longlong_p(long long *addr, long long value, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(addr, &value, sizeof(value), pe);
    shmem_internal_put_wait(ret);
}


float
shmem_float_g(float *addr, int pe)
{
    float tmp = 0.0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, sizeof(float) * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_double_put(double *target, const double *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, sizeof(double) * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_longdouble_put(long double *target, const long double *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, sizeof(long double) * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_char_put(char *target, const char *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, sizeof(char) * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_short_put(short *target, const short *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, sizeof(short) * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_int_put(int *target, const int *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, sizeof(int) * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_long_put(long *target, const long *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, sizeof(long) * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_longlong_put(long long *target, const long long *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, sizeof(long long) * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_put32(void *target, const void *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, 4 * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_put64(void *target, const void *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, 8 * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_put128(void *target, const void *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, 16 * len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_putmem(void *target, const void *source, size_t len, int pe)
{
    int ret;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    ret = shmem_internal_put(target, source, len, pe);
    shmem_internal_put_wait(ret);
}


void
shmem_float_get(float *target, const float *source, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, sizeof(float), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_double_iput(double *target, const double *source, ptrdiff_t tst,
                  ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, sizeof(double), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_longdouble_iput(long double *target, const long double *source,
                      ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, sizeof(long double), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_short_iput(short *target, const short *source, ptrdiff_t tst, ptrdiff_t sst,
                 size_t len, int pe)
{
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, sizeof(short), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_int_iput(int *target, const int *source, ptrdiff_t tst, ptrdiff_t sst,
               size_t len, int pe)
{
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, sizeof(int), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_long_iput(long *target, const long *source, ptrdiff_t tst,
                ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, sizeof(long), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_longlong_iput(long long *target, const long long *source, ptrdiff_t tst,
                    ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, sizeof(long long), pe);
	target += tst;
	source += sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_iput32(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst,
             size_t len, int pe)
{
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, sizeof(uint32_t), pe);
	target = (uint32_t*)target + tst;
	source = (uint32_t*)source + sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_iput64(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst,
             size_t len, int pe)
{
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, sizeof(uint64_t), pe);
	target = (uint64_t*)target + tst;
	source = (uint64_t*)source + sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_iput128(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    tst *= 16;
    sst *= 16;
    for ( ; len > 0 ; --len) {
        ret = shmem_internal_put(target, source, 16, pe);
	target = (uint8_t*)target + tst;
	source = (uint8_t*)source + sst;
    }
    shmem_internal_put_wait(ret);
}


void
shmem_float_iget(float *target, const float *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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
    if (!shmem_int_initialized) {
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

