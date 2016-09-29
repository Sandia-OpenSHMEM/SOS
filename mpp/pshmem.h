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

#ifndef PSHMEM_H
#define PSHMEM_H

#include <shmemx.h>
#include <stddef.h>
#include <complex.h>

#ifdef __cplusplus
#ifndef complex
#define complex _Complex
#define SHMEM_UNDEF_COMPLEX
#endif
extern "C" {
#endif

/* 8.1: OpenSHMEM Library API Library Setup, Exit, and Query Routines */
void pshmem_init(void);
void pshmem_finalize(void);
void pshmem_global_exit(int status);
int pshmem_n_pes(void);
int pshmem_my_pe(void);

int pshmem_pe_accessible(int pe);
int pshmem_addr_accessible(const void *addr, int pe);
void *pshmem_ptr(const void *target, int pe);

void pshmem_info_get_version(int *major, int *minor);
void pshmem_info_get_name(char *name);

/* 8.2: Memory Management Routines */
void *pshmem_malloc(size_t size);
void *pshmem_align(size_t alignment, size_t size);
void *pshmem_realloc(void *ptr, size_t size);
void pshmem_free(void *ptr);

/* 8.3: Elemental Data Put Routines */
void pshmem_float_p(float *addr, float value, int pe);
void pshmem_double_p(double *addr, double value, int pe);
void pshmem_longdouble_p(long double *addr, long double value, int pe);
void pshmem_char_p(char *addr, char value, int pe);
void pshmem_short_p(short *addr, short value, int pe);
void pshmem_int_p(int *addr, int value, int pe);
void pshmem_long_p(long *addr, long value, int pe);
void pshmem_longlong_p(long long *addr, long long value, int pe);

/* 8.3: Block Data Put Routines */
void pshmem_float_put(float *target, const float *source, size_t len, int pe);
void pshmem_double_put(double *target, const double *source, size_t len,
                      int pe);
void pshmem_longdouble_put(long double *target, const long double *source,
                          size_t len, int pe);
void pshmem_char_put(char *dest, const char *source, size_t nelems, int pe);
void pshmem_short_put(short *target, const short *source, size_t len, int pe);
void pshmem_int_put(int *target, const int *source, size_t len, int pe);
void pshmem_long_put(long *target, const long *source, size_t len, int pe);
void pshmem_longlong_put(long long *target, const long long *source,
                        size_t len, int pe);
void pshmem_put8(void *dest, const void *source, size_t nelems, int pe);
void pshmem_put16(void *dest, const void *source, size_t nelems, int pe);
void pshmem_put32(void *target, const void *source, size_t len, int pe);
void pshmem_put64(void *target, const void *source, size_t len, int pe);
void pshmem_put128(void *target, const void *source, size_t len, int pe);
void pshmem_putmem(void *target, const void *source, size_t len, int pe);

/* 8.3: Strided Put Routines */
void pshmem_float_iput(float *target, const float *source, ptrdiff_t tst,
                      ptrdiff_t sst, size_t len, int pe);
void pshmem_double_iput(double *target, const double *source, ptrdiff_t tst,
                       ptrdiff_t sst, size_t len, int pe);
void pshmem_longdouble_iput(long double *target, const long double *source,
                           ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void pshmem_char_iput(char *target, const char *source, ptrdiff_t tst,
                      ptrdiff_t sst, size_t len, int pe);
void pshmem_short_iput(short *target, const short *source, ptrdiff_t tst,
                      ptrdiff_t sst, size_t len, int pe);
void pshmem_int_iput(int *target, const int *source, ptrdiff_t tst,
                    ptrdiff_t sst, size_t len, int pe);
void pshmem_long_iput(long *target, const long *source, ptrdiff_t tst,
                     ptrdiff_t sst, size_t len, int pe);
void pshmem_longlong_iput(long long *target, const long long *source,
                         ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void pshmem_iput32(void *target, const void *source, ptrdiff_t tst,
                  ptrdiff_t sst, size_t len, int pe);
void pshmem_iput64(void *target, const void *source, ptrdiff_t tst,
                  ptrdiff_t sst, size_t len, int pe);
void pshmem_iput128(void *target, const void *source, ptrdiff_t tst,
                   ptrdiff_t sst, size_t len, int pe);

/* 8.3: Elemental Data Get Routines */
float pshmem_float_g(const float *addr, int pe);
double pshmem_double_g(const double *addr, int pe);
long double pshmem_longdouble_g(const long double *addr, int pe);
char pshmem_char_g(const char *addr, int pe);
short pshmem_short_g(const short *addr, int pe);
int pshmem_int_g(const int *addr, int pe);
long pshmem_long_g(const long *addr, int pe);
long long pshmem_longlong_g(const long long *addr, int pe);

/* 8.3: Block Data Get Routines */
void pshmem_float_get(float *target, const float *source, size_t len, int pe);
void pshmem_double_get(double *target, const double *source, size_t len,
                      int pe);
void pshmem_longdouble_get(long double *target, const long double *source,
                          size_t len, int pe);
void pshmem_char_get(char *dest, const char *source, size_t nelems, int pe);
void pshmem_short_get(short *target, const short *source, size_t len, int pe);
void pshmem_int_get(int *target, const int *source, size_t len, int pe);
void pshmem_long_get(long *target, const long *source, size_t len, int pe);
void pshmem_longlong_get(long long *target, const long long *source,
                        size_t len, int pe);
void pshmem_get8(void *dest, const void *source, size_t nelems, int pe);
void pshmem_get16(void *dest, const void *source, size_t nelems, int pe);
void pshmem_get32(void *target, const void *source, size_t len, int pe);
void pshmem_get64(void *target, const void *source, size_t len, int pe);
void pshmem_get128(void *target, const void *source, size_t len, int pe);
void pshmem_getmem(void *target, const void *source, size_t len, int pe);

/* 8.3: Strided Get Routines */
void pshmem_float_iget(float *target, const float *source, ptrdiff_t tst,
                      ptrdiff_t sst, size_t len, int pe);
void pshmem_double_iget(double *target, const double *source, ptrdiff_t tst,
                       ptrdiff_t sst, size_t len, int pe);
void pshmem_longdouble_iget(long double *target, const long double *source,
                           ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void pshmem_char_iget(char *target, const char *source, ptrdiff_t tst,
                      ptrdiff_t sst, size_t len, int pe);
void pshmem_short_iget(short *target, const short *source, ptrdiff_t tst,
                      ptrdiff_t sst, size_t len, int pe);
void pshmem_int_iget(int *target, const int *source, ptrdiff_t tst,
                    ptrdiff_t sst, size_t len, int pe);
void pshmem_long_iget(long *target, const long *source, ptrdiff_t tst,
                     ptrdiff_t sst, size_t len, int pe);
void pshmem_longlong_iget(long long *target, const long long *source,
                         ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void pshmem_iget32(void *target, const void *source, ptrdiff_t tst,
                  ptrdiff_t sst, size_t len, int pe);
void pshmem_iget64(void *target, const void *source, ptrdiff_t tst,
                  ptrdiff_t sst, size_t len, int pe);
void pshmem_iget128(void *target, const void *source, ptrdiff_t tst,
                   ptrdiff_t sst, size_t len, int pe);

/* 8.4: Nonblocking remote memory access routines -- Put */
void pshmem_float_put_nbi(float *dest, const float *source, size_t nelems,
                         int pe);
void pshmem_double_put_nbi(double *dest, const double *source, size_t nelems,
                          int pe);
void pshmem_longdouble_put_nbi(long double *dest, const long double *source,
                              size_t nelems, int pe);
void pshmem_putmem_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_char_put_nbi(char *dest, const char *source, size_t nelems, int pe);
void pshmem_short_put_nbi(short *dest, const short *source, size_t nelems,
                         int pe);
void pshmem_int_put_nbi(int *dest, const int *source, size_t nelems, int pe);
void pshmem_long_put_nbi(long *dest, const long *source, size_t nelems, int pe);
void pshmem_longlong_put_nbi(long long *dest, const long long *source,
                            size_t nelems, int pe);
void pshmem_put8_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_put16_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_put32_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_put64_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_put128_nbi(void *dest, const void *source, size_t nelems, int pe);

/* 8.4: Nonblocking remote memory access routines -- Get */
void pshmem_float_get_nbi(float *dest, const float *source, size_t nelems,
                         int pe);
void pshmem_double_get_nbi(double *dest, const double *source, size_t nelems,
                          int pe);
void pshmem_longdouble_get_nbi(long double *dest, const long double *source,
                              size_t nelems, int pe);
void pshmem_getmem_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_char_get_nbi(char *dest, const char *source, size_t nelems, int pe);
void pshmem_short_get_nbi(short *dest, const short *source, size_t nelems,
                         int pe);
void pshmem_int_get_nbi(int *dest, const int *source, size_t nelems, int pe);
void pshmem_long_get_nbi(long *dest, const long *source, size_t nelems, int pe);
void pshmem_longlong_get_nbi(long long *dest, const long long *source,
                            size_t nelems, int pe);
void pshmem_get8_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_get16_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_get32_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_get64_nbi(void *dest, const void *source, size_t nelems, int pe);
void pshmem_get128_nbi(void *dest, const void *source, size_t nelems, int pe);

/* 8.4: Atomic Memory fetch-and-operate Routines -- Swap */
float pshmem_float_swap(float *target, float value, int pe);
double pshmem_double_swap(double *target, double value, int pe);
int pshmem_int_swap(int *target, int value, int pe);
long pshmem_long_swap(long *target, long value, int pe);
long long pshmem_longlong_swap(long long *target, long long value, int pe);
long pshmem_swap(long *target, long value, int pe);

/* 8.4: Atomic Memory fetch-and-operate Routines -- Cswap */
int pshmem_int_cswap(int *target, int cond, int value, int pe);
long pshmem_long_cswap(long *target, long cond, long value, int pe);
long long pshmem_longlong_cswap(long long * target, long long cond, 
                          long long value, int pe);

/* 8.4: Atomic Memory fetch-and-operate Routines -- Fetch and Add */
int pshmem_int_fadd(int *target, int value, int pe);
long pshmem_long_fadd(long *target, long value, int pe);
long long pshmem_longlong_fadd(long long *target, long long value, int pe);

/* 8.4: Atomic Memory fetch-and-operate Routines -- Fetch and Increment */
int pshmem_int_finc(int *target, int pe);
long pshmem_long_finc(long *target, int pe);
long long pshmem_longlong_finc(long long *target, int pe);

/* 8.4: Atomic Memory Operation Routines -- Add */
void pshmem_int_add(int *target, int value, int pe);
void pshmem_long_add(long *target, long value, int pe);
void pshmem_longlong_add(long long *target, long long value, int pe);

/* 8.4: Atomic Memory Operation Routines -- Increment */
void pshmem_int_inc(int *target, int pe);
void pshmem_long_inc(long *target, int pe);
void pshmem_longlong_inc(long long *target, int pe);

/* 8.4: Atomic fetch */
int pshmem_int_fetch(const int *source, int pe);
long pshmem_long_fetch(const long *source, int pe);
long long pshmem_longlong_fetch(const long long *source, int pe);
float pshmem_float_fetch(const float *source, int pe);
double pshmem_double_fetch(const double *source, int pe);

/* 8.4: Atomic set */
void pshmem_int_set(int *dest, int value, int pe);
void pshmem_long_set(long *dest, long value, int pe);
void pshmem_longlong_set(long long *dest, long long value, int pe);
void pshmem_float_set(float *dest, float value, int pe);
void pshmem_double_set(double *dest, double value, int pe);

/* 8.5: Barrier Synchronization Routines */
void pshmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync);
void pshmem_barrier_all(void);

/* 8.5: Reduction Routines */
void pshmem_short_and_to_all(short *target, const short *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            short *pWrk, long *pSync);
void pshmem_int_and_to_all(int *target, const int *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          int *pWrk, long *pSync);
void pshmem_long_and_to_all(long *target, const long *source, int nreduce,
                           int PE_start, int logPE_stride, int PE_size,
                           long *pWrk, long *pSync);
void pshmem_longlong_and_to_all(long long *target, const long long *source,
                               int nreduce, int PE_start, int logPE_stride,
                               int PE_size, long long *pWrk, long *pSync);

void pshmem_short_or_to_all(short *target, const short *source, int nreduce,
                           int PE_start, int logPE_stride, int PE_size, 
                           short *pWrk, long *pSync);
void pshmem_int_or_to_all(int *target, const int *source, int nreduce,
                         int PE_start, int logPE_stride, int PE_size, 
                         int *pWrk, long *pSync);
void pshmem_long_or_to_all(long *target, const long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size, 
                          long *pWrk, long *pSync);
void pshmem_longlong_or_to_all(long long *target, const long long *source,
                              int nreduce, int PE_start, int logPE_stride,
                              int PE_size, long long *pWrk, long *pSync);

void pshmem_short_xor_to_all(short *target, const short *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size, 
                            short *pWrk, long *pSync);
void pshmem_int_xor_to_all(int *target, const int *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size, 
                          int *pWrk, long *pSync);
void pshmem_long_xor_to_all(long *target, const long *source, int nreduce,
                           int PE_start, int logPE_stride, int PE_size, 
                           long *pWrk, long *pSync);
void pshmem_longlong_xor_to_all(long long *target, const long long *source,
                               int nreduce, int PE_start, int logPE_stride,
                               int PE_size, long long *pWrk, long *pSync);

void pshmem_float_min_to_all(float *target, const float *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size, 
                            float *pWrk, long *pSync);
void pshmem_double_min_to_all(double *target, const double *source, int nreduce,
                             int PE_start, int logPE_stride, int PE_size, 
                             double *pWrk, long *pSync);
void pshmem_longdouble_min_to_all(long double *target, const long double *source,
                                 int nreduce, int PE_start, int logPE_stride,
                                 int PE_size, long double *pWrk, long *pSync);
void pshmem_short_min_to_all(short *target, const short *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size, 
                            short *pWrk, long *pSync);
void pshmem_int_min_to_all(int *target, const int *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size, 
                          int *pWrk, long *pSync);
void pshmem_long_min_to_all(long *target, const long *source, int nreduce,
                           int PE_start, int logPE_stride, int PE_size, 
                           long *pWrk, long *pSync);
void pshmem_longlong_min_to_all(long long *target, const long long *source,
                               int nreduce, int PE_start, int logPE_stride,
                               int PE_size, long long *pWrk, long *pSync);

void pshmem_float_max_to_all(float *target, const float *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            float *pWrk, long *pSync);
void pshmem_double_max_to_all(double *target, const double *source, int nreduce,
                             int PE_start, int logPE_stride, int PE_size,
                             double *pWrk, long *pSync);
void pshmem_longdouble_max_to_all(long double *target, const long double *source,
                                 int nreduce, int PE_start, int logPE_stride,
                                 int PE_size, long double *pWrk, long *pSync);
void pshmem_short_max_to_all(short *target, const short *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            short *pWrk, long *pSync);
void pshmem_int_max_to_all(int *target, const int *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          int *pWrk, long *pSync);
void pshmem_long_max_to_all(long *target, const long *source, int nreduce,
                           int PE_start, int logPE_stride, int PE_size,
                           long *pWrk, long *pSync);
void pshmem_longlong_max_to_all(long long *target, const long long *source,
                               int nreduce, int PE_start, int logPE_stride,
                               int PE_size, long long *pWrk, long *pSync);

void pshmem_float_sum_to_all(float *target, const float *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size, 
                            float *pWrk, long *pSync);
void pshmem_double_sum_to_all(double *target, const double *source, int nreduce,
                             int PE_start, int logPE_stride, int PE_size, 
                             double *pWrk, long *pSync);
void pshmem_longdouble_sum_to_all(long double *target, const long double *source,
                                 int nreduce, int PE_start, int logPE_stride,
                                 int PE_size, long double *pWrk, long *pSync);
void pshmem_complexf_sum_to_all(float complex *target, const float complex *source,
                               int nreduce, int PE_start, int logPE_stride,
                               int PE_size, float complex *pWrk, long *pSync);
void pshmem_complexd_sum_to_all(double complex *target, const double complex *source,
                               int nreduce, int PE_start, int logPE_stride,
                               int PE_size, double complex *pWrk, long *pSync);
void pshmem_short_sum_to_all(short *target, const short *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size, 
                            short *pWrk, long *pSync);
void pshmem_int_sum_to_all(int *target, const int *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size, 
                          int *pWrk, long *pSync);
void pshmem_long_sum_to_all(long *target, const long *source, int nreduce,
                           int PE_start, int logPE_stride, int PE_size, 
                           long *pWrk, long *pSync);
void pshmem_longlong_sum_to_all(long long *target, const long long *source,
                               int nreduce, int PE_start, int logPE_stride,
                               int PE_size, long long *pWrk, long *pSync);

void pshmem_float_prod_to_all(float *target, const float *source, int nreduce, 
                             int PE_start, int logPE_stride, int PE_size, 
                             float *pWrk, long *pSync);
void pshmem_double_prod_to_all(double *target, const double *source, int nreduce, 
                              int PE_start, int logPE_stride, int PE_size,
                              double *pWrk, long *pSync);
void pshmem_longdouble_prod_to_all(long double *target, const long double *source,
                                  int nreduce, int PE_start, int logPE_stride,
                                  int PE_size, long double *pWrk, long *pSync);
void pshmem_complexf_prod_to_all(float complex *target, const float complex *source,
                                int nreduce, int PE_start, int logPE_stride,
                                int PE_size, float complex *pWrk, long *pSync);
void pshmem_complexd_prod_to_all(double complex *target, 
                                const double complex *source, int nreduce,
                                int PE_start, int logPE_stride, int PE_size, 
                                double complex *pWrk, long *pSync);
void pshmem_short_prod_to_all(short *target, const short *source, int nreduce, 
                             int PE_start, int logPE_stride, int PE_size, 
                             short *pWrk, long *pSync);
void pshmem_int_prod_to_all(int *target, const int *source, int nreduce,
                           int PE_start, int logPE_stride, int PE_size, 
                           int *pWrk, long *pSync);
void pshmem_long_prod_to_all(long *target, const long *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size, 
                            long *pWrk, long *pSync);
void pshmem_longlong_prod_to_all(long long *target, const long long *source,
                                int nreduce, int PE_start, int logPE_stride,
                                int PE_size, long long *pWrk, long *pSync);

/* 8.5: Collect Routines */
void pshmem_collect32(void *target, const void *source, size_t nlong,
                     int PE_start, int logPE_stride, int PE_size, long *pSync);
void pshmem_collect64(void *target, const void *source, size_t nlong,
                     int PE_start, int logPE_stride, int PE_size, long *pSync);
void pshmem_fcollect32(void *target, const void *source, size_t nlong,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pSync);
void pshmem_fcollect64(void *target, const void *source, size_t nlong,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pSync);

/* 8.5: Broadcast Routines */
void pshmem_broadcast32(void *target, const void *source, size_t nlong, 
                       int PE_root, int PE_start, int logPE_stride,
                       int PE_size, long *pSync);
void pshmem_broadcast64(void *target, const void *source, size_t nlong,
                       int PE_root, int PE_start, int logPE_stride,
                       int PE_size, long *pSync);

/* 8.6: All-to-all routines */
void pshmem_alltoall32(void *dest, const void *source, size_t nelems, int PE_start,
                      int logPE_stride, int PE_size, long *pSync);
void pshmem_alltoall64(void *dest, const void *source, size_t nelems, int PE_start,
                      int logPE_stride, int PE_size, long *pSync);

void pshmem_alltoalls32(void *dest, const void *source, ptrdiff_t dst,
                       ptrdiff_t sst, size_t nelems, int PE_start,
                       int logPE_stride, int PE_size, long *pSync);
void pshmem_alltoalls64(void *dest, const void *source, ptrdiff_t dst,
                       ptrdiff_t sst, size_t nelems, int PE_start,
                       int logPE_stride, int PE_size, long *pSync);

/* 8.6: Point-to-Point Synchronization Routines -- Wait */
void pshmem_short_wait(volatile short *var, short value);
void pshmem_int_wait(volatile int *var, int value);
void pshmem_long_wait(volatile long *var, long value);
void pshmem_longlong_wait(volatile long long *var, long long value);
void pshmem_wait(volatile long *ivar, long cmp_value);

/* 8.6: Point-to-Point Synchronization Routines -- Wait Until */
void pshmem_short_wait_until(volatile short *var, int cond, short value);
void pshmem_int_wait_until(volatile int *var, int cond, int value);
void pshmem_long_wait_until(volatile long *var, int cond, long value);
void pshmem_longlong_wait_until(volatile long long *var, int cond,
                               long long value);
void pshmem_wait_until(volatile long *ivar, int cmp, long value);

/* 8.7: Memory Ordering Routines */
void pshmem_quiet(void);
void pshmem_fence(void);

/* 8.8: Lock Routines */
void pshmem_set_lock(volatile long *lock);
void pshmem_clear_lock(volatile long *lock);
int pshmem_test_lock(volatile long *lock);

/* 8.9: Cache Management Routines (deprecated) */
void pshmem_set_cache_inv(void) __attribute__ ((deprecated));
void pshmem_set_cache_line_inv(void *target) __attribute__ ((deprecated));
void pshmem_clear_cache_inv(void) __attribute__ ((deprecated));
void pshmem_clear_cache_line_inv(void *target) __attribute__ ((deprecated));
void pshmem_udcflush(void) __attribute__ ((deprecated));
void pshmem_udcflush_line(void *target) __attribute__ ((deprecated));

/* F: Deprecated API */
int p_num_pes(void) __attribute__ ((deprecated));
int p_my_pe(void) __attribute__ ((deprecated));
void *pshmalloc(size_t size) __attribute__ ((deprecated));
void *pshmemalign(size_t alignment, size_t size) __attribute__ ((deprecated));
void *pshrealloc(void *ptr, size_t size) __attribute__ ((deprecated));
void pshfree(void *ptr) __attribute__ ((deprecated));
void pstart_pes(int npes) __attribute__ ((deprecated));

/* EXT: Experimental SHMEM Extensions */
void pshmemx_init_thread(int tl_requested, int *tl_provided);
double pshmemx_wtime(void);
char* pshmemx_nodename(void);

/* EXT: Signalling puts */
void pshmemx_getmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe);
void pshmemx_putmem_ct(shmemx_ct_t ct, void *target, const void *source, size_t len, int pe);
void pshmemx_ct_create(shmemx_ct_t *ct);
void pshmemx_ct_free(shmemx_ct_t *ct);
long pshmemx_ct_get(shmemx_ct_t ct);
void pshmemx_ct_set(shmemx_ct_t ct, long value);
void pshmemx_ct_wait(shmemx_ct_t ct, long wait_for);

#ifdef __cplusplus
} /* extern "C" */
#ifdef SHMEM_UNDEF_COMPLEX
#undef complex
#endif
#endif

#endif /* PSHMEM_H */
