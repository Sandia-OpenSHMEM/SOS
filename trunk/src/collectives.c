/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include <portals4.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"


static long *barrier_work_array;

int
shmem_barrier_init(void)
{
    barrier_work_array = shmalloc(sizeof(long) * _SHMEM_BARRIER_SYNC_SIZE);
    if (NULL == barrier_work_array) return -1;
    bzero(barrier_work_array, sizeof(long) * _SHMEM_BARRIER_SYNC_SIZE);
    return 0;
}


void
shmem_barrier_all(void)
{
    shmem_quiet();
    shmem_barrier(0, 0, shmem_int_num_pes, barrier_work_array);
}


void
shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;
    if (PE_start == shmem_int_my_pe) {
        int pe, i;
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);
        pSync[0] = 0;
        for (pe = PE_start + stride, i = 1 ; 
             i < PE_size ;  
             i++, pe += stride) {
            shmem_long_p(pSync, 1, pe);
        }
    } else {
        shmem_long_inc(pSync, PE_start);
        shmem_long_wait(pSync, 0);
        pSync[0] = 0;
    }
}


static inline
void
shmem_int_op_to_all(void *target, void *source, int len,
                    int PE_start, int logPE_stride, int PE_size,
                    void *pWrk, long *pSync, 
                    ptl_op_t op, ptl_datatype_t datatype)
{
    /* BWB: Unimplemented */
    printf("shmem_int_collect unimplemented\n");
    abort();
}


static inline
void
shmem_int_bcast(void *target, const void *source, size_t len,
                int PE_root, int PE_start, int logPE_stride, int PE_size,
                long *pSync)
{
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;
    int i;

    if (PE_root == shmem_int_my_pe) {
        for (i = PE_start ; i < PE_size ; i += stride) {
            if (i == shmem_int_my_pe && source == target) continue;
            shmem_putmem(target, source, len, i);
            if (i != shmem_int_my_pe) shmem_long_p(pSync, 1, i);
        }
    } else {
        shmem_long_wait(pSync, 0);
        pSync[0] = 0;
    }
}


static inline
void
shmem_int_collect(void *target, const void *source, size_t len,
                  int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    /* BWB: Unimplemented */
    printf("shmem_int_collect unimplemented\n");
    abort();
}


static inline
void
shmem_int_fcollect(void *target, const void *source, size_t len,
                   int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    /* BWB: Unimplemented */
    printf("shmem_int_fcollect unimplemented\n");
    abort();
}


void
shmem_short_and_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       short *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(short) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, PTL_SHORT);
}


void
shmem_int_and_to_all(int *target, int *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size,
                     int *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(int) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, PTL_INT);
}


void
shmem_long_and_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, PTL_LONG);
}


void
shmem_longlong_and_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, PTL_LONG);
}


void
shmem_short_or_to_all(short *target, short *source, int nreduce, 
                      int PE_start, int logPE_stride, int PE_size, 
                      short *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(short) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LOR, PTL_SHORT);
}


void
shmem_int_or_to_all(int *target, int *source, int nreduce, 
                    int PE_start, int logPE_stride, int PE_size, 
                    int *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(int) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LOR, PTL_INT);
}


void
shmem_long_or_to_all(long *target, long *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size, 
                     long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LOR, PTL_LONG);
}


void
shmem_longlong_or_to_all(long long *target, long long *source, int nreduce,
                         int PE_start, int logPE_stride, int PE_size, 
                         long long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LOR, PTL_LONG);
}


void
shmem_short_xor_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(short) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LXOR, PTL_SHORT);
}

void
shmem_int_xor_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(int) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LXOR, PTL_INT);
}


void
shmem_long_xor_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LXOR, PTL_LONG);
}


void
shmem_longlong_xor_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size, 
                          long long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LXOR, PTL_LONG);
}


void
shmem_float_min_to_all(float *target, float *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       float *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(float) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_FLOAT);
}


void
shmem_double_min_to_all(double *target, double *source, int nreduce,
                        int PE_start, int logPE_stride, int PE_size, 
                        double *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(double) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_DOUBLE);
}


void
shmem_longdouble_min_to_all(long double *target, long double *source, int nreduce, 
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long double) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_DOUBLE);
    /* BWB: FIX ME: This should be PTL_LONGDOUBLE */
}


void
shmem_short_min_to_all(short *target, short *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(short) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_SHORT);
}


void
shmem_int_min_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(int) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_INT);
}


void
shmem_long_min_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_LONG);
}


void
shmem_longlong_min_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_LONG);
}


void
shmem_float_max_to_all(float *target, float *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       float *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(float) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_FLOAT);
}


void
shmem_double_max_to_all(double *target, double *source, int nreduce,
                        int PE_start, int logPE_stride, int PE_size,
                        double *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(double) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_DOUBLE);
}


void
shmem_longdouble_max_to_all(long double *target, long double *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long double) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_DOUBLE);
    /* BWB: FIX ME: This should be PTL_LONGDOUBLE */
}


void
shmem_short_max_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       short *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(short) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_SHORT);
}


void
shmem_int_max_to_all(int *target, int *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size,
                     int *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(int) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_INT);
}


void
shmem_long_max_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_LONG);
}


void 
shmem_longlong_max_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_LONG);
}


void
shmem_float_sum_to_all(float *target, float *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       float *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(float) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_FLOAT);
}


void
shmem_double_sum_to_all(double *target, double *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        double *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(double) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_DOUBLE);
}


void
shmem_longdouble_sum_to_all(long double *target, long double *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long double) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_DOUBLE);
    /* BWB: FIX ME: This should be PTL_LONGDOUBLE */
}


void
shmem_complexf_sum_to_all(float complex *target, float complex *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size,
                          float complex *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(float complex) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_FLOAT);
    /* BWB: FIX ME: This should be a complex */
}


void 
shmem_complexd_sum_to_all(double complex *target, double complex *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size, 
                          double complex *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(double complex) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_DOUBLE);
    /* BWB: FIX ME: This should be a complex */
}


void
shmem_short_sum_to_all(short *target, short *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(short) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_SHORT);
}


void
shmem_int_sum_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(int) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_INT);
}


void
shmem_long_sum_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_LONG);
}


void
shmem_longlong_sum_to_all(long long *target, long long *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size, 
                          long long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_LONG);
}


void
shmem_float_prod_to_all(float *target, float *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        float *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(float) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_FLOAT);
}


void
shmem_double_prod_to_all(double *target, double *source, int nreduce, 
                         int PE_start, int logPE_stride, int PE_size,
                         double *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(double) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_DOUBLE);
}


void
shmem_longdouble_prod_to_all(long double *target, long double *source, int nreduce, 
                             int PE_start, int logPE_stride, int PE_size,
                             long double *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long double) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_DOUBLE);
    /* BWB: FIX ME: This should be PTL_LONGDOUBLE */
}


void
shmem_complexf_prod_to_all(float complex *target, float complex *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size,
                           float complex *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(float complex) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_FLOAT);
    /* BWB: FIX ME: This should be a complex */
}


void
shmem_complexd_prod_to_all(double complex *target, double complex *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size, 
                           double complex *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(double complex) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_DOUBLE);
    /* BWB: FIX ME: This should be a complex */
}


void
shmem_short_prod_to_all(short *target, short *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        short *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(short) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_SHORT);
}


void
shmem_int_prod_to_all(int *target, int *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      int *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(int) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_INT);
}


void
shmem_long_prod_to_all(long *target, long *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_LONG);
}


void
shmem_longlong_prod_to_all(long long *target, long long *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size,
                           long long *pWrk, long *pSync)
{
    shmem_int_op_to_all(target, source, sizeof(long long) * nreduce,
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_LONG);
}


void
shmem_broadcast32(void *target, const void *source, size_t nlong, 
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
    shmem_int_bcast(target, source, nlong * 4,
                    PE_root, PE_start, logPE_stride, PE_size,
                    pSync);
}


void
shmem_broadcast64(void *target, const void *source, size_t nlong,
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
    shmem_int_bcast(target, source, nlong * 8,
                    PE_root, PE_start, logPE_stride, PE_size,
                    pSync);
}


void
shmem_collect32(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    shmem_int_collect(target, source, nlong * 4,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_collect64(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    shmem_int_collect(target, source, nlong * 8,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect32(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    shmem_int_fcollect(target, source, nlong * 4,
                       PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect64(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    shmem_int_fcollect(target, source, nlong * 8,
                       PE_start, logPE_stride, PE_size, pSync);
}

