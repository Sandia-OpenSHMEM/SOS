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
#include <strings.h>
#include <string.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"

#define MAX(A, B) ((A > B) ? A : B)
#define MIN(A, B) ((A < B) ? A : B)

static long *barrier_work_array;

int
shmem_internal_barrier_init(void)
{
    barrier_work_array = shmalloc(sizeof(long) * _SHMEM_BARRIER_SYNC_SIZE);
    if (NULL == barrier_work_array) return -1;
    bzero(barrier_work_array, sizeof(long) * _SHMEM_BARRIER_SYNC_SIZE);
    return 0;
}


void
shmem_barrier_all(void)
{

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_quiet();
    shmem_barrier(0, 0, shmem_int_num_pes, barrier_work_array);
}


/* Simple fan-in/fan-out algorithm. Should be safe to reuse pSync
   array immediately after return */
void
shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int ret = 0;
    long zero = 0, one = 1;
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    if (PE_start == shmem_int_my_pe) {
        int pe, i;
        /* wait for N - 1 callins up the tree */
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);
        /* Clear pSync; have to do local assignment to clear out NIC atomic cache */
        ret += shmem_internal_put(pSync, &zero, sizeof(zero), shmem_int_my_pe);
        shmem_internal_put_wait(ret);
        ret = 0;
        /* Send acks down psync tree */
        for (pe = PE_start + stride, i = 1 ; 
             i < PE_size ;  
             i++, pe += stride) {
            ret += shmem_internal_put(pSync, &one, sizeof(one), pe);
        }
        shmem_internal_put_wait(ret);
    } else {
        /* send message up psync tree */
        ret += shmem_internal_atomic(pSync, &one, sizeof(one), PE_start, PTL_SUM, DTYPE_LONG);
        shmem_internal_put_wait(ret);
        /* wait for ack down psync tree */
        shmem_long_wait(pSync, 0);
        /* Clear pSync; never atomically incremented, ok to direct assign */
        pSync[0] = 0;
    }
}


static inline
void
shmem_internal_bcast(void *target, const void *source, size_t len,
                int PE_root, int PE_start, int logPE_stride, int PE_size,
                long *pSync)
{ 
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;
    int i, ret = 0;
    long one = 1;

    if (PE_root == shmem_int_my_pe) {
        for (i = PE_start ; i < PE_size ; i += stride) {
            if (i == shmem_int_my_pe && source == target) continue;
            ret += shmem_internal_put(target, source, len, i);
            if (i != shmem_int_my_pe) ret += shmem_internal_put(pSync, &one, sizeof(long), i);
        }
        shmem_internal_put_wait(ret);
    } else {
        shmem_long_wait(pSync, 0);
        /* Clear pSync ; never atomically incremented, ok to direct assign */
        pSync[0] = 0;
    }
}


/* Simple fan-in with atomics with a twist.  PE_start must initialize
   its target before allowing any other rank to do the atomic
   operation.  PE_start does a put from source to target, waits for
   completion, then sends a ping to each peer's pSync.  At that point,
   the peers atomic into PE_start's target and send an atomic incr
   into PE_start's pSync and complete.  PE_start completes when it has
   PE_size - 1 counters in its pSync. */
static inline
void
shmem_internal_op_to_all(void *target, void *source, int count, int type_size,
                    int PE_start, int logPE_stride, int PE_size,
                    void *pWrk, long *pSync, 
                    ptl_op_t op, ptl_datatype_t datatype)
{
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;
    int ret = 0;
    long zero = 0, one = 1;

    if (PE_start == shmem_int_my_pe) {
        int pe, i;
        /* update our target buffer with our contribution */
        ret = shmem_internal_put(target, source, count * type_size, shmem_int_my_pe);
        shmem_internal_put_wait(ret);
        ret = 0;
        /* let everyone know that it's safe to send to us */
        for (pe = PE_start + stride, i = 1 ; 
             i < PE_size ;  
             i++, pe += stride) {
            ret += shmem_internal_put(pSync, &one, sizeof(one), pe);
        }
        shmem_internal_put_wait(ret);
        ret = 0;
        /* Wait for others to acknowledge sending data */
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);
        /* reset pSync; atomics used, so have to use Portals op */
        ret += shmem_internal_put(pSync, &zero, sizeof(zero), shmem_int_my_pe);
        shmem_internal_put_wait(ret);
    } else {
        /* wait for clear to send */
        shmem_long_wait(pSync, 0);
        /* send data, ack, and wait for completion */
        ret += shmem_internal_atomic(target, source, count * type_size, PE_start, op, datatype);
        ret += shmem_internal_atomic(pSync, &one, sizeof(one), PE_start, PTL_SUM, DTYPE_LONG);
        shmem_internal_put_wait(ret);
        /* reset pSync */
        pSync[0] = 0;
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, count * type_size, PE_start, PE_start, logPE_stride, PE_size, pSync);
}


/* len can be different on each node, so this is semi-linear.
   PE_Start starts a copy of his data into target, and increments a
   length counter, sending that counter to the next PE.  That PE
   starts the transfer to counter offset, increments the counter, and
   send the counter to the next PE.  The sends are non-blocking to
   different peers (PE_start and next), so some overlap is likely.
   Finally, the last peer sends the counter back to PE_start so that
   the size of the data to broadcast is known. */
static inline
void
shmem_internal_collect(void *target, const void *source, size_t len,
                  int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int ret = 0;
    long tmp[2] = {0, 1};
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;
    int pe;
    int bcast_len = 0;

    if (PE_size == 1) {
        if (target != source) memcpy(target, source, len);
        return;
    }

    /* collect in PE_start */
    if (PE_start == shmem_int_my_pe) {
        if (target != source) {
            ret += shmem_internal_put(target, source, len, PE_start);
        }
        tmp[0] = len;
        ret += shmem_internal_put(pSync, tmp, 2 * sizeof(long), PE_start + stride);
        shmem_long_wait(&pSync[1], 0);
        shmem_internal_put_wait(ret);
        bcast_len = pSync[0];
        pSync[0] = pSync[1] = 0;
    } else {
        shmem_long_wait(&pSync[1], 0);
        ret += shmem_internal_put((char*) target + pSync[0], source, len, PE_start);
        if (shmem_int_my_pe == PE_start + stride * (PE_size - 1)) {
            pe = PE_start;
        } else {
            pe = shmem_int_my_pe + stride;
        }
        tmp[0] = pSync[0] + len;
        pSync[0] = pSync[1] = 0;
        ret += shmem_internal_put(pSync, tmp, 2 * sizeof(long), PE_start);
        shmem_internal_put_wait(ret);
    }

    /* broadcast out */
    shmem_internal_bcast(target, target, bcast_len, PE_start, PE_start, logPE_stride, PE_size, pSync);
}


/* Since offsets can be directly computed, each rank puts directly
   into their offset in PE_Start's target, then increments a counter
   in pSync.  PE_start then broadcasts to everyone. */
static inline
void
shmem_internal_fcollect(void *target, const void *source, size_t len,
                   int PE_start, int logPE_stride, int PE_size, long *pSync)
{
    int ret = 0;
    long tmp = 1;
    int stride = (logPE_stride == 0) ? 1 : 1 << logPE_stride;

    if (PE_start == shmem_int_my_pe) {
        if (source != target) {
            ret += shmem_internal_put(target, source, len, PE_start);
        }
        shmem_long_wait_until(pSync, SHMEM_CMP_EQ, PE_size - 1);
        tmp = 0;
        ret += shmem_internal_put(target, &tmp, sizeof(tmp), PE_start);
        shmem_internal_put_wait(ret);
    } else {
        size_t offset = (shmem_int_my_pe - PE_start) / stride;
        ret += shmem_internal_put((char*) target + offset, source, len, PE_start);
        ret += shmem_internal_atomic(pSync, &tmp, sizeof(long), PE_start, PTL_SUM, DTYPE_LONG);
        shmem_internal_put_wait(ret);
    }
    
    shmem_internal_bcast(target, target, len * PE_size, PE_start, PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_short_and_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, DTYPE_SHORT);
}


void
shmem_int_and_to_all(int *target, int *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size,
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, DTYPE_INT);
}


void
shmem_long_and_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, DTYPE_LONG);
}


void
shmem_longlong_and_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LAND, DTYPE_LONG);
}


void
shmem_short_or_to_all(short *target, short *source, int nreduce, 
                      int PE_start, int logPE_stride, int PE_size, 
                      short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LOR, DTYPE_SHORT);
}


void
shmem_int_or_to_all(int *target, int *source, int nreduce, 
                    int PE_start, int logPE_stride, int PE_size, 
                    int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LOR, DTYPE_INT);
}


void
shmem_long_or_to_all(long *target, long *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size, 
                     long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LOR, DTYPE_LONG);
}


void
shmem_longlong_or_to_all(long long *target, long long *source, int nreduce,
                         int PE_start, int logPE_stride, int PE_size, 
                         long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LOR, DTYPE_LONG);
}


void
shmem_short_xor_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LXOR, DTYPE_SHORT);
}

void
shmem_int_xor_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LXOR, DTYPE_INT);
}


void
shmem_long_xor_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LXOR, DTYPE_LONG);
}


void
shmem_longlong_xor_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size, 
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_LXOR, DTYPE_LONG);
}


void
shmem_float_min_to_all(float *target, float *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_FLOAT);
}


void
shmem_double_min_to_all(double *target, double *source, int nreduce,
                        int PE_start, int logPE_stride, int PE_size, 
                        double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_DOUBLE);
}


void
shmem_longdouble_min_to_all(long double *target, long double *source, int nreduce, 
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, PTL_LONG_DOUBLE);
}


void
shmem_short_min_to_all(short *target, short *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_SHORT);
}


void
shmem_int_min_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_INT);
}


void
shmem_long_min_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_LONG);
}


void
shmem_longlong_min_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MIN, DTYPE_LONG);
}


void
shmem_float_max_to_all(float *target, float *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_FLOAT);
}


void
shmem_double_max_to_all(double *target, double *source, int nreduce,
                        int PE_start, int logPE_stride, int PE_size,
                        double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_DOUBLE);
}


void
shmem_longdouble_max_to_all(long double *target, long double *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, PTL_LONG_DOUBLE);
}


void
shmem_short_max_to_all(short *target, short *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size,
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_SHORT);
}


void
shmem_int_max_to_all(int *target, int *source, int nreduce,
                     int PE_start, int logPE_stride, int PE_size,
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_INT);
}


void
shmem_long_max_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size,
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_LONG);
}


void 
shmem_longlong_max_to_all(long long *target, long long *source, int nreduce,
                          int PE_start, int logPE_stride, int PE_size,
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_MAX, DTYPE_LONG);
}


void
shmem_float_sum_to_all(float *target, float *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_FLOAT);
}


void
shmem_double_sum_to_all(double *target, double *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_DOUBLE);
}


void
shmem_longdouble_sum_to_all(long double *target, long double *source, int nreduce,
                            int PE_start, int logPE_stride, int PE_size,
                            long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_LONG_DOUBLE);
}


void
shmem_complexf_sum_to_all(float complex *target, float complex *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size,
                          float complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_FLOAT_COMPLEX);
}


void 
shmem_complexd_sum_to_all(double complex *target, double complex *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size, 
                          double complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, PTL_DOUBLE_COMPLEX);
}


void
shmem_short_sum_to_all(short *target, short *source, int nreduce, 
                       int PE_start, int logPE_stride, int PE_size, 
                       short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_SHORT);
}


void
shmem_int_sum_to_all(int *target, int *source, int nreduce, 
                     int PE_start, int logPE_stride, int PE_size, 
                     int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_INT);
}


void
shmem_long_sum_to_all(long *target, long *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_LONG);
}


void
shmem_longlong_sum_to_all(long long *target, long long *source, int nreduce, 
                          int PE_start, int logPE_stride, int PE_size, 
                          long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_SUM, DTYPE_LONG);
}


void
shmem_float_prod_to_all(float *target, float *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        float *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_FLOAT);
}


void
shmem_double_prod_to_all(double *target, double *source, int nreduce, 
                         int PE_start, int logPE_stride, int PE_size,
                         double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_DOUBLE);
}


void
shmem_longdouble_prod_to_all(long double *target, long double *source, int nreduce, 
                             int PE_start, int logPE_stride, int PE_size,
                             long double *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long double),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_LONG_DOUBLE);
}


void
shmem_complexf_prod_to_all(float complex *target, float complex *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size,
                           float complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(float complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_FLOAT_COMPLEX);
}


void
shmem_complexd_prod_to_all(double complex *target, double complex *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size, 
                           double complex *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(double complex),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, PTL_DOUBLE_COMPLEX);
}


void
shmem_short_prod_to_all(short *target, short *source, int nreduce, 
                        int PE_start, int logPE_stride, int PE_size, 
                        short *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(short),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_SHORT);
}


void
shmem_int_prod_to_all(int *target, int *source, int nreduce,
                      int PE_start, int logPE_stride, int PE_size, 
                      int *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(int),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_INT);
}


void
shmem_long_prod_to_all(long *target, long *source, int nreduce,
                       int PE_start, int logPE_stride, int PE_size, 
                       long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_LONG);
}


void
shmem_longlong_prod_to_all(long long *target, long long *source, int nreduce, 
                           int PE_start, int logPE_stride, int PE_size,
                           long long *pWrk, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_op_to_all(target, source, nreduce, sizeof(long long),
                    PE_start, logPE_stride, PE_size,
                    pWrk, pSync, PTL_PROD, DTYPE_LONG);
}


void
shmem_broadcast32(void *target, const void *source, size_t nlong, 
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_bcast(target, source, nlong * 4,
                    PE_root, PE_start, logPE_stride, PE_size,
                    pSync);
}


void
shmem_broadcast64(void *target, const void *source, size_t nlong,
                  int PE_root, int PE_start, int logPE_stride, int PE_size,
                  long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_bcast(target, source, nlong * 8,
                    PE_root, PE_start, logPE_stride, PE_size,
                    pSync);
}


void
shmem_collect32(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_collect(target, source, nlong * 4,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_collect64(void *target, const void *source, size_t nlong,
                int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_collect(target, source, nlong * 8,
                      PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect32(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fcollect(target, source, nlong * 4,
                       PE_start, logPE_stride, PE_size, pSync);
}


void
shmem_fcollect64(void *target, const void *source, size_t nlong,
                 int PE_start, int logPE_stride, int PE_size, long *pSync)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_fcollect(target, source, nlong * 8,
                       PE_start, logPE_stride, PE_size, pSync);
}

