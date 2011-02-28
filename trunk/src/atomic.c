/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include <portals4.h>
#include <stdio.h>
#include <stdlib.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"

#define SHMEM_SWAP(target, source, cond, pe, op, datatype)      \
    do {                                                        \
        int ret;                                                \
        ptl_process_t peer;                                     \
        ptl_event_t ev;                                         \
        peer.rank = pe;                                         \
                                                                \
        ret = PtlSwap(md_h,                                     \
                      (ptl_size_t) source,                      \
                      md_h,                                     \
                      (ptl_size_t) source,                      \
                      sizeof(*source),                          \
                      peer,                                     \
                      pt_entry,                                 \
                      0,                                        \
                      (ptl_size_t) target,                      \
                      NULL,                                     \
                      0,                                        \
                      cond,                                     \
                      op,                                       \
                      datatype);                                \
        if (PTL_OK != ret) { abort(); }                         \
                                                                \
        do {                                                    \
            ret = PtlEQWait(source_eq_h, &ev);                  \
            if (PTL_OK != ret) { abort(); }                     \
        } while (ev.type != PTL_EVENT_REPLY);                   \
    } while(0)

#define SHMEM_ADD(target, incr, pe, datatype)                   \
    do {                                                        \
        int ret;                                                \
        ptl_event_t ev;                                         \
        ptl_process_t peer;                                     \
        peer.rank = pe;                                         \
                                                                \
        ret = PtlAtomic(md_h,                                   \
                        (ptl_size_t) incr,                      \
                        sizeof(*incr),                          \
                        PTL_CT_ACK_REQ,                         \
                        peer,                                   \
                        pt_entry,                               \
                        0,                                      \
                        (ptl_size_t) target,                    \
                        NULL,                                   \
                        0,                                      \
                        PTL_SUM,                                \
                        datatype);                              \
        if (PTL_OK != ret) { abort(); }                         \
                                                                \
        ret = PtlEQWait(source_eq_h, &ev);                      \
        if (PTL_OK != ret) { abort(); }                         \
                                                                \
        if (ev.type != PTL_EVENT_SEND) {                        \
            printf("received event of type %d\n", ev.type);     \
            abort();                                            \
        }                                                       \
    } while(0)

#define SHMEM_FADD(target, source, pe, datatype)                \
    do {                                                        \
        int ret;                                                \
        ptl_event_t ev;                                         \
        ptl_process_t peer;                                     \
        peer.rank = pe;                                         \
                                                                \
        ret = PtlFetchAtomic(md_h,                              \
                             (ptl_size_t) source,               \
                             md_h,                              \
                             (ptl_size_t) source,               \
                             sizeof(*source),                   \
                             peer,                              \
                             pt_entry,                          \
                             0,                                 \
                             (ptl_size_t) target,               \
                             NULL,                              \
                             0,                                 \
                             PTL_SUM,                           \
                             datatype);                         \
        if (PTL_OK != ret) { abort(); }                         \
                                                                \
        do {                                                    \
            ret = PtlEQWait(source_eq_h, &ev);                  \
            if (PTL_OK != ret) { abort(); }                     \
        } while (ev.type != PTL_EVENT_REPLY);                   \
    } while(0)


float
shmem_float_swap(float *target, float value, int pe)
{
    float tmp = value;
    SHMEM_SWAP(target, &tmp, NULL, pe, PTL_SWAP, PTL_FLOAT);
    return tmp;
}


double
shmem_double_swap(double *target, double value, int pe)
{
    double tmp = value;
    SHMEM_SWAP(target, &tmp, NULL, pe, PTL_SWAP, PTL_DOUBLE);
    return tmp;
}


int
shmem_int_swap(int *target, int value, int pe)
{
    int tmp = value;
    SHMEM_SWAP(target, &tmp, NULL, pe, PTL_SWAP, PTL_INT);
    return tmp;
}


long
shmem_long_swap(long *target, long value, int pe)
{
    long tmp = value;
    SHMEM_SWAP(target, &tmp, NULL, pe, PTL_SWAP, PTL_LONG);
    return tmp;
}


long long
shmem_longlong_swap(long long *target, long long value, int pe)
{
    /* BWB: Unimplemented */
    return 0;
}


long
shmem_swap(long *target, long value, int pe)
{
    long tmp = value;
    SHMEM_SWAP(target, &tmp, NULL, pe, PTL_SWAP, PTL_LONG);
    return tmp;
}


int
shmem_int_cswap(int *target, int cond, int value, int pe)
{
    int tmp = value;
    SHMEM_SWAP(target, &tmp, &cond, pe, PTL_CSWAP, PTL_INT);
    return tmp;
}


long
shmem_long_cswap(long *target, long cond, long value, int pe)
{
    long tmp = value;
    SHMEM_SWAP(target, &tmp, &cond, pe, PTL_SWAP, PTL_LONG);
    return tmp;
}


long long
shmem_longlong_cswap(long long * target, long long cond, 
                     long long value, int pe)
{
    /* BWB: Unimplemented */
    return 0;
}


void
shmem_int_inc(int *target, int pe)
{
    int tmp = 1;
    SHMEM_ADD(target, &tmp, pe, PTL_INT);
}


void
shmem_long_inc(long *target, int pe)
{
    long tmp = 1;
    SHMEM_ADD(target, &tmp, pe, PTL_LONG);
}


void
shmem_longlong_inc(long long *target, int pe)
{
    /* BWB: Unimplemented */
}


int
shmem_int_finc(int *target, int pe)
{
    int source = 1;
    SHMEM_FADD(target, &source, pe, PTL_INT);
    return source;
}


long
shmem_long_finc(long *target, int pe)
{
    long source = 1;
    SHMEM_FADD(target, &source, pe, PTL_LONG);
    return source;
}


long long
shmem_longlong_finc(long long *target, int pe)
{
    /* BWB: Unimplemented */
    return 0;
}


void
shmem_int_add(int *target, int value, int pe)
{
    SHMEM_ADD(target, &value, pe, PTL_INT);
}


void
shmem_long_add(long *target, long value, int pe)
{
    SHMEM_ADD(target, &value, pe, PTL_LONG);
}


void
shmem_longlong_add(long long *target, long long value, int pe)
{
    /* BWB: Unimplemented */
}


int
shmem_int_fadd(int *target, int value, int pe)
{
    int source = value;
    SHMEM_FADD(target, &source, pe, PTL_INT);
    return source;
}


long
shmem_long_fadd(long *target, long value, int pe)
{
    long source = value;
    SHMEM_FADD(target, &source, pe, PTL_LONG);
    return source;
}


long long
shmem_longlong_fadd(long long *target, long long value,
                    int pe)
{
    /* BWB: Unimplemented */
    return 0;
}


void
shmem_clear_lock(long *lock)
{
    shmem_quiet();
    long new = 0;
    int ret;
    ptl_event_t ev;
    ptl_process_t peer;
    peer.rank = 0;
    
    ret = PtlPut(md_h,
                 (ptl_size_t) &new,
                 sizeof(long),
                 PTL_ACK_REQ,
                 peer,
                 pt_entry,
                 0,
                 (ptl_size_t) lock,
                 NULL,
                 0);
    if (PTL_OK != ret) { abort(); }
        
    do {
        ret = PtlEQWait(source_eq_h, &ev);
        if (PTL_OK != ret) { abort(); }
    } while (ev.type != PTL_EVENT_ACK);
}


void
shmem_set_lock(long *lock)
{
    while (1 == shmem_test_lock(lock));
}


int
shmem_test_lock(long *lock)
{
    return shmem_long_cswap(lock, 0, 1, 0);
}
