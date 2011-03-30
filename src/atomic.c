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

#ifdef ENABLE_EVENT_COMPLETION
#define SEND_WAIT()                                     \
    do {                                                \
        ptl_event_t ev;                                 \
        int ret;                                        \
        ret = PtlEQWait(put_eq_h, &ev);                 \
        if (PTL_OK != ret) { abort(); }                 \
        if (ev.ni_fail_type != PTL_OK) { abort(); }  \
    } while (0)
#else
#define SEND_WAIT()
#endif

#define SHMEM_SWAP(target, source, cond, pe, op, datatype)      \
    do {                                                        \
        int ret;                                                \
        ptl_pt_index_t pt;                                      \
        long offset;                                            \
        ptl_process_t peer;                                     \
        ptl_ct_event_t ct;                                      \
        peer.rank = pe;                                         \
        GET_REMOTE_ACCESS(target, pt, offset);                  \
                                                                \
        ret = PtlSwap(get_md_h,                                 \
                      (ptl_size_t) source,                      \
                      put_md_h,                                 \
                      (ptl_size_t) source,                      \
                      sizeof(*source),                          \
                      peer,                                     \
                      pt,                                       \
                      0,                                        \
                      offset,                                   \
                      NULL,                                     \
                      0,                                        \
                      cond,                                     \
                      op,                                       \
                      datatype);                                \
        if (PTL_OK != ret) { abort(); }                         \
        pending_put_counter++;                                  \
        pending_get_counter++;                                  \
                                                                \
        SEND_WAIT();                                            \
        ret = PtlCTWait(get_ct_h, pending_get_counter, &ct);    \
        if (PTL_OK != ret) { abort(); }                         \
        if (ct.failure != 0) { abort(); }                       \
    } while(0)

#define SHMEM_ADD(target, incr, pe, datatype)                   \
    do {                                                        \
        int ret;                                                \
        ptl_pt_index_t pt;                                      \
        long offset;                                            \
        ptl_process_t peer;                                     \
        peer.rank = pe;                                         \
        GET_REMOTE_ACCESS(target, pt, offset);                  \
                                                                \
        ret = PtlAtomic(put_md_h,                               \
                        (ptl_size_t) incr,                      \
                        sizeof(*incr),                          \
                        PTL_CT_ACK_REQ,                         \
                        peer,                                   \
                        pt,                                     \
                        0,                                      \
                        offset,                                 \
                        NULL,                                   \
                        0,                                      \
                        PTL_SUM,                                \
                        datatype);                              \
        if (PTL_OK != ret) { abort(); }                         \
        pending_put_counter++;                                  \
        SEND_WAIT();                                            \
    } while(0)

#define SHMEM_FADD(target, source, pe, datatype)                \
    do {                                                        \
        int ret;                                                \
        ptl_pt_index_t pt;                                      \
        long offset;                                            \
        ptl_process_t peer;                                     \
        ptl_ct_event_t ct;                                      \
        peer.rank = pe;                                         \
        GET_REMOTE_ACCESS(target, pt, offset);                  \
                                                                \
        ret = PtlFetchAtomic(get_md_h,                          \
                             (ptl_size_t) source,               \
                             put_md_h,                          \
                             (ptl_size_t) source,               \
                             sizeof(*source),                   \
                             peer,                              \
                             pt,                                \
                             0,                                 \
                             offset,                            \
                             NULL,                              \
                             0,                                 \
                             PTL_SUM,                           \
                             datatype);                         \
        if (PTL_OK != ret) { abort(); }                         \
        pending_get_counter++;                                  \
                                                                \
        SEND_WAIT();                                            \
        ret = PtlCTWait(get_ct_h, pending_get_counter, &ct);    \
        if (PTL_OK != ret) { abort(); }                         \
        if (ct.failure != 0) { abort(); }                       \
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


short
shmem_short_swap(short *target, short value, int pe)
{
    short tmp = value;
    SHMEM_SWAP(target, &tmp, NULL, pe, PTL_SWAP, PTL_SHORT);
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
    long long tmp = value;
    SHMEM_SWAP(target, &tmp, NULL, pe, PTL_SWAP, PTL_LONG);
    return tmp;
}


long
shmem_swap(long *target, long value, int pe)
{
    long tmp = value;
    SHMEM_SWAP(target, &tmp, NULL, pe, PTL_SWAP, PTL_LONG);
    return tmp;
}


short
shmem_short_cswap(short *target, short cond, short value, int pe)
{
    short tmp = value;
    SHMEM_SWAP(target, &tmp, &cond, pe, PTL_CSWAP, PTL_SHORT);
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
    long long tmp = value;
    SHMEM_SWAP(target, &tmp, &cond, pe, PTL_SWAP, PTL_LONG);
    return tmp;
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
    long long tmp = 1;
    SHMEM_ADD(target, &tmp, pe, PTL_LONG);
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
    long long source = 1;
    SHMEM_FADD(target, &source, pe, PTL_LONG);
    return source;
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
    SHMEM_ADD(target, &value, pe, PTL_LONG);
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
    long long source = value;
    SHMEM_FADD(target, &source, pe, PTL_LONG);
    return source;
}

/*
 * Use basic MCS distributed lock algorithm for lock
 */
struct lock_t {
    short last; /* only has meaning on PE 0 */
    short next;
    short signal;
    short pad;
};
typedef struct lock_t lock_t;

void
shmem_clear_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    short curr;

    shmem_quiet();
    curr = shmem_short_cswap(&(lock->last), shmem_int_my_pe + 1, 0, 0);
    if (curr != shmem_int_my_pe + 1) {
        shmem_short_wait(&(lock->next), 0);
        shmem_short_p(&(lock->signal), 1, lock->next - 1);
    }
}


void
shmem_set_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    short curr;

    shmem_short_p(&(lock->next), 0, shmem_int_my_pe);
    shmem_short_p(&(lock->signal), 0, shmem_int_my_pe);
    shmem_quiet();
    curr = shmem_short_swap(&(lock->last), shmem_int_my_pe + 1, 0);
    if (0 != curr) {
        shmem_short_p(&(lock->next), shmem_int_my_pe + 1, curr - 1);
        shmem_short_wait(&(lock->signal), 0);
    }
}


int
shmem_test_lock(long *lockp)
{
    lock_t *lock = (lock_t*) lockp;
    short curr;

    shmem_short_p(&(lock->next), 0, shmem_int_my_pe);
    shmem_short_p(&(lock->signal), 0, shmem_int_my_pe);
    shmem_quiet();
    curr = shmem_short_cswap(&(lock->last), 0, shmem_int_my_pe + 1, 0);
    if (0 == curr) {
        return 0;
    }
    return 1;
}
