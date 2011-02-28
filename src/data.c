/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include <portals4.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"


static inline
void
int_shmem_put(void *target, const void *source, size_t len, int pe)
{
    size_t sent = 0;
    int ret;
    ptl_process_t peer;
    peer.rank = pe;

    for (sent = 0 ; sent < len ; sent += max_ordered_size) {
        size_t bufsize = (len - sent < max_ordered_size) ? len - sent : max_ordered_size;
        ret = PtlPut(md_h,
                     (ptl_size_t) ((char*) source + sent),
                     bufsize,
                     PTL_CT_ACK_REQ,
                     peer,
                     pt_entry,
                     0,
                     (ptl_size_t) ((char*) target + sent),
                     NULL,
                     0);
        if (PTL_OK != ret) { abort(); }
        pending_counter++;
    }

    /* wait for completions */
    for (sent = 0 ; sent < len ; sent += max_ordered_size) {
        ptl_event_t ev;
        ret = PtlEQWait(source_eq_h, &ev);
        if (PTL_OK != ret) { abort(); }

        if (ev.type != PTL_EVENT_SEND) {
            printf("received event of type %d\n", ev.type);
            abort();
        }
    }
}


static inline
void
int_shmem_get(void *target, const void *source, size_t len, int pe)
{
    int ret;
    ptl_event_t ev;
    ptl_process_t peer;
    peer.rank = pe;

    ret = PtlGet(md_h,
                 (ptl_size_t) source,
                 len,
                 peer,
                 pt_entry,
                 0,
                 (ptl_size_t) target,
                 0);
    if (PTL_OK != ret) { abort(); }

    ret = PtlEQWait(source_eq_h, &ev);
    if (PTL_OK != ret) { abort(); }

    if (ev.type != PTL_EVENT_REPLY) {
        printf("received event of type %d\n", ev.type);
        abort();
    }
}


void
shmem_float_p(float *addr, float value, int pe)
{
    int_shmem_put(addr, &value, sizeof(value), pe);
}


void
shmem_double_p(double *addr, double value, int pe)
{
    int_shmem_put(addr, &value, sizeof(value), pe);
}


void
shmem_longdouble_p(long double *addr, long double value, int pe)
{
    int_shmem_put(addr, &value, sizeof(value), pe);
}


void
shmem_char_p(char *addr, char value, int pe)
{
    int_shmem_put(addr, &value, sizeof(value), pe);
}


void
shmem_short_p(short *addr, short value, int pe)
{
    int_shmem_put(addr, &value, sizeof(value), pe);
}


void
shmem_int_p(int *addr, int value, int pe)
{
    int_shmem_put(addr, &value, sizeof(value), pe);    
}


void
shmem_long_p(long *addr, long value, int pe)
{
    int_shmem_put(addr, &value, sizeof(value), pe);
}


void
shmem_longlong_p(long long *addr, long long value, int pe)
{
    int_shmem_put(addr, &value, sizeof(value), pe);
}


float
shmem_float_g(float *addr, int pe)
{
    float tmp = 0.0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    return tmp;
}


double
shmem_double_g(double *addr, int pe)
{
    double tmp = 0.0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    return tmp;
}

long double
shmem_longdouble_g(long double *addr, int pe)
{
    long double tmp = 0.0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    return tmp;
}


char
shmem_char_g(char *addr, int pe)
{
    char tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    return tmp;
}


short
shmem_short_g(short *addr, int pe)
{
    short tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    return tmp;
}


int
shmem_int_g(int *addr, int pe)
{
    int tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    return tmp;
}


long
shmem_long_g(long *addr, int pe)
{
    long tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    return tmp;
}


long long
shmem_longlong_g(long long *addr, int pe)
{
    long long tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    return tmp;
}


void
shmem_float_put(float *target, const float *source, size_t len, int pe)
{
    int_shmem_put(target, source, sizeof(float) * len, pe);
}


void
shmem_double_put(double *target, const double *source, size_t len, int pe)
{
    int_shmem_put(target, source, sizeof(double) * len, pe);
}


void
shmem_longdouble_put(long double *target, const long double *source, size_t len, int pe)
{
    int_shmem_put(target, source, sizeof(long double) * len, pe);
}


void
shmem_short_put(short *target, const short *source, size_t len, int pe)
{
    int_shmem_put(target, source, sizeof(short) * len, pe);
}


void
shmem_int_put(int *target, const int *source, size_t len, int pe)
{
    int_shmem_put(target, source, sizeof(int) * len, pe);
}


void
shmem_long_put(long *target, const long *source, size_t len, int pe)
{
    int_shmem_put(target, source, sizeof(long) * len, pe);
}


void
shmem_longlong_put(long long *target, const long long *source, size_t len, int pe)
{
    int_shmem_put(target, source, sizeof(long long) * len, pe);
}


void
shmem_put32(void *target, const void *source, size_t len, int pe)
{
    int_shmem_put(target, source, 4 * len, pe);
}


void
shmem_put64(void *target, const void *source, size_t len, int pe)
{
    int_shmem_put(target, source, 8 * len, pe);
}


void
shmem_put128(void *target, const void *source, size_t len, int pe)
{
    int_shmem_put(target, source, 16 * len, pe);
}


void
shmem_putmem(void *target, const void *source, size_t len, int pe)
{
    int_shmem_put(target, source, len, pe);
}


void
shmem_float_get(float *target, const float *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(float) * len, pe);
}


void
shmem_double_get(double *target, const double *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(double) * len, pe);
}


void
shmem_longdouble_get(long double *target, const long double *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(long double) * len, pe);
}


void
shmem_short_get(short *target, const short *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(short) * len, pe);
}


void
shmem_int_get(int *target, const int *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(int) * len, pe);
}


void
shmem_long_get(long *target, const long *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(long) * len, pe);
}


void
shmem_longlong_get(long long *target, const long long *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(long long) * len, pe);
}


void
shmem_get32(void *target, const void *source, size_t len, int pe)
{
    int_shmem_get(target, source, 4 * len, pe);
}


void
shmem_get64(void *target, const void *source, size_t len, int pe)
{
    int_shmem_get(target, source, 8 * len, pe);
}


void
shmem_get128(void *target, const void *source, size_t len, int pe)
{
    int_shmem_get(target, source, 16 * len, pe);
}


void
shmem_getmem(void *target, const void *source, size_t len, int pe)
{
    int_shmem_get(target, source, len, pe);
}


#if 0

void shmem_float_iput(float *target, const float *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_double_iput(double *target, const double *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_longdouble_iput(long double *target, const long double *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_short_iput(short *target, const short *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_int_iput(int *target, const int *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_long_iput(long *target, const long *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_longlong_iput(long long *target, const long long *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_iput32(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_iput64(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_iput128(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);

void shmem_float_iget(float *target, const float *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_double_iget(double *target, const double *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_longdouble_iget(long double *target, const long double *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_short_iget(short *target, const short *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_int_iget(int *target, const int *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_long_iget(long *target, const long *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_longlong_iget(long long *target, const long long *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_iget32(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_iget64(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);
void shmem_iget128(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe);

#endif
