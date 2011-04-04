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
int
int_shmem_put(void *target, const void *source, size_t len, int pe)
{
    size_t sent;
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    int tmp = 0;
    peer.rank = pe;
    GET_REMOTE_ACCESS(target, pt, offset);

    for (sent = 0 ; sent < len ; sent += max_put_size) {
        size_t bufsize = (len - sent < max_put_size) ? len - sent : max_put_size;
        ret = PtlPut(put_md_h,
                     (ptl_size_t) ((char*) source + sent),
                     bufsize,
                     PTL_CT_ACK_REQ,
                     peer,
                     pt,
                     0,
                     offset + sent,
                     NULL,
                     0);
        if (PTL_OK != ret) { abort(); }
        tmp++;
    }
    pending_put_counter += tmp;

    return tmp;
}


static inline
void
int_shmem_put_wait(int count)
{
#if ENABLE_EVENT_COMPLETION
    int ret;
    ptl_event_t ev;

    for ( ; count > 0 ; --count) {
        ret = PtlEQWait(put_eq_h, &ev);
        if (PTL_OK != ret) { abort(); }
        if (ev.ni_fail_type != PTL_OK) { abort(); }
    }
#endif
}


static inline
void
int_shmem_get(void *target, const void *source, size_t len, int pe)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    peer.rank = pe;
    GET_REMOTE_ACCESS(source, pt, offset);

    ret = PtlGet(get_md_h,
                 (ptl_size_t) target,
                 len,
                 peer,
                 pt,
                 0,
                 offset,
                 0);
    if (PTL_OK != ret) { abort(); }
    pending_get_counter++;
}


static inline
void
int_shmem_get_wait(void)
{
    int ret;
    ptl_ct_event_t ct;

    ret = PtlCTWait(get_ct_h, pending_get_counter, &ct);
    if (PTL_OK != ret) { abort(); }
    if (ct.failure != 0) { abort(); }
}


void
shmem_float_p(float *addr, float value, int pe)
{
    int ret;
    ret = int_shmem_put(addr, &value, sizeof(value), pe);
    int_shmem_put_wait(ret);
}


void
shmem_double_p(double *addr, double value, int pe)
{
    int ret;
    ret = int_shmem_put(addr, &value, sizeof(value), pe);
    int_shmem_put_wait(ret);
}


void
shmem_longdouble_p(long double *addr, long double value, int pe)
{
    int ret;
    ret = int_shmem_put(addr, &value, sizeof(value), pe);
    int_shmem_put_wait(ret);
}


void
shmem_char_p(char *addr, char value, int pe)
{
    int ret;
    ret = int_shmem_put(addr, &value, sizeof(value), pe);
    int_shmem_put_wait(ret);
}


void
shmem_short_p(short *addr, short value, int pe)
{
    int ret;
    ret = int_shmem_put(addr, &value, sizeof(value), pe);
    int_shmem_put_wait(ret);
}


void
shmem_int_p(int *addr, int value, int pe)
{
    int ret;
    ret = int_shmem_put(addr, &value, sizeof(value), pe);    
    int_shmem_put_wait(ret);
}


void
shmem_long_p(long *addr, long value, int pe)
{
    int ret;
    ret = int_shmem_put(addr, &value, sizeof(value), pe);
    int_shmem_put_wait(ret);
}


void
shmem_longlong_p(long long *addr, long long value, int pe)
{
    int ret;
    ret = int_shmem_put(addr, &value, sizeof(value), pe);
    int_shmem_put_wait(ret);
}


float
shmem_float_g(float *addr, int pe)
{
    float tmp = 0.0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    int_shmem_get_wait();
    return tmp;
}


double
shmem_double_g(double *addr, int pe)
{
    double tmp = 0.0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    int_shmem_get_wait();
    return tmp;
}

long double
shmem_longdouble_g(long double *addr, int pe)
{
    long double tmp = 0.0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    int_shmem_get_wait();
    return tmp;
}


char
shmem_char_g(char *addr, int pe)
{
    char tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    int_shmem_get_wait();
    return tmp;
}


short
shmem_short_g(short *addr, int pe)
{
    short tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    int_shmem_get_wait();
    return tmp;
}


int
shmem_int_g(int *addr, int pe)
{
    int tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    int_shmem_get_wait();
    return tmp;
}


long
shmem_long_g(long *addr, int pe)
{
    long tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    int_shmem_get_wait();
    return tmp;
}


long long
shmem_longlong_g(long long *addr, int pe)
{
    long long tmp = 0;
    int_shmem_get(&tmp, addr, sizeof(tmp), pe);
    int_shmem_get_wait();
    return tmp;
}


void
shmem_float_put(float *target, const float *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, sizeof(float) * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_double_put(double *target, const double *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, sizeof(double) * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_longdouble_put(long double *target, const long double *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, sizeof(long double) * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_short_put(short *target, const short *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, sizeof(short) * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_int_put(int *target, const int *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, sizeof(int) * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_long_put(long *target, const long *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, sizeof(long) * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_longlong_put(long long *target, const long long *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, sizeof(long long) * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_put32(void *target, const void *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, 4 * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_put64(void *target, const void *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, 8 * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_put128(void *target, const void *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, 16 * len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_putmem(void *target, const void *source, size_t len, int pe)
{
    int ret;
    ret = int_shmem_put(target, source, len, pe);
    int_shmem_put_wait(ret);
}


void
shmem_float_get(float *target, const float *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(float) * len, pe);
    int_shmem_get_wait();
}


void
shmem_double_get(double *target, const double *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(double) * len, pe);
    int_shmem_get_wait();
}


void
shmem_longdouble_get(long double *target, const long double *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(long double) * len, pe);
    int_shmem_get_wait();
}


void
shmem_short_get(short *target, const short *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(short) * len, pe);
    int_shmem_get_wait();
}


void
shmem_int_get(int *target, const int *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(int) * len, pe);
    int_shmem_get_wait();
}


void
shmem_long_get(long *target, const long *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(long) * len, pe);
    int_shmem_get_wait();
}


void
shmem_longlong_get(long long *target, const long long *source, size_t len, int pe)
{
    int_shmem_get(target, source, sizeof(long long) * len, pe);
    int_shmem_get_wait();
}


void
shmem_get32(void *target, const void *source, size_t len, int pe)
{
    int_shmem_get(target, source, 4 * len, pe);
    int_shmem_get_wait();
}


void
shmem_get64(void *target, const void *source, size_t len, int pe)
{
    int_shmem_get(target, source, 8 * len, pe);
    int_shmem_get_wait();
}


void
shmem_get128(void *target, const void *source, size_t len, int pe)
{
    int_shmem_get(target, source, 16 * len, pe);
    int_shmem_get_wait();
}


void
shmem_getmem(void *target, const void *source, size_t len, int pe)
{
    int_shmem_get(target, source, len, pe);
    int_shmem_get_wait();
}


void
shmem_float_iput(float *target, const float *source, ptrdiff_t tst, ptrdiff_t sst,
                 size_t len, int pe)
{
    int ret = 0;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, sizeof(float), pe);
	target += tst;
	source += sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_double_iput(double *target, const double *source, ptrdiff_t tst,
                  ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, sizeof(double), pe);
	target += tst;
	source += sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_longdouble_iput(long double *target, const long double *source,
                      ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, sizeof(long double), pe);
	target += tst;
	source += sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_short_iput(short *target, const short *source, ptrdiff_t tst, ptrdiff_t sst,
                 size_t len, int pe)
{
    int ret = 0;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, sizeof(short), pe);
	target += tst;
	source += sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_int_iput(int *target, const int *source, ptrdiff_t tst, ptrdiff_t sst,
               size_t len, int pe)
{
    int ret = 0;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, sizeof(int), pe);
	target += tst;
	source += sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_long_iput(long *target, const long *source, ptrdiff_t tst,
                ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, sizeof(long), pe);
	target += tst;
	source += sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_longlong_iput(long long *target, const long long *source, ptrdiff_t tst,
                    ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, sizeof(long long), pe);
	target += tst;
	source += sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_iput32(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst,
             size_t len, int pe)
{
    int ret = 0;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, sizeof(uint32_t), pe);
	target = (uint32_t*)target + tst;
	source = (uint32_t*)source + sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_iput64(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst,
             size_t len, int pe)
{
    int ret = 0;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, sizeof(uint64_t), pe);
	target = (uint64_t*)target + tst;
	source = (uint64_t*)source + sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_iput128(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    int ret = 0;
    tst *= 16;
    sst *= 16;
    for ( ; len > 0 ; --len) {
        ret = int_shmem_put(target, source, 16, pe);
	target = (uint8_t*)target + tst;
	source = (uint8_t*)source + sst;
    }
    int_shmem_put_wait(ret);
}


void
shmem_float_iget(float *target, const float *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, sizeof(float), pe);
	target += tst;
	source += sst;
    }
    int_shmem_get_wait();
}


void
shmem_double_iget(double *target, const double *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, sizeof(double), pe);
	target += tst;
	source += sst;
    }
    int_shmem_get_wait();
}


void
shmem_longdouble_iget(long double *target, const long double *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, sizeof(long double), pe);
	target += tst;
	source += sst;
    }
    int_shmem_get_wait();
}


void
shmem_short_iget(short *target, const short *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, sizeof(short), pe);
	target += tst;
	source += sst;
    }
    int_shmem_get_wait();
}


void
shmem_int_iget(int *target, const int *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, sizeof(int), pe);
	target += tst;
	source += sst;
    }
    int_shmem_get_wait();
}


void
shmem_long_iget(long *target, const long *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, sizeof(long), pe);
	target += tst;
	source += sst;
    }
    int_shmem_get_wait();
}


void
shmem_longlong_iget(long long *target, const long long *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, sizeof(long long), pe);
	target += tst;
	source += sst;
    }
    int_shmem_get_wait();
}


void
shmem_iget32(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, sizeof(uint32_t), pe);
	target = (uint32_t*)target + tst;
	source = (uint32_t*)source + sst;
    }
    int_shmem_get_wait();
}


void
shmem_iget64(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, sizeof(uint64_t), pe);
	target = (uint64_t*)target + tst;
	source = (uint64_t*)source + sst;
    }
    int_shmem_get_wait();
}


void
shmem_iget128(void *target, const void *source, ptrdiff_t tst, ptrdiff_t sst, size_t len, int pe)
{
    tst *= 16;
    sst *= 16;
    for ( ; len > 0 ; --len ) {
        int_shmem_get(target, source, 16, pe);
	target = (uint8_t*)target + tst;
	source = (uint8_t*)source + sst;
    }
    int_shmem_get_wait();
}

