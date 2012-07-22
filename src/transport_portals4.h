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

#ifndef TRANSPORT_PORTALS_H
#define TRANSPORT_PORTALS_H

#include <portals4.h>
#include <string.h>

#include "shmem_free_list.h"

#define DATA_IDX 10
#define HEAP_IDX 11

extern ptl_handle_ni_t shmem_transport_portals4_ni_h;
extern ptl_pt_index_t shmem_transport_portals4_data_pt;
extern ptl_pt_index_t shmem_transport_portals4_heap_pt;
extern ptl_handle_md_t shmem_transport_portals4_put_volatile_md_h;
extern ptl_handle_md_t shmem_transport_portals4_put_event_md_h;
extern ptl_handle_md_t shmem_transport_portals4_get_md_h;
extern ptl_handle_ct_t shmem_transport_portals4_target_ct_h;
extern ptl_handle_ct_t shmem_transport_portals4_put_ct_h;
extern ptl_handle_ct_t shmem_transport_portals4_get_ct_h;
extern ptl_handle_eq_t shmem_transport_portals4_eq_h;

extern shmem_free_list_t *shmem_transport_portals4_bounce_buffers;
extern shmem_free_list_t *shmem_transport_portals4_long_frags;

extern ptl_size_t shmem_transport_portals4_bounce_buffer_size;
extern ptl_size_t shmem_transport_portals4_max_volatile_size;
extern ptl_size_t shmem_transport_portals4_max_atomic_size;
extern ptl_size_t shmem_transport_portals4_max_fetch_atomic_size;

extern ptl_size_t shmem_transport_portals4_pending_put_counter;
extern ptl_size_t shmem_transport_portals4_pending_get_counter;

#define SHMEM_TRANSPORT_PORTALS4_TYPE_BOUNCE  0x01
#define SHMEM_TRANSPORT_PORTALS4_TYPE_LONG    0x02

struct shmem_transport_portals4_frag_t {
    shmem_free_list_item_t item;
    char type;
};
typedef struct shmem_transport_portals4_frag_t shmem_transport_portals4_frag_t;

struct shmem_transport_portals4_bounce_buffer_t {
    shmem_transport_portals4_frag_t frag;
    char data[];
};
typedef struct shmem_transport_portals4_bounce_buffer_t shmem_transport_portals4_bounce_buffer_t;

struct shmem_transport_portals4_long_frag_t {
    shmem_transport_portals4_frag_t frag;
    char complete;
};
typedef struct shmem_transport_portals4_long_frag_t shmem_transport_portals4_long_frag_t;

#ifdef ENABLE_ERROR_CHECKING
#define PORTALS4_GET_REMOTE_ACCESS(target, pt, offset)                  \
    do {                                                                \
        if (((void*) target > shmem_internal_data_base) &&              \
            ((char*) target < (char*) shmem_internal_data_base + shmem_internal_data_length)) { \
            pt = shmem_transport_portals4_data_pt;                      \
            offset = (char*) target - (char*) shmem_internal_data_base; \
        } else if (((void*) target > shmem_internal_heap_base) &&       \
                   ((char*) target < (char*) shmem_internal_heap_base + shmem_internal_heap_length)) { \
            pt = shmem_transport_portals4_heap_pt;                      \
            offset = (char*) target - (char*) shmem_internal_heap_base; \
        } else {                                                        \
            printf("[%03d] ERROR: target (0x%lx) outside of symmetric areas\n", \
                   shmem_internal_my_pe, (unsigned long) target);       \
            abort();                                                    \
        }                                                               \
    } while (0)
#else 
#define PORTALS4_GET_REMOTE_ACCESS(target, pt, offset)                  \
    do {                                                                \
        if ((void*) target < shmem_internal_heap_base) {                \
            pt = shmem_transport_portals4_data_pt;                      \
            offset = (char*) target - (char*) shmem_internal_data_base; \
        } else {                                                        \
            pt = shmem_transport_portals4_heap_pt;                      \
            offset = (char*) target - (char*) shmem_internal_heap_base; \
        }                                                               \
    } while (0)
#endif

int shmem_transport_portals4_init(long eager_size);

int shmem_transport_portals4_startup(void);

int shmem_transport_portals4_fini(void);

static inline
int
shmem_transport_portals4_quiet(void)
{
    int ret;
    ptl_ct_event_t ct;

    /* wait for remote completion (acks) of all pending events */
    ret = PtlCTWait(shmem_transport_portals4_put_ct_h, 
                    shmem_transport_portals4_pending_put_counter, &ct);
    if (PTL_OK != ret) { return ret; }
    if (ct.failure != 0) { return -1; }

    return 0;
}


static inline
int
shmem_transport_portals4_fence(void)
{
    int ret;
    ptl_ct_event_t ct;

    /* wait for remote completion (acks) of all pending events */
    ret = PtlCTWait(shmem_transport_portals4_put_ct_h, 
                    shmem_transport_portals4_pending_put_counter, &ct);
    if (PTL_OK != ret) { return ret; }
    if (ct.failure != 0) { return -1; }

    return 0;
}

        
static inline
int
shmem_transport_portals4_put(void *target, const void *source, size_t len, int pe)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    int tmp;
    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    if (len <= shmem_transport_portals4_max_volatile_size) {
        ret = PtlPut(shmem_transport_portals4_put_volatile_md_h,
                     (ptl_size_t) source,
                     len,
                     PTL_CT_ACK_REQ,
                     peer,
                     pt,
                     0,
                     offset,
                     NULL,
                     0);
        if (PTL_OK != ret) { RAISE_ERROR(ret); }
        tmp = 0;

    } else if (len <= shmem_transport_portals4_bounce_buffer_size) {
        shmem_transport_portals4_bounce_buffer_t *buff =
            (shmem_transport_portals4_bounce_buffer_t*)
            shmem_free_list_alloc(shmem_transport_portals4_bounce_buffers);
        if (NULL == buff) RAISE_ERROR(-1);

        memcpy(buff->data, source, len);
        ret = PtlPut(shmem_transport_portals4_put_event_md_h,
                     (ptl_size_t) buff->data,
                     len,
                     PTL_CT_ACK_REQ,
                     peer,
                     pt,
                     0,
                     offset,
                     buff,
                     0);
        if (PTL_OK != ret) { RAISE_ERROR(ret); }
        tmp = 0;

    } else {
        ret = PtlPut(shmem_transport_portals4_put_event_md_h,
                     (ptl_size_t) source,
                     len,
                     PTL_CT_ACK_REQ,
                     peer,
                     pt,
                     0,
                     offset,
                     NULL,
                     0);
        if (PTL_OK != ret) { RAISE_ERROR(ret); } 
        tmp = 1;
    }
    shmem_transport_portals4_pending_put_counter += 1;
    
    return tmp;
}


static inline
void
shmem_transport_portals4_put_wait(int count)
{
    int ret;
    ptl_event_t ev;

    while (count > 0) {
        ret = PtlEQWait(shmem_transport_portals4_eq_h, &ev);
        if (PTL_OK != ret) { RAISE_ERROR(ret); }
        if (ev.ni_fail_type != PTL_OK) { RAISE_ERROR(ev.ni_fail_type); }

        if (NULL == ev.user_ptr) {
            /* it's one of the long messages we're waiting for */
            count--;
        } else {
            /* it's a short send completing */
            shmem_free_list_free(shmem_transport_portals4_bounce_buffers,
                                 ev.user_ptr);
        }
    }
}


static inline
void
shmem_transport_portals4_get(void *target, const void *source, size_t len, int pe)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(source, pt, offset);

    ret = PtlGet(shmem_transport_portals4_get_md_h,
                 (ptl_size_t) target,
                 len,
                 peer,
                 pt,
                 0,
                 offset,
                 0);
    if (PTL_OK != ret) { RAISE_ERROR(ret); }
    shmem_transport_portals4_pending_get_counter++;
}


static inline
void
shmem_transport_portals4_get_wait(void)
{
    int ret;
    ptl_ct_event_t ct;

    ret = PtlCTWait(shmem_transport_portals4_get_ct_h, 
                    shmem_transport_portals4_pending_get_counter,
                    &ct);
    if (PTL_OK != ret) { RAISE_ERROR(ret); }
    if (ct.failure != 0) { RAISE_ERROR(ct.failure); }
}


static inline
int
shmem_transport_portals4_swap(void *target, void *source, void *dest, size_t len, 
                    int pe, ptl_datatype_t datatype)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= sizeof(long double complex));

    ret = PtlSwap(shmem_transport_portals4_get_md_h,
                  (ptl_size_t) dest,
                  shmem_transport_portals4_put_event_md_h,
                  (ptl_size_t) source,
                  len,
                  peer,
                  pt,
                  0,
                  offset,
                  NULL,
                  0,
                  NULL,
                  PTL_SWAP,
                  datatype);
    if (PTL_OK != ret) { RAISE_ERROR(ret); }
    shmem_transport_portals4_pending_get_counter++;

    return 1;
}


static inline
int
shmem_transport_portals4_cswap(void *target, void *source, void *dest, void *operand, size_t len, 
                     int pe, ptl_datatype_t datatype)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= sizeof(long double complex));

    ret = PtlSwap(shmem_transport_portals4_get_md_h,
                  (ptl_size_t) dest,
                  shmem_transport_portals4_put_event_md_h,
                  (ptl_size_t) source,
                  len,
                  peer,
                  pt,
                  0,
                  offset,
                  NULL,
                  0,
                  operand,
                  PTL_CSWAP,
                  datatype);
    if (PTL_OK != ret) { RAISE_ERROR(ret); }
    shmem_transport_portals4_pending_get_counter++;

    return 1;
}


static inline
int
shmem_transport_portals4_mswap(void *target, void *source, void *dest, void *mask, size_t len, 
                     int pe, ptl_datatype_t datatype)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= sizeof(long double complex));

    ret = PtlSwap(shmem_transport_portals4_get_md_h,
                  (ptl_size_t) dest,
                  shmem_transport_portals4_put_event_md_h,
                  (ptl_size_t) source,
                  len,
                  peer,
                  pt,
                  0,
                  offset,
                  NULL,
                  0,
                  mask,
                  PTL_MSWAP,
                  datatype);
    if (PTL_OK != ret) { RAISE_ERROR(ret); }
    shmem_transport_portals4_pending_get_counter++;

    return 1;
}


static inline
int
shmem_transport_portals4_atomic(void *target, void *source, size_t len,
                      int pe, ptl_op_t op, ptl_datatype_t datatype)
{
    int ret;
    ptl_pt_index_t pt;
    long offset;
    ptl_process_t peer;
    int tmp = 0;
    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    if (len <= shmem_transport_portals4_max_volatile_size) {
        ret = PtlAtomic(shmem_transport_portals4_put_volatile_md_h,
                        (ptl_size_t) source,
                        len,
                        PTL_CT_ACK_REQ,
                        peer,
                        pt,
                        0,
                        offset,
                        NULL,
                        0,
                        op,
                        datatype);
        if (PTL_OK != ret) { RAISE_ERROR(ret); }
        shmem_transport_portals4_pending_put_counter += 1;
        tmp = 0;

    } else if (len <= MIN(shmem_transport_portals4_bounce_buffer_size,
                          shmem_transport_portals4_max_atomic_size)) {
        shmem_transport_portals4_bounce_buffer_t *buff =
            (shmem_transport_portals4_bounce_buffer_t*)
            shmem_free_list_alloc(shmem_transport_portals4_bounce_buffers);
        if (NULL == buff) RAISE_ERROR(-1);

        memcpy(buff->data, source, len);
        ret = PtlAtomic(shmem_transport_portals4_put_event_md_h,
                        (ptl_size_t) buff->data,
                        len,
                        PTL_CT_ACK_REQ,
                        peer,
                        pt,
                        0,
                        offset,
                        buff,
                        0,
                        op,
                        datatype);
        if (PTL_OK != ret) { RAISE_ERROR(ret); }
        shmem_transport_portals4_pending_put_counter += 1;
        tmp = 0;

    } else {
        size_t sent = 0;

        while (sent < len) {
            size_t bufsize = MIN(len - sent, shmem_transport_portals4_max_atomic_size);
            ret = PtlAtomic(shmem_transport_portals4_put_event_md_h,
                            (ptl_size_t) ((char*) source + sent),
                            bufsize,
                            PTL_CT_ACK_REQ,
                            peer,
                            pt,
                            0,
                            offset + sent,
                            NULL,
                            0,
                            op,
                            datatype);
            if (PTL_OK != ret) { RAISE_ERROR(ret); }
            tmp++;
            sent += bufsize;
        }
        shmem_transport_portals4_pending_put_counter += tmp;
    }

    return tmp;
}


static inline
int
shmem_transport_portals4_fetch_atomic(void *target, void *source, void *dest, size_t len,
                            int pe, ptl_op_t op, ptl_datatype_t datatype)
{
    int ret;
    ptl_pt_index_t pt;
    long offset;
    ptl_process_t peer;
    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= shmem_transport_portals4_max_fetch_atomic_size);

    ret = PtlFetchAtomic(shmem_transport_portals4_get_md_h,
                         (ptl_size_t) dest,
                         shmem_transport_portals4_put_volatile_md_h,
                         (ptl_size_t) source,
                         len,
                         peer,
                         pt,
                         0,
                         offset,
                         NULL,
                         0,
                         op,
                         datatype);
    if (PTL_OK != ret) { RAISE_ERROR(ret); }
    shmem_transport_portals4_pending_get_counter++;

    return 0;
}

#endif
