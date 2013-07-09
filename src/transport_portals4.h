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

#define shmem_transport_portals4_data_pt 8
#define shmem_transport_portals4_heap_pt 9

extern ptl_handle_ni_t shmem_transport_portals4_ni_h;
#if PORTALS4_MAX_MD_SIZE < PORTALS4_MAX_VA_SIZE
extern ptl_handle_md_t *shmem_transport_portals4_put_volatile_md_h;
extern ptl_handle_md_t *shmem_transport_portals4_put_event_md_h;
extern ptl_handle_md_t *shmem_transport_portals4_get_md_h;
#else
extern ptl_handle_md_t shmem_transport_portals4_put_volatile_md_h[1];
extern ptl_handle_md_t shmem_transport_portals4_put_event_md_h[1];
extern ptl_handle_md_t shmem_transport_portals4_get_md_h[1];
#endif
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

extern int32_t shmem_transport_portals4_event_slots;

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
    int reference;
    long *completion;
};
typedef struct shmem_transport_portals4_long_frag_t shmem_transport_portals4_long_frag_t;

/*
 * Not all implementations of Portals 4 support binding a memory
 * descriptor which covers all of memory, but all support covering a
 * large fraction of memory.  Therefore, rather than working around
 * the issue by pinning per message, we use a number of memory
 * descriptors to cover all of memory.  As long as the maximum memory
 * descriptor is a large fraction of the user virtual address space
 * (like 46 bit MDs on a platform with 47 bits of user virtual address
 * space), this works fine.
 *
 * Our scheme is to create N memory descriptors which contiguously
 * cover the entire user address space, then another N-1 contiguous
 * memory descriptors offset by 1/2 the size of the MD, then a final
 * memory descriptor of 1/2 the size of the other MDs covering the top
 * of the memory space, to avoid if statements in the critical path.  This
 * scheme allows for a maximum message size of 1/2 the size of the MD
 * without ever crossing an MD boundary.  Also, because MD sizes are
 * always on a power of 2 in this scheme, computing the offsets and MD
 * selection are quick, using only bit shift and mask.
 *
 * shmem_transport_portals4_get_md() relies heavily on compiler
 * constant folding.  "mask" can be constant folded into a constant.
 * "which" compiler folds into a bit shift of a register a constant
 * number of times, then masked by a constant (the input is,
 * unfortunately, not constant).
 *
 * In the case where an MD can cover all of memory,
 * shmem_transport_mtl_portals4_get_md() will be compiled into two
 * assignments.  Assuming the function inlines (and it certainly
 * should be), the two assignments should be optimized into register
 * assignments for the Portals call relatively easily.
 */
static inline void
shmem_transport_portals4_get_md(const void *ptr, const ptl_handle_md_t *md_array,
                                ptl_handle_md_t *md_h, void **base_ptr)
{
#if PORTALS4_MAX_MD_SIZE < PORTALS4_MAX_VA_SIZE
    int mask = (1ULL << (PORTALS4_MAX_VA_SIZE - PORTALS4_MAX_MD_SIZE + 1)) - 1;
    int which = (((uintptr_t) ptr) >> (PORTALS4_MAX_MD_SIZE - 1)) & mask;
    *md_h = md_array[which];
    *base_ptr = (void*) (which * (1ULL << (PORTALS4_MAX_MD_SIZE - 1)));
#else
    *md_h = md_array[0];
    *base_ptr = 0;
#endif
}


static inline int
shmem_transport_portals4_get_num_mds(void)
{
#if PORTALS4_MAX_MD_SIZE < PORTALS4_MAX_VA_SIZE
    return (1 << (PORTALS4_MAX_VA_SIZE - PORTALS4_MAX_MD_SIZE + 1));
#else
    return 1;
#endif
}

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
    return shmem_transport_portals4_quiet();
}


static inline
void
shmem_transport_portals4_drain_eq(void)
{
    int ret;
    ptl_event_t ev;

    ret = PtlEQWait(shmem_transport_portals4_eq_h, &ev);
    if (PTL_OK != ret) { RAISE_ERROR(ret); }
    if (ev.ni_fail_type != PTL_OK) { RAISE_ERROR(ev.ni_fail_type); }

    /* The only event type we should see on a success is a send event */
    assert(ev.type == PTL_EVENT_SEND);

    shmem_transport_portals4_event_slots++;

    shmem_transport_portals4_frag_t *frag = 
         (shmem_transport_portals4_frag_t*) ev.user_ptr;

    if (SHMEM_TRANSPORT_PORTALS4_TYPE_BOUNCE == frag->type) {
         /* it's a short send completing */
         shmem_free_list_free(shmem_transport_portals4_bounce_buffers,
                              frag);
    } else {
         /* it's one of the long messages we're waiting for */
         shmem_transport_portals4_long_frag_t *long_frag = 
              (shmem_transport_portals4_long_frag_t*) frag;

         (*(long_frag->completion))--;
         if (0 >= --long_frag->reference) {
              long_frag->reference = 0;
              shmem_free_list_free(shmem_transport_portals4_long_frags,
                                   frag);
         }
    }
}


static inline
void
shmem_transport_portals4_put_small(void *target, const void *source, size_t len, int pe)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    ptl_handle_md_t md_h;
    void *base;

    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= shmem_transport_portals4_max_volatile_size);

    shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_volatile_md_h,
                                    &md_h, &base);

    ret = PtlPut(md_h,
                 (ptl_size_t) ((char*) source - (char*) base),
                 len,
                 PTL_CT_ACK_REQ,
                 peer,
                 pt,
                 0,
                 offset,
                 NULL,
                 0);
    if (PTL_OK != ret) { RAISE_ERROR(ret); }
    shmem_transport_portals4_pending_put_counter++;
}


static inline
void
shmem_transport_portals4_put_nb(void *target, const void *source, size_t len,
                                int pe, long *completion)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    ptl_handle_md_t md_h;
    void *base;

    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    if (len <= shmem_transport_portals4_max_volatile_size) {
        shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_volatile_md_h,
                                        &md_h, &base);

        ret = PtlPut(md_h,
                     (ptl_size_t) ((char*) source - (char*) base),
                     len,
                     PTL_CT_ACK_REQ,
                     peer,
                     pt,
                     0,
                     offset,
                     NULL,
                     0);
        if (PTL_OK != ret) { RAISE_ERROR(ret); }

    } else if (len <= shmem_transport_portals4_bounce_buffer_size) {
        shmem_transport_portals4_bounce_buffer_t *buff;

        while (0 >= --shmem_transport_portals4_event_slots) {
            shmem_transport_portals4_event_slots++;
            shmem_transport_portals4_drain_eq();
        }

        buff = (shmem_transport_portals4_bounce_buffer_t*)
            shmem_free_list_alloc(shmem_transport_portals4_bounce_buffers);
        if (NULL == buff) RAISE_ERROR(-1);

        assert(buff->frag.type == SHMEM_TRANSPORT_PORTALS4_TYPE_BOUNCE);

        memcpy(buff->data, source, len);

        shmem_transport_portals4_get_md(buff->data, shmem_transport_portals4_put_event_md_h,
                                        &md_h, &base);

        ret = PtlPut(md_h,
                     (ptl_size_t) ((char*) buff->data - (char*) base),
                     len,
                     PTL_CT_ACK_REQ,
                     peer,
                     pt,
                     0,
                     offset,
                     buff,
                     0);
        if (PTL_OK != ret) { RAISE_ERROR(ret); }

    } else {
        shmem_transport_portals4_long_frag_t *long_frag;  

        while (0 >= --shmem_transport_portals4_event_slots) {
            shmem_transport_portals4_event_slots++;
            shmem_transport_portals4_drain_eq();
        }

        long_frag = (shmem_transport_portals4_long_frag_t*)
            shmem_free_list_alloc(shmem_transport_portals4_long_frags);
        if (NULL == long_frag) RAISE_ERROR(-1);

        assert(long_frag->frag.type == SHMEM_TRANSPORT_PORTALS4_TYPE_LONG);
        assert(long_frag->reference == 0);
        long_frag->completion = completion;

        shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_event_md_h,
                                        &md_h, &base);

        ret = PtlPut(md_h,
                     (ptl_size_t) ((char*) source - (char*) base),
                     len,
                     PTL_CT_ACK_REQ,
                     peer,
                     pt,
                     0,
                     offset,
                     long_frag,
                     0);
        if (PTL_OK != ret) { RAISE_ERROR(ret); } 
        (*(long_frag->completion))++;
        long_frag->reference++;
    }
    shmem_transport_portals4_pending_put_counter++;
}


static inline
void
shmem_transport_portals4_put_wait(long *completion)
{
    while (*completion > 0) {
        shmem_transport_portals4_drain_eq();
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
    ptl_handle_md_t md_h;
    void *base;

    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(source, pt, offset);

    shmem_transport_portals4_get_md(target, shmem_transport_portals4_get_md_h,
                                    &md_h, &base);

    ret = PtlGet(md_h,
                 (ptl_size_t) ((char*) target - (char*) base),
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
void
shmem_transport_portals4_swap(void *target, void *source, void *dest, size_t len, 
                              int pe, ptl_datatype_t datatype)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    ptl_handle_md_t get_md_h;
    void *get_base;
    ptl_handle_md_t put_md_h;
    void *put_base;

    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= sizeof(long double complex));
    assert(len <= shmem_transport_portals4_max_volatile_size);

    shmem_transport_portals4_get_md(dest, shmem_transport_portals4_get_md_h,
                                    &get_md_h, &get_base);
    shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_volatile_md_h,
                                    &put_md_h, &put_base);

    /* note: No ack is generated on the ct associated with the
       volatile md because the reply comes back on the get md.  So no
       need to increment the put counter */
    ret = PtlSwap(get_md_h,
                  (ptl_size_t) ((char*) dest - (char*) get_base),
                  put_md_h,
                  (ptl_size_t) ((char*) source - (char*) put_base),
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
}


static inline
void
shmem_transport_portals4_cswap(void *target, void *source, void *dest, void *operand, size_t len, 
                               int pe, ptl_datatype_t datatype)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    ptl_handle_md_t get_md_h;
    void *get_base;
    ptl_handle_md_t put_md_h;
    void *put_base;

    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= sizeof(long double complex));
    assert(len <= shmem_transport_portals4_max_volatile_size);

    shmem_transport_portals4_get_md(dest, shmem_transport_portals4_get_md_h,
                                    &get_md_h, &get_base);
    shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_volatile_md_h,
                                    &put_md_h, &put_base);

    /* note: No ack is generated on the ct associated with the
       volatile md because the reply comes back on the get md.  So no
       need to increment the put counter */
    ret = PtlSwap(get_md_h,
                  (ptl_size_t) ((char*) dest - (char*) get_base),
                  put_md_h,
                  (ptl_size_t) ((char*) source - (char*) put_base),
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
}


static inline
void
shmem_transport_portals4_mswap(void *target, void *source, void *dest, void *mask, size_t len, 
                               int pe, ptl_datatype_t datatype)
{
    int ret;
    ptl_process_t peer;
    ptl_pt_index_t pt;
    long offset;
    ptl_handle_md_t get_md_h;
    void *get_base;
    ptl_handle_md_t put_md_h;
    void *put_base;

    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= sizeof(long double complex));
    assert(len <= shmem_transport_portals4_max_volatile_size);

    shmem_transport_portals4_get_md(dest, shmem_transport_portals4_get_md_h,
                                    &get_md_h, &get_base);
    shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_volatile_md_h,
                                    &put_md_h, &put_base);

    /* note: No ack is generated on the ct associated with the
       volatile md because the reply comes back on the get md.  So no
       need to increment the put counter */
    ret = PtlSwap(get_md_h,
                  (ptl_size_t) ((char*) dest - (char*) get_base),
                  put_md_h,
                  (ptl_size_t) ((char*) source - (char*) put_base),
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
}


static inline
void
shmem_transport_portals4_atomic_small(void *target, void *source, size_t len,
                                       int pe, ptl_op_t op, ptl_datatype_t datatype)
{
    int ret;
    ptl_pt_index_t pt;
    long offset;
    ptl_process_t peer;
    ptl_handle_md_t md_h;
    void *base;

    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= shmem_transport_portals4_max_volatile_size);

    shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_volatile_md_h,
                                    &md_h, &base);

    ret = PtlAtomic(md_h,
                    (ptl_size_t) ((char*) source - (char*) base),
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
}


static inline
void
shmem_transport_portals4_atomic_nb(void *target, void *source, size_t len,
                                   int pe, ptl_op_t op, ptl_datatype_t datatype,
                                   long *completion)
{
    int ret;
    ptl_pt_index_t pt;
    long offset;
    ptl_process_t peer;
    ptl_handle_md_t md_h;
    void *base;

    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    if (len <= shmem_transport_portals4_max_volatile_size) {
        shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_volatile_md_h,
                                        &md_h, &base);

        ret = PtlAtomic(md_h,
                        (ptl_size_t) ((char*) source - (char*) base),
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
        shmem_transport_portals4_pending_put_counter++;

    } else if (len <= MIN(shmem_transport_portals4_bounce_buffer_size,
                          shmem_transport_portals4_max_atomic_size)) {
        shmem_transport_portals4_bounce_buffer_t *buff;

        while (0 >= --shmem_transport_portals4_event_slots) {
            shmem_transport_portals4_event_slots++;
            shmem_transport_portals4_drain_eq();
        }

        buff = (shmem_transport_portals4_bounce_buffer_t*)
            shmem_free_list_alloc(shmem_transport_portals4_bounce_buffers);
        if (NULL == buff) RAISE_ERROR(-1);

        assert(buff->frag.type == SHMEM_TRANSPORT_PORTALS4_TYPE_BOUNCE);

        memcpy(buff->data, source, len);

        shmem_transport_portals4_get_md(buff->data, shmem_transport_portals4_put_event_md_h,
                                        &md_h, &base);

        ret = PtlAtomic(md_h,
                        (ptl_size_t) ((char*) buff->data - (char*) base),
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

    } else {
        size_t sent = 0;
        ptl_size_t base_offset;
        shmem_transport_portals4_long_frag_t *long_frag =
            (shmem_transport_portals4_long_frag_t*)
             shmem_free_list_alloc(shmem_transport_portals4_long_frags);
        if (NULL == long_frag) RAISE_ERROR(-1);

        assert(long_frag->frag.type == SHMEM_TRANSPORT_PORTALS4_TYPE_LONG);
        assert(long_frag->reference == 0);

        long_frag->completion = completion;

        shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_event_md_h,
                                        &md_h, &base);
        base_offset = (ptl_size_t) ((char*) source - (char*) base);

        while (sent < len) {
             while (0 >= --shmem_transport_portals4_event_slots) {
                  shmem_transport_portals4_event_slots++;
                  shmem_transport_portals4_drain_eq();
             }

            size_t bufsize = MIN(len - sent, shmem_transport_portals4_max_atomic_size);
            ret = PtlAtomic(shmem_transport_portals4_put_event_md_h[0],
                            base_offset + sent,
                            bufsize,
                            PTL_CT_ACK_REQ,
                            peer,
                            pt,
                            0,
                            offset + sent,
                            long_frag,
                            0,
                            op,
                            datatype);
            if (PTL_OK != ret) { RAISE_ERROR(ret); }
            (*(long_frag->completion))++;
            long_frag->reference++;
            shmem_transport_portals4_pending_put_counter++;
            sent += bufsize;
        }
    }
}


static inline
void
shmem_transport_portals4_fetch_atomic(void *target, void *source, void *dest, size_t len,
                                      int pe, ptl_op_t op, ptl_datatype_t datatype)
{
    int ret;
    ptl_pt_index_t pt;
    long offset;
    ptl_process_t peer;
    ptl_handle_md_t get_md_h;
    void *get_base;
    ptl_handle_md_t put_md_h;
    void *put_base;

    peer.rank = pe;
    PORTALS4_GET_REMOTE_ACCESS(target, pt, offset);

    assert(len <= shmem_transport_portals4_max_fetch_atomic_size);
    assert(len <= shmem_transport_portals4_max_volatile_size);

    shmem_transport_portals4_get_md(source, shmem_transport_portals4_get_md_h,
                                    &get_md_h, &get_base);
    shmem_transport_portals4_get_md(source, shmem_transport_portals4_put_volatile_md_h,
                                    &put_md_h, &put_base);

    /* note: No ack is generated on the ct associated with the
       volatile md because the reply comes back on the get md.  So no
       need to increment the put counter */
    ret = PtlFetchAtomic(get_md_h,
                         (ptl_size_t) ((char*) dest - (char*) get_base),
                         put_md_h,
                         (ptl_size_t) ((char*) source - (char*) put_base),
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
}

#endif
