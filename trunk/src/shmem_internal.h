/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#ifndef PORTALS_SHMEM_INTERNAL_H
#define PORTALS_SHMEM_INTERNAL_H

#define DATA_IDX 10
#define HEAP_IDX 11

extern ptl_handle_ni_t ni_h;
extern ptl_pt_index_t data_pt;
extern ptl_pt_index_t heap_pt;
extern ptl_handle_md_t put_md_h;
extern ptl_handle_md_t get_md_h;
extern ptl_handle_ct_t target_ct_h;
extern ptl_handle_ct_t put_ct_h;
extern ptl_handle_ct_t get_ct_h;
#ifdef ENABLE_EVENT_COMPLETION
extern ptl_handle_eq_t put_eq_h;
#endif
extern ptl_handle_eq_t err_eq_h;
extern ptl_size_t max_put_size;
extern ptl_size_t max_atomic_size;
extern ptl_size_t max_fetch_atomic_size;

extern ptl_size_t pending_put_counter;
extern ptl_size_t pending_get_counter;

extern void *shmem_heap_base;
extern long shmem_heap_length;
extern void *shmem_data_base;
extern long shmem_data_length;

extern int shmem_int_my_pe;
extern int shmem_int_num_pes;


#define GET_REMOTE_ACCESS(target, pt, offset)                           \
    do {                                                                \
        if (((void*) target > shmem_data_base) &&                       \
            ((char*) target < (char*) shmem_data_base + shmem_data_length)) { \
            pt = data_pt;                                               \
            offset = (char*) target - (char*) shmem_data_base;          \
        } else if (((void*) target > shmem_heap_base) &&                \
                   ((char*) target < (char*) shmem_heap_base + shmem_heap_length)) { \
        pt = heap_pt;                                                   \
        offset = (char*) target - (char*) shmem_heap_base;              \
    } else {                                                            \
        printf("target outside of symmetric areas\n");                  \
        abort();                                                        \
    }                                                                   \
    } while (0)



static inline
int
shmem_internal_put(void *target, const void *source, size_t len, int pe)
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
shmem_internal_put_wait(int count)
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
shmem_internal_get(void *target, const void *source, size_t len, int pe)
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
shmem_internal_get_wait(void)
{
    int ret;
    ptl_ct_event_t ct;

    ret = PtlCTWait(get_ct_h, pending_get_counter, &ct);
    if (PTL_OK != ret) { abort(); }
    if (ct.failure != 0) { abort(); }
}



/* initialization routines */
int symmetric_init(void);
int shmem_barrier_init(void);

#endif
