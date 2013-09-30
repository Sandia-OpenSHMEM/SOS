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
#include <stdlib.h>
#include <sys/time.h>
#include <sys/param.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"
#include "runtime.h"

int8_t shmem_transport_portals4_pt_state[SHMEM_TRANSPORT_PORTALS4_NUM_PTS] = {
    /*  0 */ PT_FREE,
    /*  1 */ PT_FREE,
    /*  2 */ PT_FREE,
    /*  3 */ PT_FREE,
    /*  4 */ PT_FREE,
    /*  5 */ PT_FREE,
    /*  6 */ PT_RESERVED,
    /*  7 */ PT_FREE,
    /*  8 */ PT_RESERVED,
#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    /*  9 */ PT_FREE,
#else
    /*  9 */ PT_RESERVED,
#endif
    /* 10 */ PT_FREE,
    /* 11 */ PT_FREE,
    /* 12 */ PT_FREE,
    /* 13 */ PT_FREE,
    /* 14 */ PT_FREE,
    /* 15 */ PT_FREE,
    /* 16 */ PT_RESERVED,
    /* 17 */ PT_RESERVED,
    /* 18 */ PT_RESERVED,
    /* 19 */ PT_RESERVED,
    /* 20 */ PT_RESERVED,
    /* 21 */ PT_RESERVED,
    /* 22 */ PT_FREE,
    /* 23 */ PT_FREE,
    /* 24 */ PT_FREE,
    /* 25 */ PT_FREE,
    /* 26 */ PT_FREE,
    /* 27 */ PT_FREE,
    /* 28 */ PT_FREE,
    /* 29 */ PT_FREE,
    /* 30 */ PT_FREE,
    /* 31 */ PT_FREE,
};

ptl_handle_ni_t shmem_transport_portals4_ni_h = PTL_INVALID_HANDLE;
#if PORTALS4_MAX_MD_SIZE < PORTALS4_MAX_VA_SIZE
ptl_handle_md_t *shmem_transport_portals4_put_volatile_md_h = NULL;
ptl_handle_md_t *shmem_transport_portals4_put_event_md_h = NULL;
ptl_handle_md_t *shmem_transport_portals4_get_md_h = NULL;
#else
ptl_handle_md_t shmem_transport_portals4_put_volatile_md_h[1] = { PTL_INVALID_HANDLE };
ptl_handle_md_t shmem_transport_portals4_put_event_md_h[1] = { PTL_INVALID_HANDLE };
ptl_handle_md_t shmem_transport_portals4_get_md_h[1] = { PTL_INVALID_HANDLE };
#endif
#if ENABLE_REMOTE_VIRTUAL_ADDRESSING
ptl_handle_le_t shmem_transport_portals4_le_h = PTL_INVALID_HANDLE;
#else
ptl_handle_le_t shmem_transport_portals4_data_le_h = PTL_INVALID_HANDLE;
ptl_handle_le_t shmem_transport_portals4_heap_le_h = PTL_INVALID_HANDLE;
#endif
ptl_handle_ct_t shmem_transport_portals4_target_ct_h = PTL_INVALID_HANDLE;
ptl_handle_ct_t shmem_transport_portals4_put_ct_h = PTL_INVALID_HANDLE;
ptl_handle_ct_t shmem_transport_portals4_get_ct_h = PTL_INVALID_HANDLE;
ptl_handle_eq_t shmem_transport_portals4_eq_h = PTL_INVALID_HANDLE;

shmem_free_list_t *shmem_transport_portals4_bounce_buffers = NULL;
shmem_free_list_t *shmem_transport_portals4_long_frags = NULL;

ptl_size_t shmem_transport_portals4_bounce_buffer_size = 0;
ptl_size_t shmem_transport_portals4_max_volatile_size = 0;
ptl_size_t shmem_transport_portals4_max_atomic_size = 0;
ptl_size_t shmem_transport_portals4_max_fetch_atomic_size = 0;

ptl_size_t shmem_transport_portals4_pending_put_counter = 0;
ptl_size_t shmem_transport_portals4_pending_get_counter = 0;

int32_t shmem_transport_portals4_event_slots = 2048;

static ptl_ni_limits_t ni_limits;
#if ENABLE_REMOTE_VIRTUAL_ADDRESSING
static ptl_pt_index_t all_pt = PTL_PT_ANY;
#else
static ptl_pt_index_t data_pt = PTL_PT_ANY;
static ptl_pt_index_t heap_pt = PTL_PT_ANY;
#endif

static
void
init_bounce_buffer(shmem_free_list_item_t *item)
{
    shmem_transport_portals4_frag_t *frag =
        (shmem_transport_portals4_frag_t*) item;
    frag->type = SHMEM_TRANSPORT_PORTALS4_TYPE_BOUNCE;
}

static
void
init_long_frag(shmem_free_list_item_t *item)
{
    shmem_transport_portals4_long_frag_t *frag =
        (shmem_transport_portals4_long_frag_t*) item;
    frag->frag.type = SHMEM_TRANSPORT_PORTALS4_TYPE_LONG;
    frag->reference = 0;
}


static void
cleanup_handles(void)
{
#if PORTALS4_MAX_MD_SIZE < PORTALS4_MAX_VA_SIZE
    int i;

    if (NULL != shmem_transport_portals4_get_md_h) {
        for (i = 0 ; i < shmem_transport_portals4_get_num_mds() ; ++i) {
            if (!PtlHandleIsEqual(shmem_transport_portals4_get_md_h[i], PTL_INVALID_HANDLE)) {
                PtlMDRelease(shmem_transport_portals4_get_md_h[i]);
            }
        }
        free(shmem_transport_portals4_get_md_h);
    }
    if (NULL != shmem_transport_portals4_put_event_md_h) {
        for (i = 0 ; i < shmem_transport_portals4_get_num_mds() ; ++i) {
            if (!PtlHandleIsEqual(shmem_transport_portals4_put_event_md_h[i], PTL_INVALID_HANDLE)) {
                PtlMDRelease(shmem_transport_portals4_put_event_md_h[i]);
            }
        }
        free(shmem_transport_portals4_put_event_md_h);
    }
    if (NULL != shmem_transport_portals4_put_volatile_md_h) {
        for (i = 0 ; i < shmem_transport_portals4_get_num_mds() ; ++i) {
            if (!PtlHandleIsEqual(shmem_transport_portals4_put_volatile_md_h[i], PTL_INVALID_HANDLE)) {
                PtlMDRelease(shmem_transport_portals4_put_volatile_md_h[i]);
            }
        }
        free(shmem_transport_portals4_put_volatile_md_h);
    }
#else
    if (!PtlHandleIsEqual(shmem_transport_portals4_get_md_h[1], PTL_INVALID_HANDLE)) {
        PtlMDRelease(shmem_transport_portals4_get_md_h[1]);
    }
    if (!PtlHandleIsEqual(shmem_transport_portals4_put_event_md_h[1], PTL_INVALID_HANDLE)) {
        PtlMDRelease(shmem_transport_portals4_put_event_md_h[1]);
    }
    if (!PtlHandleIsEqual(shmem_transport_portals4_put_volatile_md_h[0], PTL_INVALID_HANDLE)) {
        PtlMDRelease(shmem_transport_portals4_put_volatile_md_h[0]);
    }
#endif
    if (!PtlHandleIsEqual(shmem_transport_portals4_get_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(shmem_transport_portals4_get_ct_h);
    }
    if (!PtlHandleIsEqual(shmem_transport_portals4_put_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(shmem_transport_portals4_put_ct_h);
    }
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    if (!PtlHandleIsEqual(shmem_transport_portals4_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(shmem_transport_portals4_le_h);
    }
#else
    if (!PtlHandleIsEqual(shmem_transport_portals4_heap_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(shmem_transport_portals4_heap_le_h);
    }
    if (!PtlHandleIsEqual(shmem_transport_portals4_data_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(shmem_transport_portals4_data_le_h);
    }
#endif
    if (!PtlHandleIsEqual(shmem_transport_portals4_target_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(shmem_transport_portals4_target_ct_h);
    }
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    if (PTL_PT_ANY != all_pt) {
        PtlPTFree(shmem_transport_portals4_ni_h, all_pt);
    }
#else
    if (PTL_PT_ANY != heap_pt) {
        PtlPTFree(shmem_transport_portals4_ni_h, heap_pt);
    }
    if (PTL_PT_ANY != data_pt) {
        PtlPTFree(shmem_transport_portals4_ni_h, data_pt);
    }
#endif
    if (!PtlHandleIsEqual(shmem_transport_portals4_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(shmem_transport_portals4_eq_h);
    }
    if (!PtlHandleIsEqual(shmem_transport_portals4_ni_h, PTL_INVALID_HANDLE)) {
        PtlNIFini(shmem_transport_portals4_ni_h);
    }
    if (NULL != shmem_transport_portals4_bounce_buffers) {
        shmem_free_list_destroy(shmem_transport_portals4_bounce_buffers);
    }
    if (NULL != shmem_transport_portals4_long_frags) {
        shmem_free_list_destroy(shmem_transport_portals4_long_frags);
    }
}


int
shmem_transport_portals4_init(long eager_size)
{
    ptl_process_t my_id;
    int ret;
    ptl_ni_limits_t ni_req_limits;

    /* Initialize Portals */
    ret = PtlInit();
    if (PTL_OK != ret) {
        fprintf(stderr, "ERROR: PtlInit failed: %d\n", ret);
        return 1;
    }

    shmem_transport_portals4_bounce_buffer_size = eager_size;
    shmem_transport_portals4_bounce_buffers = 
        shmem_free_list_init(sizeof(shmem_transport_portals4_bounce_buffer_t) + eager_size,
                             init_bounce_buffer);

    shmem_transport_portals4_long_frags =
        shmem_free_list_init(sizeof(shmem_transport_portals4_long_frag_t),
                             init_long_frag);

    /* Initialize network */
    ni_req_limits.max_entries = 1024;
    ni_req_limits.max_unexpected_headers = 1024;
    ni_req_limits.max_mds = 1024;
    ni_req_limits.max_eqs = 1024;
    ni_req_limits.max_cts = 1024;
    ni_req_limits.max_pt_index = 64;
    ni_req_limits.max_iovecs = 1024;
    ni_req_limits.max_list_size = 1024;
    ni_req_limits.max_triggered_ops = 1024;
    ni_req_limits.max_msg_size = LONG_MAX;
    ni_req_limits.max_atomic_size = LONG_MAX;
    ni_req_limits.max_fetch_atomic_size = LONG_MAX;
    ni_req_limits.max_waw_ordered_size = LONG_MAX;
    ni_req_limits.max_war_ordered_size = LONG_MAX;
    ni_req_limits.max_volatile_size = LONG_MAX;
    ni_req_limits.features = 0;
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    ni_req_limits.features |= PTL_TARGET_BIND_INACCESSIBLE;
#endif

    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_NO_MATCHING | PTL_NI_LOGICAL,
                    PTL_PID_ANY,
                    &ni_req_limits,
                    &ni_limits,
                    &shmem_transport_portals4_ni_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlNIInit failed: %d\n",
                shmem_internal_my_pe, ret);
        return ret;
    }

#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    if ((PTL_TARGET_BIND_INACCESSIBLE & ni_limits.features) == 0) {
        fprintf(stderr,
                "[%03d] ERROR: Remote virtual addressing feature enabled, but Portals\n"
                "doesn't support PTL_TARGET_BIND_INACCESSIBLE.  Aborting.\n",
                shmem_internal_my_pe);
    }
#endif

    ret = PtlGetPhysId(shmem_transport_portals4_ni_h, &my_id);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlGetPhysId failed: %d\n",
                shmem_internal_my_pe, ret);
        return ret;
    }

    /* Share information */
    ret = shmem_runtime_put("portals4-procid", &my_id, sizeof(my_id));
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: runtime_put failed: %d\n",
                shmem_internal_my_pe, ret);
        return ret;
    }

    return 0;
}


int
shmem_transport_portals4_startup(void)
{
    int ret, i;
    ptl_process_t *desired = NULL;
    ptl_md_t md;
    ptl_le_t le;
    ptl_uid_t uid = PTL_UID_ANY;
    ptl_process_t my_id;
#ifdef USE_ON_NODE_COMMS
    int num_on_node = 0;
#endif

    desired = malloc(sizeof(ptl_process_t) * shmem_internal_num_pes);
    if (NULL == desired) {
        ret = 1;
        goto cleanup;
    }

    ret = PtlGetPhysId(shmem_transport_portals4_ni_h, &my_id);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlGetPhysId failed: %d\n",
                shmem_internal_my_pe, ret);
        return ret;
    }

    for (i = 0 ; i < shmem_internal_num_pes; ++i) {
        ret = shmem_runtime_get(i, "portals4-procid",
                                &desired[i], sizeof(ptl_process_t));
        if (0 != ret) {
            fprintf(stderr, "[%03d] ERROR: runtime_get failed: %d\n",
                    shmem_internal_my_pe, ret);
            goto cleanup;
        }

#ifdef USE_ON_NODE_COMMS
        /* update the connectivity map... */
        if (desired[i].phys.nid == my_id.phys.nid) {
            SHMEM_SET_RANK_SAME_NODE(i, num_on_node++);
            if (num_on_node > 255) {
                fprintf(stderr, "[%03d] ERROR: Too many local ranks.\n",
                        shmem_internal_my_pe);
                goto cleanup;
            }
        }
#endif
    }

    ret = PtlSetMap(shmem_transport_portals4_ni_h,
                    shmem_internal_num_pes,                    
                    desired);
    if (PTL_OK != ret && PTL_IGNORED != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlSetMap failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    ret = PtlGetUid(shmem_transport_portals4_ni_h, &uid);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlGetUid failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    shmem_transport_portals4_max_volatile_size = ni_limits.max_volatile_size;
    shmem_transport_portals4_max_atomic_size = ni_limits.max_atomic_size;
    shmem_transport_portals4_max_fetch_atomic_size = ni_limits.max_fetch_atomic_size;

    if (shmem_transport_portals4_max_volatile_size < sizeof(long double complex)) {
        fprintf(stderr, "[%03d] ERROR: Max volatile size found to be %lu, too small to continue\n",
                shmem_internal_my_pe, (unsigned long) shmem_transport_portals4_max_volatile_size);
        goto cleanup;
    }
    if (shmem_transport_portals4_max_atomic_size < sizeof(long double complex)) {
        fprintf(stderr, "[%03d] ERROR: Max atomic size found to be %lu, too small to continue\n",
                shmem_internal_my_pe, (unsigned long) shmem_transport_portals4_max_atomic_size);
        goto cleanup;
    }
    if (shmem_transport_portals4_max_fetch_atomic_size < sizeof(long double complex)) {
        fprintf(stderr, "[%03d] ERROR: Max fetch atomic size found to be %lu, too small to continue\n",
                shmem_internal_my_pe, (unsigned long) shmem_transport_portals4_max_fetch_atomic_size);
        goto cleanup;
    }

    /* create portal table entries */
    ret = PtlEQAlloc(shmem_transport_portals4_ni_h, 
                     shmem_transport_portals4_event_slots,
                     &shmem_transport_portals4_eq_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlEQAlloc failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    ret = PtlPTAlloc(shmem_transport_portals4_ni_h,
                     0,
                     shmem_transport_portals4_eq_h,
                     shmem_transport_portals4_pt,
                     &all_pt);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlPTAlloc of table entry failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
#else
    ret = PtlPTAlloc(shmem_transport_portals4_ni_h,
                     0,
                     shmem_transport_portals4_eq_h,
                     shmem_transport_portals4_data_pt,
                     &data_pt);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlPTAlloc of data table failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
    ret = PtlPTAlloc(shmem_transport_portals4_ni_h,
                     0,
                     shmem_transport_portals4_eq_h,
                     shmem_transport_portals4_heap_pt,
                     &heap_pt);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlPTAlloc of heap table failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
#endif

    /* target ct */
    ret = PtlCTAlloc(shmem_transport_portals4_ni_h, &shmem_transport_portals4_target_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of target ct failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    le.start = NULL;
    le.length = PTL_SIZE_MAX;
    le.ct_handle = shmem_transport_portals4_target_ct_h;
    le.uid = uid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_LINK_DISABLE |
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(shmem_transport_portals4_ni_h,
                      shmem_transport_portals4_pt,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &shmem_transport_portals4_le_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlLEAppend of all memory failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
#else
    /* Open LE to heap section */
    le.start = shmem_internal_heap_base;
    le.length = shmem_internal_heap_length;
    le.ct_handle = shmem_transport_portals4_target_ct_h;
    le.uid = uid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_LINK_DISABLE |
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(shmem_transport_portals4_ni_h,
                      shmem_transport_portals4_heap_pt,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &shmem_transport_portals4_heap_le_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlLEAppend of heap section failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* Open LE to data section */
    le.start = shmem_internal_data_base;
    le.length = shmem_internal_data_length;
    le.ct_handle = shmem_transport_portals4_target_ct_h;
    le.uid = uid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_LINK_DISABLE |
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(shmem_transport_portals4_ni_h,
                      shmem_transport_portals4_data_pt,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &shmem_transport_portals4_data_le_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlLEAppend of data section failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
#endif

    /* Open MD to all memory */
    ret = PtlCTAlloc(shmem_transport_portals4_ni_h, &shmem_transport_portals4_put_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of put ct failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
    ret = PtlCTAlloc(shmem_transport_portals4_ni_h, &shmem_transport_portals4_get_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of get ct failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

#if PORTALS4_MAX_MD_SIZE < PORTALS4_MAX_VA_SIZE
    shmem_transport_portals4_put_volatile_md_h = 
        malloc(sizeof(ptl_handle_md_t) * shmem_transport_portals4_get_num_mds());
    if (NULL == shmem_transport_portals4_put_volatile_md_h) {
        fprintf(stderr, "[%03d] ERROR: malloc() of MD array failed\n",
                shmem_internal_my_pe);
        ret = -1;
        goto cleanup;
    }
    for (i = 0 ; i < shmem_transport_portals4_get_num_mds() ; ++i) {
        shmem_transport_portals4_put_volatile_md_h[i] = PTL_INVALID_HANDLE;
    }

    shmem_transport_portals4_put_event_md_h = 
        malloc(sizeof(ptl_handle_md_t) * shmem_transport_portals4_get_num_mds());
    if (NULL == shmem_transport_portals4_put_event_md_h) {
        fprintf(stderr, "[%03d] ERROR: malloc() of MD array failed\n",
                shmem_internal_my_pe);
        ret = -1;
        goto cleanup;
    }
    for (i = 0 ; i < shmem_transport_portals4_get_num_mds() ; ++i) {
        shmem_transport_portals4_put_event_md_h[i] = PTL_INVALID_HANDLE;
    }

    shmem_transport_portals4_get_md_h = 
        malloc(sizeof(ptl_handle_md_t) * shmem_transport_portals4_get_num_mds());
    if (NULL == shmem_transport_portals4_get_md_h) {
        fprintf(stderr, "[%03d] ERROR: malloc() of MD array failed\n",
                shmem_internal_my_pe);
        ret = -1;
        goto cleanup;
    }
    for (i = 0 ; i < shmem_transport_portals4_get_num_mds() ; ++i) {
        shmem_transport_portals4_get_md_h[i] = PTL_INVALID_HANDLE;
    }

    for (i = 0 ; i < shmem_transport_portals4_get_num_mds() ; ++i) {
        ptl_size_t size = (1ULL << PORTALS4_MAX_MD_SIZE) - 1;
        ptl_size_t offset_unit = (1ULL << PORTALS4_MAX_MD_SIZE) / 2;

        md.start = (char*) (offset_unit * i);
        md.length = (i - 1 == shmem_transport_portals4_get_num_mds()) ? size / 2 : size;

        md.options = PTL_MD_EVENT_CT_ACK;
        md.eq_handle = shmem_transport_portals4_eq_h;
        md.ct_handle = shmem_transport_portals4_put_ct_h;
        ret = PtlMDBind(shmem_transport_portals4_ni_h,
                    &md,
                    &shmem_transport_portals4_put_event_md_h[i]);
        if (PTL_OK != ret) {
            fprintf(stderr, "[%03d] ERROR: PtlMDBind of put MD failed: %d\n",
                    shmem_internal_my_pe, ret);
            goto cleanup;
        }

        md.options = PTL_MD_EVENT_CT_ACK |
            PTL_MD_EVENT_SUCCESS_DISABLE |
            PTL_MD_VOLATILE;
        md.eq_handle = shmem_transport_portals4_eq_h;
        md.ct_handle = shmem_transport_portals4_put_ct_h;
        ret = PtlMDBind(shmem_transport_portals4_ni_h,
                        &md,
                        &shmem_transport_portals4_put_volatile_md_h[i]);
        if (PTL_OK != ret) {
            fprintf(stderr, "[%03d] ERROR: PtlMDBind of put MD failed: %d\n",
                    shmem_internal_my_pe, ret);
            goto cleanup;
        }

        md.options = PTL_MD_EVENT_CT_REPLY | 
            PTL_MD_EVENT_SUCCESS_DISABLE;
        md.eq_handle = shmem_transport_portals4_eq_h;
        md.ct_handle = shmem_transport_portals4_get_ct_h;
        ret = PtlMDBind(shmem_transport_portals4_ni_h,
                        &md,
                        &shmem_transport_portals4_get_md_h[i]);
        if (PTL_OK != ret) {
            fprintf(stderr, "[%03d] ERROR: PtlMDBind of get MD failed: %d\n",
                    shmem_internal_my_pe, ret);
            goto cleanup;
        }
    }
#else
    md.start = 0;
    md.length = PTL_SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_ACK;
    md.eq_handle = shmem_transport_portals4_eq_h;
    md.ct_handle = shmem_transport_portals4_put_ct_h;
    ret = PtlMDBind(shmem_transport_portals4_ni_h,
                    &md,
                    &shmem_transport_portals4_put_event_md_h[0]);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of put MD failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    md.start = 0;
    md.length = PTL_SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_ACK |
        PTL_MD_EVENT_SUCCESS_DISABLE |
        PTL_MD_VOLATILE;
    md.eq_handle = shmem_transport_portals4_eq_h;
    md.ct_handle = shmem_transport_portals4_put_ct_h;
    ret = PtlMDBind(shmem_transport_portals4_ni_h,
                    &md,
                    &shmem_transport_portals4_put_volatile_md_h[0]);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of put MD failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    md.start = 0;
    md.length = PTL_SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_REPLY | 
        PTL_MD_EVENT_SUCCESS_DISABLE;
    md.eq_handle = shmem_transport_portals4_eq_h;
    md.ct_handle = shmem_transport_portals4_get_ct_h;
    ret = PtlMDBind(shmem_transport_portals4_ni_h,
                    &md,
                    &shmem_transport_portals4_get_md_h[0]);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of get MD failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
#endif

    ret = 0;

 cleanup:
    if (NULL != desired) free(desired);
    return ret;
}


int
shmem_transport_portals4_fini(void)
{
    ptl_ct_event_t ct;

    /* wait for remote completion (acks) of all pending events */
    PtlCTWait(shmem_transport_portals4_put_ct_h, 
              shmem_transport_portals4_pending_put_counter, &ct);

    cleanup_handles();
    PtlFini();

    return 0;
}

