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

ptl_handle_ni_t shmem_transport_portals4_ni_h = PTL_INVALID_HANDLE;
ptl_pt_index_t shmem_transport_portals4_data_pt = PTL_PT_ANY;
ptl_pt_index_t shmem_transport_portals4_heap_pt = PTL_PT_ANY;
ptl_handle_md_t shmem_transport_portals4_put_volatile_md_h = PTL_INVALID_HANDLE;
ptl_handle_md_t shmem_transport_portals4_put_event_md_h = PTL_INVALID_HANDLE;
ptl_handle_md_t shmem_transport_portals4_get_md_h = PTL_INVALID_HANDLE;
ptl_handle_le_t shmem_transport_portals4_data_le_h = PTL_INVALID_HANDLE;
ptl_handle_le_t shmem_transport_portals4_heap_le_h = PTL_INVALID_HANDLE;
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

static ptl_ni_limits_t ni_limits;


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
    shmem_transport_portals4_frag_t *frag =
        (shmem_transport_portals4_frag_t*) item;
    frag->type = SHMEM_TRANSPORT_PORTALS4_TYPE_LONG;
}


static void
cleanup_handles(void)
{
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_get_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(shmem_transport_portals4_get_md_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_put_event_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(shmem_transport_portals4_put_event_md_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_put_volatile_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(shmem_transport_portals4_put_volatile_md_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_get_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(shmem_transport_portals4_get_ct_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_put_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(shmem_transport_portals4_put_ct_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_heap_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(shmem_transport_portals4_heap_le_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_data_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(shmem_transport_portals4_data_le_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_target_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(shmem_transport_portals4_target_ct_h);
    }
    if (PTL_PT_ANY != shmem_transport_portals4_heap_pt) {
        PtlPTFree(shmem_transport_portals4_ni_h, shmem_transport_portals4_heap_pt);
    }
    if (PTL_PT_ANY != shmem_transport_portals4_data_pt) {
        PtlPTFree(shmem_transport_portals4_ni_h, shmem_transport_portals4_data_pt);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(shmem_transport_portals4_eq_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_transport_portals4_ni_h, PTL_INVALID_HANDLE)) {
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
    ni_req_limits.features = PTL_TOTAL_DATA_ORDERING;

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
    long waw_size;
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

    /* Check message size limits to make sure rational */
    if ((PTL_TOTAL_DATA_ORDERING & ni_limits.features) != 0) {
        waw_size = ni_limits.max_waw_ordered_size;
    } else {
        /* waw ordering doesn't matter for message size if no total
           ordering.  Therefore, make it big, so it's not the limiter
           in the following tests. */
        waw_size = LONG_MAX;
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

    /* create portal table entry */
    ret = PtlEQAlloc(shmem_transport_portals4_ni_h, 64, &shmem_transport_portals4_eq_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlEQAlloc failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
    ret = PtlPTAlloc(shmem_transport_portals4_ni_h,
                     0,
                     shmem_transport_portals4_eq_h,
                     DATA_IDX,
                     &shmem_transport_portals4_data_pt);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlPTAlloc of data table failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
    ret = PtlPTAlloc(shmem_transport_portals4_ni_h,
                     0,
                     shmem_transport_portals4_eq_h,
                     HEAP_IDX,
                     &shmem_transport_portals4_heap_pt);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlPTAlloc of heap table failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* target ct */
    ret = PtlCTAlloc(shmem_transport_portals4_ni_h, &shmem_transport_portals4_target_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of target ct failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

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

    md.start = 0;
    md.length = SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_ACK;
    md.eq_handle = shmem_transport_portals4_eq_h;
    md.ct_handle = shmem_transport_portals4_put_ct_h;
    ret = PtlMDBind(shmem_transport_portals4_ni_h,
                    &md,
                    &shmem_transport_portals4_put_event_md_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of put MD failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }


    md.start = 0;
    md.length = SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_ACK |
        PTL_MD_EVENT_SUCCESS_DISABLE |
        PTL_MD_VOLATILE;
    md.eq_handle = shmem_transport_portals4_eq_h;
    md.ct_handle = shmem_transport_portals4_put_ct_h;
    ret = PtlMDBind(shmem_transport_portals4_ni_h,
                    &md,
                    &shmem_transport_portals4_put_volatile_md_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of put MD failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    md.start = 0;
    md.length = SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_REPLY | 
        PTL_MD_EVENT_SUCCESS_DISABLE;
    md.eq_handle = shmem_transport_portals4_eq_h;
    md.ct_handle = shmem_transport_portals4_get_ct_h;
    ret = PtlMDBind(shmem_transport_portals4_ni_h,
                    &md,
                    &shmem_transport_portals4_get_md_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of get MD failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

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

