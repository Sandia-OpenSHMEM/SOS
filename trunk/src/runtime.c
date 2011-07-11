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
#include <unistd.h>
#include <sys/time.h>
#include <stdio.h>
#include <limits.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"
#include "runtime.h"

ptl_handle_ni_t ni_h = PTL_INVALID_HANDLE;
ptl_pt_index_t data_pt = PTL_PT_ANY;
ptl_pt_index_t heap_pt = PTL_PT_ANY;
ptl_handle_md_t put_md_h = PTL_INVALID_HANDLE;
ptl_handle_md_t get_md_h = PTL_INVALID_HANDLE;
ptl_handle_le_t data_le_h = PTL_INVALID_HANDLE;
ptl_handle_le_t heap_le_h = PTL_INVALID_HANDLE;
ptl_handle_ct_t target_ct_h = PTL_INVALID_HANDLE;
ptl_handle_ct_t put_ct_h = PTL_INVALID_HANDLE;
ptl_handle_ct_t get_ct_h = PTL_INVALID_HANDLE;
#ifdef ENABLE_EVENT_COMPLETION
ptl_handle_eq_t put_eq_h = PTL_INVALID_HANDLE;
#endif
ptl_handle_eq_t err_eq_h = PTL_INVALID_HANDLE;
ptl_size_t max_put_size = 0;
ptl_size_t max_atomic_size = 0;
ptl_size_t max_fetch_atomic_size = 0;
ptl_size_t pending_put_counter = 0;
ptl_size_t pending_get_counter = 0;

void *shmem_data_base = NULL;
long shmem_data_length = 0;

int shmem_int_my_pe = -1;
int shmem_int_num_pes = -1;
int shmem_int_initialized = 0;
int shmem_int_finalized = 0;
int shmem_internal_total_data_ordering = 0;

#ifdef __APPLE__
#include <mach-o/getsect.h>
#else
extern char data_start;
extern char end;
#endif

static void
shmem_internal_shutdown(void)
{
    if (!shmem_int_initialized ||
        shmem_int_finalized) {
        return;
    }

    shmem_int_finalized = 1;

    if (!PtlHandleIsEqual(get_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(get_md_h);
    }
    if (!PtlHandleIsEqual(put_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(put_md_h);
    }
#ifdef ENABLE_EVENT_COMPLETION
    if (!PtlHandleIsEqual(put_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(put_eq_h);
    }
#endif
    if (!PtlHandleIsEqual(get_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(get_ct_h);
    }
    if (!PtlHandleIsEqual(put_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(put_ct_h);
    }
    if (!PtlHandleIsEqual(heap_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(heap_le_h);
    }
    if (!PtlHandleIsEqual(data_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(data_le_h);
    }
    if (!PtlHandleIsEqual(target_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(target_ct_h);
    }
    if (PTL_PT_ANY != heap_pt) {
        PtlPTFree(ni_h, heap_pt);
    }
    if (PTL_PT_ANY != data_pt) {
        PtlPTFree(ni_h, data_pt);
    }
    if (PtlHandleIsEqual(err_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(err_eq_h);
    }
    if (PtlHandleIsEqual(ni_h, PTL_INVALID_HANDLE)) {
        PtlNIFini(ni_h);
    }
    if (NULL != shmem_data_base) {
        shmem_internal_symmetric_fini();
    }
    shmem_internal_runtime_fini();
    PtlFini();
}

void
start_pes(int npes)
{
    int ret;
    ptl_process_t *desired = NULL, *mapping = NULL;
    ptl_md_t md;
    ptl_le_t le;
    ptl_uid_t uid = PTL_UID_ANY;
    ptl_ni_limits_t ni_limits, ni_req_limits;

    if (shmem_int_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    /* Initialize Portals */
    ret = PtlInit();
    if (PTL_OK != ret) {
        fprintf(stderr, "ERROR: PtlInit failed: %d\n", ret);
        abort();
    }

    ret = shmem_internal_runtime_init();
    if (0 != ret) {
        fprintf(stderr, "ERROR: runtime init failed: %d\n", ret);
        goto cleanup;
    }
    shmem_int_my_pe = shmem_internal_get_rank();
    shmem_int_num_pes = shmem_internal_get_size();

    desired = shmem_internal_get_mapping();
    if (NULL == desired) {
        fprintf(stderr, "[%03d] ERROR: runtime mapping failed.\n", 
                shmem_int_my_pe);
        goto cleanup;
    }

    mapping  = malloc(sizeof(ptl_process_t) * shmem_int_num_pes);
    if (NULL == mapping) {
        fprintf(stderr, "[%03d] ERROR: malloc of mapping failed.\n", 
                shmem_int_my_pe);
        goto cleanup;
    }

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
    ni_req_limits.max_atomic_size = 512;
    ni_req_limits.max_fetch_atomic_size = 512;
    ni_req_limits.max_waw_ordered_size = 512;
    ni_req_limits.max_war_ordered_size = 512;
    ni_req_limits.max_volatile_size = 512;
#ifdef PTL_TOTAL_DATA_ORDERING
    ni_req_limits.features = PTL_TOTAL_DATA_ORDERING;
#else
    ni_req_limits.features = 0;
#endif

    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_NO_MATCHING | PTL_NI_LOGICAL,
                    PTL_PID_ANY,
                    &ni_req_limits,
                    &ni_limits,
                    shmem_int_num_pes,
                    desired,
                    mapping,
                    &ni_h);
    free(desired);
    /* BWB: FIX ME: probably should do something with mapping... */
    free(mapping);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlNIInit failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    ret = PtlGetUid(ni_h, &uid);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlGetUid failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    /* Check message size limits to make sure rational */
    max_put_size = (ni_limits.max_waw_ordered_size > ni_limits.max_volatile_size) ?  
        ni_limits.max_waw_ordered_size : ni_limits.max_volatile_size;
    max_atomic_size = (ni_limits.max_waw_ordered_size > ni_limits.max_atomic_size) ?
        ni_limits.max_waw_ordered_size : ni_limits.max_atomic_size;
    max_fetch_atomic_size = (ni_limits.max_waw_ordered_size > ni_limits.max_atomic_size) ?
        ni_limits.max_waw_ordered_size : ni_limits.max_fetch_atomic_size;
    if (max_put_size < sizeof(long double complex)) {
        fprintf(stderr, "Max put size found to be %lu, too small to continue\n",
                (unsigned long) max_put_size);
        goto cleanup;
    }
    if (max_atomic_size < sizeof(long double complex)) {
        fprintf(stderr, "Max atomic size found to be %lu, too small to continue\n",
                (unsigned long) max_put_size);
        goto cleanup;
    }
    if (max_fetch_atomic_size < sizeof(long double complex)) {
        fprintf(stderr, "Max fetch atomic size found to be %lu, too small to continue\n",
                (unsigned long) max_put_size);
        goto cleanup;
    }
#ifdef PTL_TOTAL_DATA_ORDERING
    if (PTL_TOTAL_DATA_ORDERING & ni_limits.features != 0) {
        shmem_internal_total_data_ordering = 1;        
    }
#endif

    /* create symmetric allocation */
    ret = shmem_internal_symmetric_init();
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: symmetric heap initialization failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    /* create portal table entry */
    ret = PtlEQAlloc(ni_h, 64, &err_eq_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlEQAlloc failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }
    ret = PtlPTAlloc(ni_h,
                     0,
                     err_eq_h,
                     DATA_IDX,
                     &data_pt);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlPTAlloc of data table failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }
    ret = PtlPTAlloc(ni_h,
                     0,
                     err_eq_h,
                     HEAP_IDX,
                     &heap_pt);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlPTAlloc of heap table failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    /* target ct */
    ret = PtlCTAlloc(ni_h, &target_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of target ct failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    /* Open LE to heap section */
    le.start = shmem_heap_base;
    le.length = shmem_heap_length;
    le.ct_handle = target_ct_h;
    le.ac_id.uid = uid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(ni_h,
                      heap_pt,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &heap_le_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlLEAppend of heap section failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    /* Open LE to data section */
#ifdef __APPLE__
    le.start = shmem_data_base = (void*) get_etext();
    le.length = shmem_data_length = get_end() - get_etext() - 1;
#else
    le.start = shmem_data_base = &data_start;
    le.length = shmem_data_length = (unsigned long) &end  - (unsigned long) &data_start - 1;
#endif

    le.ct_handle = target_ct_h;
    le.ac_id.uid = uid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(ni_h,
                      data_pt,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &data_le_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlLEAppend of data section failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    /* Open MD to all memory */
    ret = PtlCTAlloc(ni_h, &put_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of put ct failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }
    ret = PtlCTAlloc(ni_h, &get_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of get ct failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }
#ifdef ENABLE_EVENT_COMPLETION
    ret = PtlEQAlloc(ni_h, 64, &put_eq_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlEQAlloc of put eq failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }
#endif

    md.start = 0;
    md.length = SIZE_MAX;
    md.options = 
#if ! defined(ENABLE_EVENT_COMPLETION)
        PTL_MD_VOLATILE |
        PTL_MD_EVENT_SUCCESS_DISABLE |
#endif
        PTL_MD_EVENT_CT_ACK;
#ifdef ENABLE_EVENT_COMPLETION
    md.eq_handle = put_eq_h;
#else
    md.eq_handle = err_eq_h;
#endif
    md.ct_handle = put_ct_h;
    ret = PtlMDBind(ni_h,
                    &md,
                    &put_md_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of put MD failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    md.start = 0;
    md.length = SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_REPLY | 
        PTL_MD_EVENT_SUCCESS_DISABLE;
    md.eq_handle = err_eq_h;
    md.ct_handle = get_ct_h;
    ret = PtlMDBind(ni_h,
                    &md,
                    &get_md_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of get MD failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    /* setup space for barrier */
    ret = shmem_internal_barrier_init();
    if (ret != 0) {
        fprintf(stderr, "[%03d] ERROR: initialization of barrier space failed: %d\n",
                shmem_int_my_pe, ret);
        goto cleanup;
    }

    shmem_int_initialized = 1;
    atexit(shmem_internal_shutdown);

    /* Give point for debuggers to get a chance to attach if requested by user */
    if (NULL != getenv("SHMEM_DEBUGGER_ATTACH")) {
        printf("[%02d] waiting for attach, pid=%d\n", shmem_int_my_pe, getpid());
        volatile int foobar = 0;
        while (foobar == 0) { }
    }

    /* finish up */
    shmem_internal_barrier();
    return;

 cleanup:
    if (!PtlHandleIsEqual(get_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(get_md_h);
    }
    if (!PtlHandleIsEqual(put_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(put_md_h);
    }
#ifdef ENABLE_EVENT_COMPLETION
    if (!PtlHandleIsEqual(put_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(put_eq_h);
    }
#endif
    if (!PtlHandleIsEqual(get_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(get_ct_h);
    }
    if (!PtlHandleIsEqual(put_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(put_ct_h);
    }
    if (!PtlHandleIsEqual(heap_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(heap_le_h);
    }
    if (!PtlHandleIsEqual(data_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(data_le_h);
    }
    if (!PtlHandleIsEqual(target_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(target_ct_h);
    }
    if (PTL_PT_ANY != heap_pt) {
        PtlPTFree(ni_h, heap_pt);
    }
    if (PTL_PT_ANY != data_pt) {
        PtlPTFree(ni_h, data_pt);
    }
    if (PtlHandleIsEqual(err_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(err_eq_h);
    }
    if (PtlHandleIsEqual(ni_h, PTL_INVALID_HANDLE)) {
        PtlNIFini(ni_h);
    }
    if (NULL != shmem_data_base) {
        shmem_internal_symmetric_fini();
    }
    shmem_internal_runtime_fini();
    PtlFini();
    abort();
}


int
shmem_my_pe(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_int_my_pe;
}


int
_my_pe(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_int_my_pe;
}


int
my_pe(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_int_my_pe;
}


int
shmem_n_pes(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_int_num_pes;
}


int
_num_pes(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_int_num_pes;
}


int
num_pes(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_int_num_pes;
}


double
shmem_wtime(void)
{
    double wtime;
    struct timeval tv;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
    return wtime;
}


int 
shmem_pe_accessible(int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return (pe >= 0 && pe < shmem_int_num_pes) ? 1 : 0;
}


int
shmem_addr_accessible(void *addr, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_int_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    if (addr > shmem_heap_base &&
        (char*) addr < (char*) shmem_heap_base + shmem_heap_length) {
        return 1;
    }
    return 0;
}
