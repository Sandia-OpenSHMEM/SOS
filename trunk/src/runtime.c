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

#include "mpp/shmem.h"
#include "shmem_internal.h"
#include "runtime.h"

ptl_handle_ni_t shmem_internal_ni_h = PTL_INVALID_HANDLE;
ptl_pt_index_t shmem_internal_data_pt = PTL_PT_ANY;
ptl_pt_index_t shmem_internal_heap_pt = PTL_PT_ANY;
ptl_handle_md_t shmem_internal_put_md_h = PTL_INVALID_HANDLE;
ptl_handle_md_t shmem_internal_get_md_h = PTL_INVALID_HANDLE;
ptl_handle_le_t shmem_internal_data_le_h = PTL_INVALID_HANDLE;
ptl_handle_le_t shmem_internal_heap_le_h = PTL_INVALID_HANDLE;
ptl_handle_ct_t shmem_internal_target_ct_h = PTL_INVALID_HANDLE;
ptl_handle_ct_t shmem_internal_put_ct_h = PTL_INVALID_HANDLE;
ptl_handle_ct_t shmem_internal_get_ct_h = PTL_INVALID_HANDLE;
#ifdef ENABLE_EVENT_COMPLETION
ptl_handle_eq_t shmem_internal_put_eq_h = PTL_INVALID_HANDLE;
#endif
ptl_handle_eq_t shmem_internal_err_eq_h = PTL_INVALID_HANDLE;
ptl_size_t shmem_internal_max_put_size = 0;
ptl_size_t shmem_internal_max_atomic_size = 0;
ptl_size_t shmem_internal_max_fetch_atomic_size = 0;
ptl_size_t shmem_internal_pending_put_counter = 0;
ptl_size_t shmem_internal_pending_get_counter = 0;

void *shmem_internal_data_base = NULL;
long shmem_internal_data_length = 0;

int shmem_internal_my_pe = -1;
int shmem_internal_num_pes = -1;
int shmem_internal_initialized = 0;
int shmem_internal_finalized = 0;
int shmem_internal_total_data_ordering = 0;

#ifdef MAXHOSTNAMELEN
static char shmem_internal_my_hostname[MAXHOSTNAMELEN];
#else
static char shmem_internal_my_hostname[HOST_NAME_MAX];
#endif

#ifdef __APPLE__
#include <mach-o/getsect.h>
#else
extern char data_start;
extern char end;
#endif


static void
cleanup_handles(void)
{
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_get_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(shmem_internal_get_md_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_put_md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(shmem_internal_put_md_h);
    }
#ifdef ENABLE_EVENT_COMPLETION
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_put_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(shmem_internal_put_eq_h);
    }
#endif
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_get_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(shmem_internal_get_ct_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_put_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(shmem_internal_put_ct_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_heap_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(shmem_internal_heap_le_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_data_le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(shmem_internal_data_le_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_target_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(shmem_internal_target_ct_h);
    }
    if (PTL_PT_ANY != shmem_internal_heap_pt) {
        PtlPTFree(shmem_internal_ni_h, shmem_internal_heap_pt);
    }
    if (PTL_PT_ANY != shmem_internal_data_pt) {
        PtlPTFree(shmem_internal_ni_h, shmem_internal_data_pt);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_err_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(shmem_internal_err_eq_h);
    }
    if (PTL_OK != PtlHandleIsEqual(shmem_internal_ni_h, PTL_INVALID_HANDLE)) {
        PtlNIFini(shmem_internal_ni_h);
    }
}


static void
shmem_internal_shutdown(void)
{
    ptl_ct_event_t ct;

    if (!shmem_internal_initialized ||
        shmem_internal_finalized) {
        return;
    }
    shmem_internal_finalized = 1;

    shmem_internal_runtime_barrier();

    /* wait for remote completion (acks) of all pending events */
    PtlCTWait(shmem_internal_put_ct_h, 
              shmem_internal_pending_put_counter, &ct);

    cleanup_handles();
    shmem_internal_symmetric_fini();
    shmem_internal_runtime_fini();
    PtlFini();
}


void
start_pes(int npes)
{
    int ret;
    char *s;
    ptl_process_t *desired = NULL;
    ptl_md_t md;
    ptl_le_t le;
    ptl_uid_t uid = PTL_UID_ANY;
    ptl_ni_limits_t ni_limits, ni_req_limits;
    int radix = -1, crossover = -1;

    if (shmem_internal_initialized) {
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
    shmem_internal_my_pe = shmem_internal_runtime_get_rank();
    shmem_internal_num_pes = shmem_internal_runtime_get_size();

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
                    &shmem_internal_ni_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlNIInit failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    desired = shmem_internal_runtime_get_mapping(shmem_internal_ni_h);
    if (NULL == desired) {
        fprintf(stderr, "[%03d] ERROR: runtime mapping failed.\n", 
                shmem_internal_my_pe);
        goto cleanup;
    }

    ret = PtlSetMap(shmem_internal_ni_h,
                    shmem_internal_num_pes,                    
                    desired);
    if (PTL_OK != ret && PTL_IGNORED != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlSetMap failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    ret = PtlGetUid(shmem_internal_ni_h, &uid);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlGetUid failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* Check message size limits to make sure rational */
    shmem_internal_max_put_size = (ni_limits.max_waw_ordered_size > ni_limits.max_volatile_size) ?  
        ni_limits.max_waw_ordered_size : ni_limits.max_volatile_size;
    shmem_internal_max_atomic_size = (ni_limits.max_waw_ordered_size > ni_limits.max_atomic_size) ?
        ni_limits.max_waw_ordered_size : ni_limits.max_atomic_size;
    shmem_internal_max_fetch_atomic_size = (ni_limits.max_waw_ordered_size > ni_limits.max_atomic_size) ?
        ni_limits.max_waw_ordered_size : ni_limits.max_fetch_atomic_size;
    if (shmem_internal_max_put_size < sizeof(long double complex)) {
        fprintf(stderr, "[%03d] ERROR: Max put size found to be %lu, too small to continue\n",
                shmem_internal_my_pe, (unsigned long) shmem_internal_max_put_size);
        goto cleanup;
    }
    if (shmem_internal_max_atomic_size < sizeof(long double complex)) {
        fprintf(stderr, "[%03d] ERROR: Max atomic size found to be %lu, too small to continue\n",
                shmem_internal_my_pe, (unsigned long) shmem_internal_max_put_size);
        goto cleanup;
    }
    if (shmem_internal_max_fetch_atomic_size < sizeof(long double complex)) {
        fprintf(stderr, "[%03d] ERROR: Max fetch atomic size found to be %lu, too small to continue\n",
                shmem_internal_my_pe, (unsigned long) shmem_internal_max_put_size);
        goto cleanup;
    }
#ifdef PTL_TOTAL_DATA_ORDERING
    if ((PTL_TOTAL_DATA_ORDERING & ni_limits.features) != 0) {
        shmem_internal_total_data_ordering = 1;        
    }
#endif

    /* create symmetric allocation */
    ret = shmem_internal_symmetric_init();
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: symmetric heap initialization failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* create portal table entry */
    ret = PtlEQAlloc(shmem_internal_ni_h, 64, &shmem_internal_err_eq_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlEQAlloc failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
    ret = PtlPTAlloc(shmem_internal_ni_h,
                     0,
                     shmem_internal_err_eq_h,
                     DATA_IDX,
                     &shmem_internal_data_pt);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlPTAlloc of data table failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
    ret = PtlPTAlloc(shmem_internal_ni_h,
                     0,
                     shmem_internal_err_eq_h,
                     HEAP_IDX,
                     &shmem_internal_heap_pt);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlPTAlloc of heap table failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* target ct */
    ret = PtlCTAlloc(shmem_internal_ni_h, &shmem_internal_target_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of target ct failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* Open LE to heap section */
    le.start = shmem_internal_heap_base;
    le.length = shmem_internal_heap_length;
    le.ct_handle = shmem_internal_target_ct_h;
    le.uid = uid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(shmem_internal_ni_h,
                      shmem_internal_heap_pt,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &shmem_internal_heap_le_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlLEAppend of heap section failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* Open LE to data section */
#ifdef __APPLE__
    le.start = shmem_internal_data_base = (void*) get_etext();
    le.length = shmem_internal_data_length = get_end() - get_etext();
#else
    le.start = shmem_internal_data_base = &data_start;
    le.length = shmem_internal_data_length = (unsigned long) &end  - (unsigned long) &data_start;
#endif

    le.ct_handle = shmem_internal_target_ct_h;
    le.uid = uid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(shmem_internal_ni_h,
                      shmem_internal_data_pt,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &shmem_internal_data_le_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlLEAppend of data section failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* Open MD to all memory */
    ret = PtlCTAlloc(shmem_internal_ni_h, &shmem_internal_put_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of put ct failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
    ret = PtlCTAlloc(shmem_internal_ni_h, &shmem_internal_get_ct_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlCTAlloc of get ct failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }
#ifdef ENABLE_EVENT_COMPLETION
    ret = PtlEQAlloc(shmem_internal_ni_h, 64, &shmem_internal_put_eq_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlEQAlloc of put eq failed: %d\n",
                shmem_internal_my_pe, ret);
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
    md.eq_handle = shmem_internal_put_eq_h;
#else
    md.eq_handle = shmem_internal_err_eq_h;
#endif
    md.ct_handle = shmem_internal_put_ct_h;
    ret = PtlMDBind(shmem_internal_ni_h,
                    &md,
                    &shmem_internal_put_md_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of put MD failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    md.start = 0;
    md.length = SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_REPLY | 
        PTL_MD_EVENT_SUCCESS_DISABLE;
    md.eq_handle = shmem_internal_err_eq_h;
    md.ct_handle = shmem_internal_get_ct_h;
    ret = PtlMDBind(shmem_internal_ni_h,
                    &md,
                    &shmem_internal_get_md_h);
    if (PTL_OK != ret) {
        fprintf(stderr, "[%03d] ERROR: PtlMDBind of get MD failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* setup collectives */
    s = getenv("SHMEM_TREE_RADIX");
    if ( s != NULL && *s != '\0' ) {
        radix = atoi(s);
        if (radix < 2) radix = 2;
    }

    s = getenv("SHMEM_TREE_THRESHOLD");
    if ( s != NULL && *s != '\0' ) {
        crossover = atoi(s);
        if (crossover < 4) crossover = 4;
    }

    ret = shmem_internal_collectives_init(crossover, radix);
    if (ret != 0) {
        fprintf(stderr,
                "[%03d] ERROR: initialization of collectives failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    atexit(shmem_internal_shutdown);
    shmem_internal_initialized = 1;

    if (gethostname(shmem_internal_my_hostname,
                    sizeof(shmem_internal_my_hostname))) {
        sprintf(shmem_internal_my_hostname, "ERR: gethostname '%s'?",
                strerror(errno));
    }

    /* finish up */
    shmem_internal_runtime_barrier();
    return;

 cleanup:
    if (desired)
        free(desired);
    cleanup_handles();
    if (NULL != shmem_internal_data_base) {
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
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_my_pe;
}


int
_my_pe(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_my_pe;
}


int
my_pe(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_my_pe;
}


int
shmem_n_pes(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_num_pes;
}


int
_num_pes(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_num_pes;
}


int
num_pes(void)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return shmem_internal_num_pes;
}


double
shmem_wtime(void)
{
    double wtime;
    struct timeval tv;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
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
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    return (pe >= 0 && pe < shmem_internal_num_pes) ? 1 : 0;
}


int
shmem_addr_accessible(void *addr, int pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    if ((char*) addr > (char*) shmem_internal_heap_base &&
        (char*) addr < (char*) shmem_internal_heap_base + shmem_internal_heap_length) {
        return 1;
    }
    if ((char*) addr > (char*) shmem_internal_data_base &&
        (char*) addr < (char*) shmem_internal_data_base + shmem_internal_data_length) {
        return 1;
    }

    return 0;
}


void *
shmem_ptr (void *target, int pe)
{
    return (void*) 0;
    // Only if regular load/stores are used to implement put/get:
    //return (shmem_accessible( target, pe ) == 1 ? target : (void*)0);
}

char *
shmem_nodename(void)
{
    return shmem_internal_my_hostname;
}
