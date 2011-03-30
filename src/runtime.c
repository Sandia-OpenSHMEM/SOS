/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include <portals4.h>
#include <portals4_runtime.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"

ptl_handle_ni_t ni_h;
ptl_pt_index_t data_pt = PTL_PT_ANY;
ptl_pt_index_t heap_pt = PTL_PT_ANY;
ptl_handle_md_t put_md_h;
ptl_handle_md_t get_md_h;
ptl_handle_le_t data_le_h;
ptl_handle_le_t heap_le_h;
ptl_handle_ct_t target_ct_h;
ptl_handle_ct_t put_ct_h;
ptl_handle_ct_t get_ct_h;
#ifdef ENABLE_EVENT_COMPLETION
ptl_handle_eq_t put_eq_h;
#endif
ptl_handle_eq_t err_eq_h;
ptl_size_t max_ordered_size = 0;
ptl_size_t pending_put_counter = 0;
ptl_size_t pending_get_counter = 0;

void *shmem_data_base = NULL;
long shmem_data_length = 0;


#ifdef __APPLE__
#include <mach-o/getsect.h>
#else
extern char etext;
extern char end;
#endif

void
start_pes(int npes)
{
    int ret;
    ptl_process_t *mapping;
    ptl_md_t md;
    ptl_le_t le;
    ptl_jid_t jid = PTL_JID_ANY;
    ptl_ni_limits_t ni_limits;

    /* Fix me: PTL_INVALID_HANDLE isn't constant in the current
       implementation.  Needs to be, but work around for now */
    ni_h = PTL_INVALID_HANDLE;
    put_md_h = PTL_INVALID_HANDLE;
    get_md_h = PTL_INVALID_HANDLE;
    data_le_h = PTL_INVALID_HANDLE;
    heap_le_h = PTL_INVALID_HANDLE;
    target_ct_h = PTL_INVALID_HANDLE;
    put_ct_h = PTL_INVALID_HANDLE;
    get_ct_h = PTL_INVALID_HANDLE;
#ifdef ENABLE_EVENT_COMPLETION
    put_eq_h = PTL_INVALID_HANDLE;
#endif
    err_eq_h = PTL_INVALID_HANDLE;
    
    /* Initialize Portals */
    ret = PtlInit();
    if (PTL_OK != ret) goto cleanup;
    mapping  = malloc(sizeof(ptl_process_t) * runtime_get_size());
    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_NO_MATCHING | PTL_NI_LOGICAL,
                    PTL_PID_ANY,
                    NULL,
                    &ni_limits,
                    runtime_get_size(),
                    NULL,
                    mapping,
                    &ni_h);
    if (PTL_OK != ret) goto cleanup;
    max_ordered_size = ni_limits.max_ordered_size;

#if 0
    PtlGetJid(ni_h, &jid);
#endif

    /* create symmetric allocation */
    ret = symmetric_init();
    if (0 != ret) goto cleanup;

    /* create portal table entry */
    ret = PtlEQAlloc(ni_h, 64, &err_eq_h);
    if (PTL_OK != ret) goto cleanup;
    ret = PtlPTAlloc(ni_h,
                     0,
                     err_eq_h,
                     DATA_IDX,
                     &data_pt);
    if (PTL_OK != ret) goto cleanup;
    ret = PtlPTAlloc(ni_h,
                     0,
                     err_eq_h,
                     HEAP_IDX,
                     &heap_pt);
    if (PTL_OK != ret) goto cleanup;

    /* target ct */
    ret = PtlCTAlloc(ni_h, &target_ct_h);
    if (PTL_OK != ret) goto cleanup;

    /* Open LE to data section */
#ifdef __APPLE__
    le.start = shmem_data_base = (void*) get_etext();
    le.length = shmem_data_length = get_end() - get_etext();
#else
    le.start = shmem_data_base = &etext;
    le.length = shmem_data_length = (unsigned long) &end  - (unsigned long) &etext;
#endif
    le.ct_handle = target_ct_h;
    le.ac_id.jid = jid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(ni_h,
                      data_pt,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &data_le_h);
    if (PTL_OK != ret) goto cleanup;

    /* Open LE to heap section */
    le.start = shmem_heap_base;
    le.length = shmem_heap_length;
    le.ct_handle = target_ct_h;
    le.ac_id.jid = jid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(ni_h,
                      heap_pt,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &heap_le_h);
    if (PTL_OK != ret) goto cleanup;

    /* Open MD to all memory */
    ret = PtlCTAlloc(ni_h, &put_ct_h);
    if (PTL_OK != ret) goto cleanup;
    ret = PtlCTAlloc(ni_h, &get_ct_h);
    if (PTL_OK != ret) goto cleanup;
#ifdef ENABLE_EVENT_COMPLETION
    ret = PtlEQAlloc(ni_h, 64, &put_eq_h);
    if (PTL_OK != ret) goto cleanup;
#endif

    md.start = 0;
    md.length = SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_ACK | 
#if ! defined(ENABLE_EVENT_COMPLETION)
        PTL_MD_EVENT_SUCCESS_DISABLE |
#endif
        PTL_MD_REMOTE_FAILURE_DISABLE;
#ifdef ENABLE_EVENT_COMPLETION
    md.eq_handle = put_eq_h;
#else
    md.eq_handle = err_eq_h;
#endif
    md.ct_handle = put_ct_h;
    ret = PtlMDBind(ni_h,
                    &md,
                    &put_md_h);
    if (PTL_OK != ret) goto cleanup;

    md.start = 0;
    md.length = SIZE_MAX;
    md.options = PTL_MD_EVENT_CT_REPLY | 
        PTL_MD_REMOTE_FAILURE_DISABLE |
        PTL_MD_EVENT_SUCCESS_DISABLE;
    md.eq_handle = err_eq_h;
    md.ct_handle = get_ct_h;
    ret = PtlMDBind(ni_h,
                    &md,
                    &get_md_h);
    if (PTL_OK != ret) goto cleanup;

    /* finish up */
    runtime_barrier();
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
    PtlFini();
    abort();
}


int
shmem_my_pe(void)
{
    return runtime_get_rank();
}


int
_my_pe(void)
{
    return shmem_my_pe();
}


int
shmem_n_pes(void)
{
    return runtime_get_size();
}


int
_num_pes(void)
{
    return shmem_n_pes();
}


double
shmem_wtime(void)
{
    double wtime;
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
    return wtime;
}


int 
shmem_pe_accessible(int pe)
{
    if (pe >= 0 && pe < shmem_n_pes()) {
        return 1;
    }
    return 0;
}


int
shmem_addr_accessible(void *addr, int pe)
{
    if (addr > shmem_heap_base &&
        (char*) addr < (char*) shmem_heap_base + shmem_heap_length) {
        return 1;
    }
    return 0;
}
