/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include "shmem.h"
#include "shmem_internal.h"

#include <portals4.h>
#include <portals4_runtime.h>
#include <stdlib.h>
#include <unistd.h>

ptl_handle_ni_t ni_h;
ptl_pt_index_t pt_entry = PTL_PT_ANY;
ptl_handle_md_t md_h;
ptl_handle_le_t le_h;
ptl_handle_ct_t recv_ct_h;
ptl_handle_ct_t send_ct_h;
ptl_handle_eq_t err_eq_h;

void
start_pes(int npes)
{
    int ret;
    ptl_process_t *mapping;
    ptl_md_t md;
    ptl_le_t le;
    ptl_uid_t uid;

    /* Fix me: PTL_INVALID_HANDLE isn't constant in the current
       implementation.  Needs to be, but work around for now */
    ni_h = PTL_INVALID_HANDLE;
    md_h = PTL_INVALID_HANDLE;
    le_h = PTL_INVALID_HANDLE;
    recv_ct_h = PTL_INVALID_HANDLE;
    send_ct_h = PTL_INVALID_HANDLE;
    err_eq_h = PTL_INVALID_HANDLE;
    
    runtime_init();

    /* Initialize Portals */
    mapping  = malloc(sizeof(ptl_process_t) * runtime_get_size());
    ret = PtlInit();
    if (PTL_OK != ret) goto cleanup;
    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_NO_MATCHING | PTL_NI_LOGICAL,
                    PTL_PID_ANY,
                    NULL,
                    NULL,
                    runtime_get_size(),
                    NULL,
                    mapping,
                    &ni_h);
    if (PTL_OK != ret) goto cleanup;

    PtlGetUid(ni_h, &uid);

    /* create symmetric allocation */
    ret = symmetric_init();
    if (0 != ret) goto cleanup;

    /* create portal table entry */
    ret = PtlEQAlloc(ni_h, 64, &err_eq_h);
    if (PTL_OK != ret) goto cleanup;
    ret = PtlPTAlloc(ni_h,
                     0,
                     err_eq_h,
                     SHMEM_IDX,
                     &pt_entry);
    if (PTL_OK != ret) goto cleanup;

    /* Open LE to all memory */
    ret = PtlCTAlloc(ni_h, &recv_ct_h);
    if (PTL_OK != ret) goto cleanup;

    le.start = 0;
    le.length = SIZE_MAX;
    le.ct_handle = recv_ct_h;
    le.ac_id.uid = uid;
    le.options = PTL_LE_OP_PUT | PTL_LE_OP_GET | 
        PTL_LE_EVENT_SUCCESS_DISABLE | 
        PTL_LE_EVENT_CT_COMM;
    ret = PtlLEAppend(ni_h,
                      pt_entry,
                      &le,
                      PTL_PRIORITY_LIST,
                      NULL,
                      &le_h);
    if (PTL_OK != ret) goto cleanup;

    /* Open MD to all memory */
    ret = PtlCTAlloc(ni_h, &send_ct_h);
    if (PTL_OK != ret) goto cleanup;

    md.start = 0;
    md.length = SIZE_MAX;
    md.options = PTL_MD_EVENT_SUCCESS_DISABLE |
        PTL_MD_EVENT_CT_SEND | PTL_MD_EVENT_CT_REPLY |
        PTL_MD_REMOTE_FAILURE_DISABLE;
    md.eq_handle = err_eq_h;
    md.ct_handle = send_ct_h;
    ret = PtlMDBind(ni_h,
                    &md,
                    &md_h);
    if (PTL_OK != ret) goto cleanup;

    /* finish up */
    runtime_barrier();
    return;

 cleanup:
    if (!PtlHandleIsEqual(md_h, PTL_INVALID_HANDLE)) {
        PtlMDRelease(md_h);
    }
    if (!PtlHandleIsEqual(send_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(send_ct_h);
    }
    if (!PtlHandleIsEqual(le_h, PTL_INVALID_HANDLE)) {
        PtlLEUnlink(le_h);
    }
    if (!PtlHandleIsEqual(recv_ct_h, PTL_INVALID_HANDLE)) {
        PtlCTFree(recv_ct_h);
    }
    if (PTL_PT_ANY != pt_entry) {
        PtlPTFree(ni_h, pt_entry);
    }
    if (PtlHandleIsEqual(err_eq_h, PTL_INVALID_HANDLE)) {
        PtlEQFree(err_eq_h);
    }
    if (PtlHandleIsEqual(ni_h, PTL_INVALID_HANDLE)) {
        PtlNIFini(ni_h);
    }
    PtlFini();
    /* BWB: FIX ME: should probably be a bit more subtle here */
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
