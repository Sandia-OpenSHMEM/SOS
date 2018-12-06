/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

/*
 * Wrappers to interface with PMI runtime
 */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if defined(PMI_PORTALS4)
#include <portals4/pmi.h>
#else
#include <pmix.h>
#endif

#include "runtime.h"
#include "shmem_internal.h"
#include "uthash.h"

static pmix_proc_t myproc;
static size_t size;
static uint32_t local_rank = 0;
static uint32_t local_size = 1;
static int *local_ranks;

int
shmem_runtime_init(void)
{
    pmix_status_t rc;
    pmix_proc_t proc;
    proc.rank = PMIX_RANK_WILDCARD;
    pmix_value_t *val;

    if (PMIX_SUCCESS != (rc = PMIx_Init(&myproc, NULL, 0))) {
        RETURN_ERROR_MSG_PREINIT("PMIx_Init failed (%d)\n", rc);
        return rc;
    }

    strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = PMIX_RANK_WILDCARD;

    if (PMIX_SUCCESS == (rc = PMIx_Get(&proc, PMIX_JOB_SIZE, NULL, 0, &val))) {
        size = val->data.uint32;
        PMIX_VALUE_RELEASE(val);
    } else {
        RETURN_ERROR_MSG_PREINIT("Size is not properly initiated (%d)\n", rc);
        return rc;
    }

    return PMIX_SUCCESS;
}


int
shmem_runtime_fini(void)
{
    pmix_status_t rc;

    free(local_ranks);

    if (PMIX_SUCCESS != (rc = PMIx_Finalize(NULL, 0))) {
        RETURN_ERROR_MSG_PREINIT("PMIx_Finalize failed (%d)\n", rc);
        return rc;
    }

    return PMIX_SUCCESS;
}


void
shmem_runtime_abort(int exit_code, const char msg[])
{

#ifdef HAVE___BUILTIN_TRAP
    if (shmem_internal_params.TRAP_ON_ABORT)
        __builtin_trap();
#endif

    pmix_status_t rc;

    if (PMIX_SUCCESS != (rc = PMIx_Abort(exit_code, msg, NULL, 0))) {
        RETURN_ERROR_STR("PMIx_Abort failed");
    }

    /* PMI_Abort should not return */
    abort();

}



int
shmem_runtime_get_rank(void)
{
    return myproc.rank;
}

int
shmem_runtime_get_size(void)
{
    return size;
}

int
shmem_runtime_get_local_rank(int pe)
{
    shmem_internal_assert(size > 0 && pe < size && pe >= 0);
    return local_ranks[pe];
}

int
shmem_runtime_get_local_size(void)
{
    return local_size;
}

// static void opcbfunc(pmix_status_t status, void *cbdata)
// {
//     bool *active = (bool*)cbdata;

//     fprintf(stderr, "%s:%d completed fence_nb", myproc.nspace, myproc.rank);
//     *active = false;
// }

int
shmem_runtime_exchange(int need_node_util)
{
    pmix_status_t rc;
    pmix_info_t info;
    bool wantit=true;
    //bool active = true;

    if (need_node_util) {
        pmix_proc_t proc;
        pmix_value_t *val;

        strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
        proc.rank = PMIX_RANK_WILDCARD;

        if (PMIX_SUCCESS == (rc = PMIx_Get(&proc, PMIX_LOCAL_SIZE, NULL, 0, &val))) {
            local_size = val->data.uint32;
            PMIX_VALUE_RELEASE(val);
        } else {
            RETURN_ERROR_MSG_PREINIT("PMIX_LOCAL_SIZE is not properly initiated (%d)\n", rc);
        }

        local_ranks = (int *)malloc(size * sizeof(int));

        for (int i = 0; i < size; i++) {
            local_ranks[i] = -1;
        }

        /* Note: PMIX_LOCAL_PROCS should be available in the near future (would avoid parsing) */
        if (PMIX_SUCCESS == (rc = PMIx_Get(&proc, PMIX_LOCAL_PEERS, NULL, 0, &val))) {
           char *local_peers_str = strdup(val->data.string);
           PMIX_VALUE_RELEASE(val);

           char *ptr = strtok(local_peers_str, ",");
           for (int i = 0; i < local_size; i++) {
              int idx = strtoul(ptr, NULL, 10);
              shmem_internal_assert(idx < shmem_internal_num_pes && idx >= 0);
              local_ranks[idx] = i;
              ptr = strtok(NULL, ",");
           }
           shmem_internal_assert(ptr == NULL);
           free(local_peers_str);
        } else {
           RETURN_ERROR_MSG_PREINIT("PMIX_LOCAL_PEERS is not properly initiated (%d)\n", rc);
        }

        proc.rank = myproc.rank;
        if (PMIX_SUCCESS == (rc = PMIx_Get(&proc, PMIX_LOCAL_RANK, NULL, 0, &val))) {
            local_rank = val->data.uint16;
            PMIX_VALUE_RELEASE(val);
        } else {
            RETURN_ERROR_MSG_PREINIT("PMIX_LOCAL_RANK is not properly initiated (%d)\n", rc);
        }
    }

    /* commit any values we "put" */
    if (PMIX_SUCCESS != (rc = PMIx_Commit())) {
        RETURN_ERROR_MSG("PMIx_Commit failed (%d)\n", rc);
        return rc;
    }

    /* execute a fence, directing that all info be exchanged */
    PMIX_INFO_CONSTRUCT(&info);
    PMIX_INFO_LOAD(&info, PMIX_COLLECT_DATA, &wantit, PMIX_BOOL);

    // Future optimization for when fabrics are ready to support the non-block-
    // ing capabilities. The commented out call function above, the commented
    // variable "bool active," and the PMIx_Fence_nb if-statement are here for
    // when that is ready. Current implementations will cause the test to hang.

    // if (PMIX_SUCCESS != (rc = PMIx_Fence_nb(NULL, 0, &info, 1, opcbfunc, &active))) {
    if (PMIX_SUCCESS != (rc = PMIx_Fence(NULL, 0, &info, 1))) {
        RETURN_ERROR_MSG("PMIx_Fence failed (%d)\n", rc);
    }
    PMIX_INFO_DESTRUCT(&info);

    return rc;
}


int
shmem_runtime_put(char *key, void *value, size_t valuelen)
{
    pmix_value_t val;
    pmix_status_t rc;

    PMIX_VALUE_CONSTRUCT(&val);
    val.type = PMIX_BYTE_OBJECT;
    val.data.bo.bytes = value;
    val.data.bo.size = valuelen;

    rc = PMIx_Put(PMIX_GLOBAL, key, &val);
    val.data.bo.bytes = NULL;  // protect the data
    val.data.bo.size = 0;
    PMIX_VALUE_DESTRUCT(&val);

    return rc;
}

/* I'm assuming you malloc'd a region and are giving me its length */
int
shmem_runtime_get(int pe, char *key, void *value, size_t valuelen)
{
    pmix_proc_t proc;
    pmix_value_t *val;
    pmix_status_t rc;

    /* ensure the region is zero'd out */
    memset(value, 0, valuelen);

    /* setup the ID of the proc whose info we are getting */
    strncpy(proc.nspace, myproc.nspace, PMIX_MAX_NSLEN);
    proc.rank = pe;

    rc = PMIx_Get(&proc, key, NULL, 0, &val);

    if (PMIX_SUCCESS == rc) {
        if (NULL != val) {
            /* see if the data fits into the given region */
            if (valuelen < val->data.bo.size) {
                PMIX_VALUE_RELEASE(val);
                return PMIX_ERROR;
            }
            /* copy the results across */
            memcpy(value, val->data.bo.bytes, val->data.bo.size);
            PMIX_VALUE_RELEASE(val);
        }
    }

    return rc;
}


void
shmem_runtime_barrier(void)
{
    PMIx_Fence(NULL, 0, NULL, 0);
}
