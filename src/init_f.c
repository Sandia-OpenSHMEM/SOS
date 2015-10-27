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

#include <string.h>
#include "config.h"

#include "shmem.h"
#include "shmem_internal.h"


#define FC_START_PES FC_FUNC_(start_pes, START_PES)
void FC_START_PES(fortran_integer_t *npes);
void
FC_START_PES(fortran_integer_t *npes)
{
    int tl_provided;

    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init(SHMEMX_THREAD_SINGLE, &tl_provided);
}


#define FC_SHMEM_INIT FC_FUNC_(shmem_init, SHMEM_INIT)
void FC_SHMEM_INIT(void);
void
FC_SHMEM_INIT(void)
{
    int tl_provided;

    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init(SHMEMX_THREAD_SINGLE, &tl_provided);
}


#define FC_SHMEM_GLOBAL_EXIT FC_FUNC_(shmem_global_exit, SHMEM_GLOBAL_EXIT)
void FC_SHMEM_GLOBAL_EXIT(fortran_integer_t *status);
void
FC_SHMEM_GLOBAL_EXIT(fortran_integer_t *status)
{
    char str[256];

    SHMEM_ERR_CHECK_INITIALIZED();

    snprintf(str, 256, "PE %d called shmem_global_exit with status %d", shmem_internal_my_pe, *status);
    shmem_runtime_abort(*status, str);
}


#define FC_SHMEM_FINALIZE FC_FUNC_(shmem_finalize, SHMEM_FINALIZE)
void FC_SHMEM_FINALIZE(void);
void
FC_SHMEM_FINALIZE(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_finalize();
}


#define FC_SHMEM_INFO_GET_VERSION FC_FUNC_(shmem_info_get_version, SHMEM_INFO_GET_VERSION)
void FC_SHMEM_INFO_GET_VERSION(fortran_integer_t *major, fortran_integer_t *minor);
void
FC_SHMEM_INFO_GET_VERSION(fortran_integer_t *major, fortran_integer_t *minor)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    *major = SHMEM_MAJOR_VERSION;
    *minor = SHMEM_MINOR_VERSION;
}


#define FC_SHMEM_INFO_GET_NAME FC_FUNC_(shmem_info_get_name, SHMEM_INFO_GET_NAME)
void FC_SHMEM_INFO_GET_NAME(char *name, int32_t name_len);
void
FC_SHMEM_INFO_GET_NAME(char *name, int32_t name_len)
{
    size_t len = sizeof(SHMEM_VENDOR_STRING) - 1; /* Length without NULL terminator */

    SHMEM_ERR_CHECK_INITIALIZED();

    strncpy(name, SHMEM_VENDOR_STRING, SHMEM_MAX_NAME_LEN);

    /* Drop NULL terminator and fill with whitespace */
    memset(&name[len], ' ', SHMEM_MAX_NAME_LEN - len);
}
