/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include <string.h>
#include "config.h"

#include "shmem.h"
#include "shmemx.h"
#include "shmem_internal.h"
#include "runtime.h"

#ifdef ENABLE_PROFILING

#pragma weak start_pes = pstart_pes
#define start_pes pstart_pes

#pragma weak shmem_init = pshmem_init
#define shmem_init pshmem_init

#pragma weak shmemx_nodename = pshmemx_nodename
#define shmemx_nodename pshmemx_nodename

#pragma weak shmemx_init_thread = pshmemx_init_thread
#define shmemx_init_thread pshmemx_init_thread

#pragma weak shmem_finalize = pshmem_finalize
#define shmem_finalize pshmem_finalize

#pragma weak shmem_info_get_version = pshmem_info_get_version
#define shmem_info_get_version pshmem_info_get_version

#pragma weak shmem_info_get_name = pshmem_info_get_name
#define shmem_info_get_name pshmem_info_get_name

#endif /* ENABLE_PROFILING */

void
start_pes(int npes)
{
    int tl_provided;

    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init(SHMEMX_THREAD_SINGLE, &tl_provided);
}


void
shmem_init(void)
{
    int tl_provided;

    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init(SHMEMX_THREAD_SINGLE, &tl_provided);
}


char *
shmemx_nodename(void)
{
    return shmem_internal_nodename();
}


void
shmemx_init_thread(int tl_requested, int *tl_provided)
{
    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    shmem_internal_init(tl_requested, tl_provided);
}


void
shmem_global_exit(int status)
{
    char str[256];

    SHMEM_ERR_CHECK_INITIALIZED();

    snprintf(str, 256, "PE %d called shmem_global_exit with status %d", shmem_internal_my_pe, status);
    shmem_runtime_abort(status, str);
}


void
shmem_finalize(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    shmem_internal_finalize();
}


void
shmem_info_get_version(int *major, int *minor)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    *major = SHMEM_MAJOR_VERSION;
    *minor = SHMEM_MINOR_VERSION;
}


void
shmem_info_get_name(char *name)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    strncpy(name, SHMEM_VENDOR_STRING, SHMEM_MAX_NAME_LEN);
    name[SHMEM_MAX_NAME_LEN-1] = '\0'; /* Ensure string is null terminated */
}
