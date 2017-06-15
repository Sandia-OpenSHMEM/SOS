/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2016 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#include <stdlib.h>
#include <sys/time.h>
#include <sys/param.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmemx.h"
#include "shmem_internal.h"
#include "shmem_collectives.h"
#include "shmem_comm.h"
#include "runtime.h"
#include "build_info.h"

#ifdef __APPLE__
#include <mach-o/getsect.h>
#else
extern char data_start;
extern char end;
#endif

void *shmem_internal_heap_base = NULL;
long shmem_internal_heap_length = 0;
void *shmem_internal_data_base = NULL;
long shmem_internal_data_length = 0;
int shmem_internal_heap_use_huge_pages = 0;
long shmem_internal_heap_huge_page_size = 0;

int shmem_internal_my_pe = -1;
int shmem_internal_num_pes = -1;
int shmem_internal_initialized = 0;
int shmem_internal_finalized = 0;
int shmem_internal_initialized_with_start_pes = 0;
int shmem_internal_global_exit_called = 0;

int shmem_internal_thread_level;
int shmem_internal_debug = 0;
int shmem_internal_trap_on_abort = 0;

#ifdef ENABLE_THREADS
shmem_internal_mutex_t shmem_internal_mutex_alloc;
#endif

#ifdef USE_ON_NODE_COMMS
char *shmem_internal_location_array = NULL;
#endif

#ifdef MAXHOSTNAMELEN
static char shmem_internal_my_hostname[MAXHOSTNAMELEN];
#else
static char shmem_internal_my_hostname[HOST_NAME_MAX];
#endif


static void
shmem_internal_shutdown(void)
{
    if (!shmem_internal_initialized ||
        shmem_internal_finalized) {
        return;
    }

    shmem_internal_barrier_all();

    shmem_internal_finalized = 1;
    shmem_transport_fini();

#ifdef USE_XPMEM
    shmem_transport_xpmem_fini();
#endif
#ifdef USE_CMA
    shmem_transport_cma_fini();
#endif

    SHMEM_MUTEX_DESTROY(shmem_internal_mutex_alloc);

    shmem_internal_symmetric_fini();
    shmem_runtime_fini();
}


static void
shmem_internal_shutdown_atexit(void)
{
    if ( shmem_internal_initialized && !shmem_internal_finalized &&
         !shmem_internal_initialized_with_start_pes && !shmem_internal_global_exit_called &&
         shmem_internal_my_pe == 0) {
        RAISE_WARN_STR("shutting down without a call to shmem_finalize");
    }

    shmem_internal_shutdown();
}


void
shmem_internal_start_pes(int npes)
{
    int tl_provided;

    shmem_internal_initialized_with_start_pes = 1;
    shmem_internal_init(SHMEMX_THREAD_SINGLE, &tl_provided);
}


void
shmem_internal_init(int tl_requested, int *tl_provided)
{
    int ret;
    long eager_size;

    int runtime_initialized   = 0;
    int transport_initialized = 0;
#ifdef USE_XPMEM
    int xpmem_initialized     = 0;
#endif
#ifdef USE_CMA
    int cma_initialized       = 0;
#endif

    /* set up threading */
    SHMEM_MUTEX_INIT(shmem_internal_mutex_alloc);
#ifdef ENABLE_THREADS
    shmem_internal_thread_level = tl_requested;
    *tl_provided = tl_requested;
#else
    shmem_internal_thread_level = SHMEMX_THREAD_SINGLE;
    *tl_provided = SHMEMX_THREAD_SINGLE;
#endif

    /* Parse environment variables into shmem_internal_params */
    shmem_internal_parse_env();

    ret = shmem_runtime_init();
    if (0 != ret) {
        fprintf(stderr, "ERROR: runtime init failed: %d\n", ret);
        goto cleanup;
    }
    runtime_initialized = 1;
    shmem_internal_my_pe = shmem_runtime_get_rank();
    shmem_internal_num_pes = shmem_runtime_get_size();

    /* Ensure that the vendor string will not cause an overflow in user code */
    if (sizeof(SHMEM_VENDOR_STRING) > SHMEM_MAX_NAME_LEN) {
        RETURN_ERROR_MSG("SHMEM_VENDOR_STRING length (%zu) exceeds SHMEM_MAX_NAME_LEN (%d)\n",
                         sizeof(SHMEM_VENDOR_STRING), SHMEM_MAX_NAME_LEN);
        goto cleanup;
    }

    /* Unless the user asked for it, disable bounce buffering in MULTIPLE
     * because of threading overheads */
    if (shmem_internal_thread_level == SHMEMX_THREAD_MULTIPLE &&
        !shmem_internal_params.BOUNCE_SIZE_provided) {
        eager_size = 0;
    }
    else {
        eager_size = shmem_internal_params.BOUNCE_SIZE;
    }

    shmem_internal_debug = shmem_internal_params.DEBUG;
    shmem_internal_trap_on_abort = shmem_internal_params.TRAP_ON_ABORT;

    /* huge page support only on Linux for now, default is to use 2MB large pages */
#ifdef __linux__
    if (shmem_internal_params.SYMMETRIC_HEAP_USE_MALLOC == 0) {
        shmem_internal_heap_use_huge_pages =
            shmem_internal_param.SYMMETRIC_HEAP_USE_HUGE_PAGES;
        shmem_internal_heap_huge_page_size =
            shmem_internal_param.SYMMETRIC_HEAP_PAGE_SIZE;
    }
#endif

#ifdef USE_CMA
    shmem_transport_cma_put_max = shmem_internal_param.CMA_PUT_MAX;
    shmem_transport_cma_get_max = shmem_internal_param.CMA_GET_MAX;
#endif

    /* Find symmetric data */
#ifdef __APPLE__
    shmem_internal_data_base = (void*) get_etext();
    shmem_internal_data_length = get_end() - get_etext();
#else
    shmem_internal_data_base = &data_start;
    shmem_internal_data_length = (unsigned long) &end  - (unsigned long) &data_start;
#endif

#ifdef USE_ON_NODE_COMMS
    shmem_internal_location_array = malloc(sizeof(char) * shmem_internal_num_pes);
    if (NULL == shmem_internal_location_array) goto cleanup;

    memset(shmem_internal_location_array, -1, shmem_internal_num_pes);
#endif

    /* create symmetric heap */
    ret = shmem_internal_symmetric_init(shmem_internal_params.SYMMETRIC_SIZE,
                                        shmem_internal_params.SYMMETRIC_HEAP_USE_MALLOC);
    if (0 != ret) {
        RETURN_ERROR_MSG("Symmetric heap initialization failed (%d)\n", ret);
        goto cleanup;
    }

    /* Initialize transport devices */
    ret = shmem_transport_init(eager_size);
    if (0 != ret) {
        RETURN_ERROR_MSG("Transport init failed (%d)\n", ret);
        goto cleanup;
    }
    transport_initialized = 1;
#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_init(eager_size);
    if (0 != ret) {
        RETURN_ERROR_MSG("XPMEM init failed (%d)\n", ret);
        goto cleanup;
    }
    xpmem_initialized = 1;
#endif

#ifdef USE_CMA
    ret = shmem_transport_cma_init(eager_size);
    if (0 != ret) {
        RETURN_ERROR_MSG("CMA init failed (%d)\n", ret);
        goto cleanup;
    }
    cma_initialized = 1;
#endif

    /* exchange information */
    ret = shmem_runtime_exchange();
    if (0 != ret) {
        RETURN_ERROR_MSG("Runtime exchange failed (%d)\n", ret);
        goto cleanup;
    }

    /* finish transport initialization after information sharing. */
    ret = shmem_transport_startup();
    if (0 != ret) {
        RETURN_ERROR_MSG("Transport startup failed (%d)\n", ret);
        goto cleanup;
    }

#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_startup();
    if (0 != ret) {
        RETURN_ERROR_MSG("XPMEM startup failed (%d)\n", ret);
        goto cleanup;
    }
#endif
#ifdef USE_CMA
    ret = shmem_transport_cma_startup();
    if (0 != ret) {
        RETURN_ERROR_MSG("CMA startup failed (%d)\n", ret);
        goto cleanup;
    }
#endif

    ret = shmem_internal_collectives_init(shmem_internal_params.COLL_CROSSOVER,
                                          shmem_internal_params.COLL_RADIX);
    if (ret != 0) {
        RETURN_ERROR_MSG("Initialization of collectives failed (%d)\n", ret);
        goto cleanup;
    }

    atexit(shmem_internal_shutdown_atexit);
    shmem_internal_initialized = 1;

    /* get hostname for shmem_getnodename */
    if (gethostname(shmem_internal_my_hostname,
                    sizeof(shmem_internal_my_hostname))) {
        snprintf(shmem_internal_my_hostname,
                    sizeof(shmem_internal_my_hostname),
                    "ERR: gethostname '%s'?", strerror(errno));
    }

    /* last minute printing of information */
    if (0 == shmem_internal_my_pe) {
        if (shmem_internal_params.VERSION || shmem_internal_params.INFO ||
            shmem_internal_params.DEBUG)
        {
            printf(PACKAGE_STRING "\n");
        }

        if (shmem_internal_params.INFO) {
            shmem_internal_print_env();
        }

        if (shmem_internal_debug) {
            char *wrapped_configure_args = shmem_util_wrap(SOS_CONFIGURE_ARGS, 60,
                                                           "                        ");

            printf("Build information:\n");
#ifdef SOS_GIT_VERSION
            printf("%-23s %s\n", "Git Version", SOS_GIT_VERSION);
#endif
            printf("%-23s %s\n", "Configure Args", wrapped_configure_args);
            printf("%-23s %s\n", "Build Date", SOS_BUILD_DATE);
            printf("%-23s %s\n", "Build CC", SOS_BUILD_CC);
            printf("%-23s %s\n", "Build CFLAGS", SOS_BUILD_CFLAGS);
            printf("\n");

            free(wrapped_configure_args);
        }

        fflush(NULL);
    }

    DEBUG_MSG("Sym. heap=%p len=%ld -- data=%p len=%ld\n",
              shmem_internal_heap_base, shmem_internal_heap_length,
              shmem_internal_data_base, shmem_internal_data_length);

    /* finish up */
    shmem_runtime_barrier();
    return;

 cleanup:
    if (transport_initialized) {
        shmem_transport_fini();
    }

#ifdef USE_XPMEM
    if (xpmem_initialized) {
        shmem_transport_xpmem_fini();
    }
#endif
#ifdef USE_CMA
    if (cma_initialized) {
        shmem_transport_cma_fini();
    }
#endif
    if (NULL != shmem_internal_data_base) {
        shmem_internal_symmetric_fini();
    }
    if (runtime_initialized) {
        shmem_runtime_fini();
    }
    abort();
}


char *
shmem_internal_nodename(void)
{
    return shmem_internal_my_hostname;
}


void shmem_internal_finalize(void)
{
    shmem_internal_shutdown();
}


void
shmem_internal_global_exit(int status)
{
    char str[256];

    snprintf(str, 256, "PE %d called shmem_global_exit with status %d", shmem_internal_my_pe, status);

    shmem_internal_global_exit_called = 1;
    shmem_runtime_abort(status, str);
}
