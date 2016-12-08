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
shmem_internal_shutdown(int barrier_requested)
{
    if (!shmem_internal_initialized ||
        shmem_internal_finalized) {
        return;
    }
    shmem_internal_finalized = 1;

    if (barrier_requested)
        shmem_internal_barrier_all();

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
        fprintf(stderr, "Warning: shutting down without a call to shmem_finalize()\n");
    }

    shmem_internal_shutdown(0);
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
    int radix = -1, crossover = -1;
    long heap_size, eager_size;
    int heap_use_malloc = 0;

    int runtime_initialized   = 0;
    int transport_initialized = 0;
#ifdef USE_XPMEM
    int xpmem_initialized     = 0;
#endif
#ifdef USE_CMA
    int cma_initialized       = 0;
#endif

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
        fprintf(stderr,
                "[%03d] ERROR: SHMEM_VENDOR_STRING length (%zu) exceeds SHMEM_MAX_NAME_LEN (%d)\n",
                shmem_internal_my_pe, sizeof(SHMEM_VENDOR_STRING), SHMEM_MAX_NAME_LEN);
        goto cleanup;
    }

    /* Process environment variables */
    radix = shmem_util_getenv_long("COLL_RADIX", 0, 4);
    crossover = shmem_util_getenv_long("COLL_CROSSOVER", 0, 4);
    heap_size = shmem_util_getenv_long("SYMMETRIC_SIZE", 1, 512 * 1024 * 1024);
    eager_size = shmem_util_getenv_long("BOUNCE_SIZE", 1, 2048);
    heap_use_malloc = shmem_util_getenv_long("SYMMETRIC_HEAP_USE_MALLOC", 0, 0);
    shmem_internal_debug = (NULL != shmem_util_getenv_str("DEBUG")) ? 1 : 0;

    /* huge page support only on Linux for now, default is to use 2MB large pages */
#ifdef __linux__
    if (heap_use_malloc == 0) {
        shmem_internal_heap_use_huge_pages=
             (shmem_util_getenv_str("SYMMETRIC_HEAP_USE_HUGE_PAGES") != NULL) ? 1 : 0;
        shmem_internal_heap_huge_page_size = shmem_util_getenv_long("SYMMETRIC_HEAP_PAGE_SIZE",
                                                                    1,
                                                                    2 * 1024 * 1024);
    }
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
    ret = shmem_internal_symmetric_init(heap_size, heap_use_malloc);
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: symmetric heap initialization failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* Initialize transport devices */
    ret = shmem_transport_init(eager_size);
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: Transport init failed\n",
                shmem_internal_my_pe);
        goto cleanup;
    }
    transport_initialized = 1;
#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_init(eager_size);
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: XPMEM init failed\n",
                shmem_internal_my_pe);
        goto cleanup;
    }
    xpmem_initialized = 1;
#endif
#ifdef USE_CMA
    shmem_transport_cma_put_max = shmem_util_getenv_long("CMA_PUT_MAX", 1, 8*1024);
    shmem_transport_cma_get_max = shmem_util_getenv_long("CMA_GET_MAX", 1, 16*1024);

    ret = shmem_transport_cma_init(eager_size);
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: CMA init failed\n",
                shmem_internal_my_pe);
        goto cleanup;
    }
    cma_initialized = 1;
#endif

    /* exchange information */
    ret = shmem_runtime_exchange();
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: runtime exchange failed: %d\n", 
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* finish transport initialization after information sharing. */
    ret = shmem_transport_startup();
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: Transport startup failed\n",
                shmem_internal_my_pe);
        goto cleanup;
    }

#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_startup();
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: XPMEM startup failed\n",
                shmem_internal_my_pe);
        goto cleanup;
    }
#endif
#ifdef USE_CMA
    ret = shmem_transport_cma_startup();
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: CMA startup failed\n",
                shmem_internal_my_pe);
        goto cleanup;
    }
#endif

    ret = shmem_internal_collectives_init(crossover, radix);
    if (ret != 0) {
        fprintf(stderr,
                "[%03d] ERROR: initialization of collectives failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    atexit(shmem_internal_shutdown_atexit);
    shmem_internal_initialized = 1;

    /* set up threading */
    SHMEM_MUTEX_INIT(shmem_internal_mutex_alloc);
#ifdef ENABLE_THREADS
    shmem_internal_thread_level = tl_requested;
    *tl_provided = tl_requested;
#else
    shmem_internal_thread_level = SHMEMX_THREAD_SINGLE;
    *tl_provided = SHMEMX_THREAD_SINGLE;
#endif

    /* get hostname for shmem_getnodename */
    if (gethostname(shmem_internal_my_hostname,
                    sizeof(shmem_internal_my_hostname))) {
        snprintf(shmem_internal_my_hostname,
                    sizeof(shmem_internal_my_hostname),
                    "ERR: gethostname '%s'?", strerror(errno));
    }

    /* last minute printing of information */
    if (0 == shmem_internal_my_pe) {
        if (NULL != shmem_util_getenv_str("VERSION")) {
            printf(PACKAGE_STRING "\n");
            fflush(NULL);
        }

        if (NULL != shmem_util_getenv_str("INFO")) {
            printf(PACKAGE_STRING "\n\n");
            printf("SMA_VERSION             %s\n",
                   (NULL != shmem_util_getenv_str("VERSION")) ? "Set" : "Not set");
            printf("\tIf set, print library version at startup\n");
            printf("SMA_INFO                %s\n",
                   (NULL != shmem_util_getenv_str("INFO")) ? "Set" : "Not set");
            printf("\tIf set, print this help message at startup\n");
            printf("SMA_SYMMETRIC_SIZE      %ld\n", heap_size);
            printf("\tSymmetric heap size\n");
            printf("SMA_SYMMETRIC_HEAP_USE_MALLOC %s\n",
                   (0 != heap_use_malloc) ? "Set" : "Not set");
            printf("\tIf set, allocate the symmetric heap using malloc\n");
            if (heap_use_malloc == 0) {
                printf("SMA_SYMMETRIC_HEAP_USE_HUGE_PAGES %s\n",
                        shmem_internal_heap_use_huge_pages ? "Yes" : "No");
                if (shmem_internal_heap_use_huge_pages) {
                    printf("SMA_SYMMETRIC_HEAP_PAGE_SIZE %ld \n",
                           shmem_internal_heap_huge_page_size);
                }
                printf("\tSymmetric heap use large pages\n");
            }
            printf("SMA_COLL_CROSSOVER      %d\n", crossover);
            printf("\tCross-over between linear and tree collectives\n");
            printf("SMA_COLL_RADIX          %d\n", radix);
            printf("\tRadix for tree-based collectives\n");
            printf("SMA_BOUNCE_SIZE         %ld\n", eager_size);
            printf("\tMaximum message size to bounce buffer\n");
            printf("SMA_BARRIER_ALGORITHM   %s\n", coll_type_str[shmem_internal_barrier_type]);
            printf("\tAlgorithm for barrier.  Options are auto, linear, tree, dissem\n");
            printf("SMA_BCAST_ALGORITHM     %s\n", coll_type_str[shmem_internal_bcast_type]);
            printf("\tAlgorithm for broadcast.  Options are auto, linear, tree\n");
            printf("SMA_REDUCE_ALGORITHM    %s\n", coll_type_str[shmem_internal_reduce_type]);
            printf("\tAlgorithm for reductions.  Options are auto, linear, tree, recdbl\n");
            printf("SMA_COLLECT_ALGORITHM   %s\n", coll_type_str[shmem_internal_collect_type]);
            printf("\tAlgorithm for collect.  Options are auto, linear\n");
            printf("SMA_FCOLLECT_ALGORITHM  %s\n", coll_type_str[shmem_internal_fcollect_type]);
            printf("\tAlgorithm for fcollect.  Options are auto, linear, ring, recdbl\n");
#ifdef USE_CMA
            printf("SMA_CMA_PUT_MAX         %zu\n", shmem_transport_cma_put_max);
            printf("SMA_CMA_GET_MAX         %zu\n", shmem_transport_cma_get_max);
#endif /* USE_CMA */
            printf("SMA_DEBUG               %s\n", (NULL != shmem_util_getenv_str("DEBUG")) ? "On" : "Off");
            printf("\tEnable debugging messages.\n");

            shmem_transport_print_info();
            printf("\n");
            fflush(NULL);
        }
    }

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
    shmem_internal_shutdown(1);
}


void
shmem_internal_global_exit(int status)
{
    char str[256];

    snprintf(str, 256, "PE %d called shmem_global_exit with status %d", shmem_internal_my_pe, status);

    shmem_internal_global_exit_called = 1;
    shmem_runtime_abort(status, str);
}
