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

#include "config.h"

#ifdef HAVE_SCHED_GETAFFINITY
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <sched.h>
#endif

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
#include "shmem_internal.h"
#include "shmem_collectives.h"
#include "shmem_comm.h"
#include "runtime.h"
#include "build_info.h"

#if defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING) && defined(__linux__)
#include <sys/personality.h>
#endif

#ifdef __APPLE__
#include <mach-o/getsect.h>
#else
/* Declare data_start and end as weak to avoid a linker error if the symbols
 * are not present.  During initialization we check if the symbols exist. */
#pragma weak __data_start
#pragma weak _end
extern int __data_start;
extern int _end;
#endif

void *shmem_internal_heap_base = NULL;
long shmem_internal_heap_length = 0;
void *shmem_internal_data_base = NULL;
long shmem_internal_data_length = 0;

int shmem_internal_my_pe = -1;
int shmem_internal_num_pes = -1;
int shmem_internal_initialized = 0;
int shmem_internal_finalized = 0;
int shmem_internal_initialized_with_start_pes = 0;
int shmem_internal_global_exit_called = 0;

int shmem_internal_thread_level;

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

static char *shmem_internal_thread_level_str[4] = { "SINGLE", "FUNNELED",
                                                    "SERIALIZED", "MULTIPLE" };

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
    shmem_internal_init(SHMEM_THREAD_SINGLE, &tl_provided);
}


void
shmem_internal_init(int tl_requested, int *tl_provided)
{
    int ret;
    char errmsg[256];

    int runtime_initialized   = 0;
    int transport_initialized = 0;
#ifdef USE_XPMEM
    int xpmem_initialized     = 0;
#endif
#ifdef USE_CMA
    int cma_initialized       = 0;
#endif

#ifdef HAVE_SCHED_GETAFFINITY
    cpu_set_t my_set;
    int core_count = 0;
#endif

    /* set up threading */
    SHMEM_MUTEX_INIT(shmem_internal_mutex_alloc);
#ifdef ENABLE_THREADS
    shmem_internal_thread_level = tl_requested;
    *tl_provided = tl_requested;
#else
    shmem_internal_thread_level = SHMEM_THREAD_SINGLE;
    *tl_provided = SHMEM_THREAD_SINGLE;
#endif

    ret = shmem_runtime_init();
    if (0 != ret) {
        fprintf(stderr, "ERROR: runtime init failed: %d\n", ret);
        goto cleanup;
    }
    runtime_initialized = 1;
    shmem_internal_my_pe = shmem_runtime_get_rank();
    shmem_internal_num_pes = shmem_runtime_get_size();

    /* Parse environment variables into shmem_internal_params */
    shmem_internal_parse_env();

    /* Ensure that the vendor string will not cause an overflow in user code */
    if (sizeof(SHMEM_VENDOR_STRING) > SHMEM_MAX_NAME_LEN) {
        RETURN_ERROR_MSG("SHMEM_VENDOR_STRING length (%zu) exceeds SHMEM_MAX_NAME_LEN (%d)\n",
                         sizeof(SHMEM_VENDOR_STRING), SHMEM_MAX_NAME_LEN);
        goto cleanup;
    }

    /* Unless the user asked for it, disable bounce buffering in MULTIPLE
     * because of threading overheads */
    if (shmem_internal_thread_level == SHMEM_THREAD_MULTIPLE &&
        !shmem_internal_params.BOUNCE_SIZE_provided) {
        shmem_internal_params.BOUNCE_SIZE = 0;
    }

    /* Print library parameters */
    if (0 == shmem_internal_my_pe) {
        if (shmem_internal_params.VERSION || shmem_internal_params.INFO ||
            shmem_internal_params.DEBUG)
        {
            printf(PACKAGE_STRING "\n");
        }

        if (shmem_internal_params.INFO) {
            shmem_internal_print_env();
            printf("\n");
        }

        if (shmem_internal_params.DEBUG) {
            char *wrapped_configure_args = shmem_util_wrap(SOS_CONFIGURE_ARGS, 60,
                                                           "                        ");
            char *wrapped_build_cflags   = shmem_util_wrap(SOS_BUILD_CFLAGS, 60,
                                                           "                        ");

            printf("Build information:\n");
#ifdef SOS_GIT_VERSION
            printf("%-23s %s\n", "  Git Version", SOS_GIT_VERSION);
#endif
            printf("%-23s %s\n", "  Configure Args", wrapped_configure_args);
            printf("%-23s %s\n", "  Build Date", SOS_BUILD_DATE);
            printf("%-23s %s\n", "  Build CC", SOS_BUILD_CC);
            printf("%-23s %s\n", "  Build CFLAGS", wrapped_build_cflags);
            printf("\n");

            free(wrapped_configure_args);
            free(wrapped_build_cflags);
        }

        fflush(NULL);
    }

    /* ASLR is an OS security feature that randomizes the address map of each
     * process.  Remote virtual addressing assumes that symmetric addresses are
     * identical across processes.  ASLR can violate this assumption.
     *
     * However, ASLR does not always preclude identical symmetric addresses
     * across PEs.  Linking the application with -no-pie can cause the OS to
     * load the data segment at symmetric addresses.  The heap is mmap'd
     * relative to the location of the data segment and will also be symmetric.
     * Thus, we allow advanced users to disable this check. */
#if defined(ENABLE_REMOTE_VIRTUAL_ADDRESSING) && defined(__linux__) && !defined(DISABLE_ASLR_CHECK_AC)
    if (!shmem_internal_params.DISABLE_ASLR_CHECK) {
        FILE *aslr = fopen("/proc/sys/kernel/randomize_va_space", "r");
        if (aslr) {
            int aslr_status = fgetc(aslr);
            if (aslr_status != EOF && aslr_status != '0') {
                int persona = personality(0xffffffff);
                /* Check if ASLR was disabled with setarch */
                if (! (persona & ADDR_NO_RANDOMIZE)) {
                    RAISE_ERROR_MSG("Remote virtual addressing is enabled; however, address space layout randomization\n"
                                    RAISE_PE_PREFIX
                                    "is present.  Disable ASLR or rebuild without '--enable-remote-virtual-addressing'.\n"
                                    RAISE_PE_PREFIX
                                    "This error message can be disabled by setting SHMEM_DISABLE_ASLR_CHECK or building\n"
                                    RAISE_PE_PREFIX
                                    "with --disable-aslr-check.\n",
                                    shmem_internal_my_pe, shmem_internal_my_pe, shmem_internal_my_pe);
                }
            }
            fclose(aslr);
        }
    }
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */


    /* Find symmetric data */
#ifdef __APPLE__
    shmem_internal_data_base = (void*) get_etext();
    shmem_internal_data_length = get_end() - get_etext();
#else
    /* We declare data_start and end as weak symbols, which allows them to
     * remain unbound after dynamic linking.  This is needed for compatibility
     * with binaries (e.g. forked processes or tools) that are used with
     * OpenSHMEM programs but don't themselves use OpenSHMEM.  Such binaries
     * need not be compiled with the OpenSHMEM library and, as a result, will
     * not have exposed these symbols for dynamic linking.  However, if the
     * OpenSHMEM library has a strong dependence on the symbols, the dynamic
     * linker will flag an error when loading the binary.
     *
     * If the data_start and end symbols are unbound, the dynamic linker will
     * assign them an lvalue of 0.  Here, we check that the binary that
     * initializes OpenSHMEM has exposed these symbols, enabling the library to
     * locate its symmetric data segment. */

    if (&__data_start == (int*) 0 || &_end == (int*) 0)
        RETURN_ERROR_MSG("Unable to locate symmetric data segment (%p, %p)\n",
                         (void*) &__data_start, (void*) &_end);

    shmem_internal_data_base = (void*) &__data_start;
    shmem_internal_data_length = (long) ((char*) &_end - (char*) &__data_start);
#endif

    /* create symmetric heap */
    ret = shmem_internal_symmetric_init();
    if (0 != ret) {
        RETURN_ERROR_MSG("Symmetric heap initialization failed (%d)\n", ret);
        goto cleanup;
    }

    DEBUG_MSG("Thread level=%s, Num. PEs=%d\n"
              RAISE_PE_PREFIX
              "Sym. heap=%p len=%ld -- data=%p len=%ld\n",
              shmem_internal_thread_level_str[shmem_internal_thread_level],
              shmem_internal_num_pes,
              shmem_internal_my_pe,
              shmem_internal_heap_base, shmem_internal_heap_length,
              shmem_internal_data_base, shmem_internal_data_length);

#ifdef HAVE_SCHED_GETAFFINITY
    if (shmem_internal_params.DEBUG) {
        CPU_ZERO(&my_set);

        ret = sched_getaffinity(0, sizeof(my_set), &my_set);

        if (ret == 0) {
            char *cores_str = malloc(sizeof(char) * CPU_SETSIZE * 5 + 2);
            if (NULL == cores_str) goto cleanup;

            strcpy(cores_str," ");
            size_t off = 1; /* start after " " */
            for (int i = 0; i < CPU_SETSIZE; i++) {
                if (CPU_ISSET(i, &my_set)) {
                    core_count++;
                    off += snprintf(cores_str+off, CPU_SETSIZE*5+2-off, "%d ", i);
                }
            }
            DEBUG_MSG("affinity to %d processor cores: {%s}\n", core_count, cores_str);
            free(cores_str);
        }
    }
#endif

#ifdef USE_ON_NODE_COMMS
    shmem_internal_location_array = malloc(sizeof(char) * shmem_internal_num_pes);
    if (NULL == shmem_internal_location_array) goto cleanup;

    memset(shmem_internal_location_array, -1, shmem_internal_num_pes);
#endif

    /* Initialize transport devices */
    ret = shmem_transport_init();
    if (0 != ret) {
        RETURN_ERROR_MSG("Transport init failed (%d)\n", ret);
        goto cleanup;
    }
    transport_initialized = 1;
#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_init();
    if (0 != ret) {
        RETURN_ERROR_MSG("XPMEM init failed (%d)\n", ret);
        goto cleanup;
    }
    xpmem_initialized = 1;
#endif

#ifdef USE_CMA
    ret = shmem_transport_cma_init();
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

    ret = shmem_internal_collectives_init();
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
                    "ERR: gethostname '%s'?",
                    shmem_util_strerror(errno, errmsg, 256));
    }

    /* finish up */
#ifndef USE_PMIX
    shmem_runtime_barrier();
#endif
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

