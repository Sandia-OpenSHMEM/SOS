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

#ifdef __APPLE__
#include <mach-o/getsect.h>
#else
extern char data_start;
extern char end;
#endif

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



static void
shmem_internal_shutdown(void)
{
    if (!shmem_internal_initialized ||
        shmem_internal_finalized) {
        return;
    }
    shmem_internal_finalized = 1;

#ifdef USE_PORTALS4
    shmem_transport_portals4_fini();
#endif

    shmem_internal_symmetric_fini();
    shmem_internal_runtime_fini();
    PtlFini();
}


void
start_pes(int npes)
{
    int ret;
    char *s;
    int radix = -1, crossover = -1;

    if (shmem_internal_initialized) {
        RAISE_ERROR_STR("attempt to reinitialize library");
    }

    ret = shmem_internal_runtime_init();
    if (0 != ret) {
        fprintf(stderr, "ERROR: runtime init failed: %d\n", ret);
        goto cleanup;
    }
    shmem_internal_my_pe = shmem_internal_runtime_get_rank();
    shmem_internal_num_pes = shmem_internal_runtime_get_size();

    /* Find symmetric data */
#ifdef __APPLE__
    shmem_internal_data_base = (void*) get_etext();
    shmem_internal_data_length = get_end() - get_etext();
#else
    shmem_internal_data_base = &data_start;
    shmem_internal_data_length = (unsigned long) &end  - (unsigned long) &data_start;
#endif
    /* create symmetric heap */
    ret = shmem_internal_symmetric_init();
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: symmetric heap initialization failed: %d\n",
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* Initialize transport devices */
#ifdef USE_PORTALS4
    ret = shmem_transport_portals4_init();
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: Portals 4 init failed\n", shmem_internal_my_pe);
    }
#endif

    /* exchange information */
    ret = shmem_runtime_exchange();
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: runtime exchange failed: %d\n", 
                shmem_internal_my_pe, ret);
    }

    /* finish transport initialization after information sharing */
#ifdef USE_PORTALS4
    ret = shmem_transport_portals4_startup(shmem_internal_data_base,
                                           shmem_internal_data_length,
                                           shmem_internal_heap_base,
                                           shmem_internal_heap_length);
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: Portals 4 startup failed\n", shmem_internal_my_pe);
    }
#endif

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

    /* get hostname for shmem_getnodename */
    if (gethostname(shmem_internal_my_hostname,
                    sizeof(shmem_internal_my_hostname))) {
        sprintf(shmem_internal_my_hostname, "ERR: gethostname '%s'?",
                strerror(errno));
    }

    /* last minute printing of information */
    if (0 == shmem_internal_my_pe) {
        if (NULL != getenv("SMA_VERSION")) {
            printf(PACKAGE_STRING "\n");
            fflush(NULL);
        }
    }

    /* finish up */
    shmem_internal_runtime_barrier();
    return;

 cleanup:
#ifdef USE_PORTALS4
    shmem_transport_portals4_fini();
#endif
    if (NULL != shmem_internal_data_base) {
        shmem_internal_symmetric_fini();
    }
    shmem_internal_runtime_fini();
    abort();
}


char *
shmem_nodename(void)
{
    return shmem_internal_my_hostname;
}
