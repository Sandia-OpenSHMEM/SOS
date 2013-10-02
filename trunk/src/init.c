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

#define _GNU_SOURCE /* for asprintf */
#include <stdlib.h>
#include <sys/time.h>
#include <sys/param.h>
#include <stdio.h>
#include <limits.h>
#include <string.h>
#include <errno.h>
#include <unistd.h>

#include "shmem.h"
#include "shmem_internal.h"
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

int shmem_internal_my_pe = -1;
int shmem_internal_num_pes = -1;
int shmem_internal_initialized = 0;
int shmem_internal_finalized = 0;

#ifdef USE_ON_NODE_COMMS
char *shmem_internal_location_array = NULL;
#endif

#ifdef MAXHOSTNAMELEN
static char shmem_internal_my_hostname[MAXHOSTNAMELEN];
#else
static char shmem_internal_my_hostname[HOST_NAME_MAX];
#endif

 
/* atol() + optional scaled suffix recognition: 1K, 2M, 3G, 1T */
static long
atol_scaled(char *s)
{
    long val;
    char *e;

    val = strtol(s,&e,0);
    if (e == NULL || *e =='\0')
        return val;

    if (*e == 'K')
        val *= 1024L;
    else if (*e == 'M')
        val *= 1024L*1024L;
    else if (*e == 'G')
        val *= 1024L*1024L*1024L;
    else if (*e == 'T')
        val *= 1024L*1024L*1024L*1024L;

    return val;
}


static long
get_env_long(const char* name, int is_sized, long default_value)
{
    char *env_name, *env_value;

    asprintf(&env_name, "SMA_%s", name);
    env_value = getenv(env_name);
    free(env_name);
    if (env_value != NULL) {
        if (is_sized) {
            return atol_scaled(env_value);
        } else {
            return atol(env_value);
        }
    }

    asprintf(&env_name, "SHMEM_%s", name);
    env_value = getenv(env_name);
    free(env_name);
    if (env_value != NULL) {
        if (is_sized) {
            return atol_scaled(env_value);
        } else {
            return atol(env_value);
        }
    }

    return default_value;
}


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
#ifdef USE_XPMEM
    shmem_transport_xpmem_fini();
#endif

    shmem_internal_symmetric_fini();
    shmem_runtime_fini();
}


void
shmem_internal_init()
{
    int ret;
    int radix = -1, crossover = -1;
    long heap_size, eager_size;
    int heap_use_malloc = 0;

    ret = shmem_runtime_init();
    if (0 != ret) {
        fprintf(stderr, "ERROR: runtime init failed: %d\n", ret);
        goto cleanup;
    }
    shmem_internal_my_pe = shmem_runtime_get_rank();
    shmem_internal_num_pes = shmem_runtime_get_size();

    /* Process environment variables */
    radix = get_env_long("COLL_RADIX", 0, 4);
    crossover = get_env_long("COLL_CROSSOVER", 0, 4);
    heap_size = get_env_long("SYMMETRIC_SIZE", 1, 512 * 1024 * 1024);
    eager_size = get_env_long("BOUNCE_SIZE", 1, 2048);
    heap_use_malloc = get_env_long("SYMMETRIC_HEAP_USE_MALLOC", 0, 0);

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
#ifdef USE_PORTALS4
    ret = shmem_transport_portals4_init(eager_size);
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: Portals 4 init failed\n",
                shmem_internal_my_pe);
        goto cleanup;
    }
#endif
#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_init(eager_size);
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: XPMEM init failed\n",
                shmem_internal_my_pe);
        goto cleanup;
    }
#endif

    /* exchange information */
    ret = shmem_runtime_exchange();
    if (0 != ret) {
        fprintf(stderr, "[%03d] ERROR: runtime exchange failed: %d\n", 
                shmem_internal_my_pe, ret);
        goto cleanup;
    }

    /* finish transport initialization after information sharing. */
#ifdef USE_PORTALS4
    ret = shmem_transport_portals4_startup();
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: Portals 4 startup failed\n",
                shmem_internal_my_pe);
        goto cleanup;
    }
#else
#error "Need connectivity information, no portals support"
#endif
#ifdef USE_XPMEM
    ret = shmem_transport_xpmem_startup();
    if (0 != ret) {
        fprintf(stderr,
                "[%03d] ERROR: XPMEM startup failed\n",
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

        if (NULL != getenv("SMA_INFO")) {
            printf(PACKAGE_STRING "\n\n");
            printf("SMA_VERSION             %s\n", 
                   (NULL != getenv("SMA_VERSION")) ? "Set" : "Not set");
            printf("\tIf set, print library version at startup\n");
            printf("SMA_INFO                %s\n", 
                   (NULL != getenv("SMA_VERSION")) ? "Set" : "Not set");
            printf("\tIf set, print this help message at startup\n");
            printf("SMA_SYMMETRIC_SIZE      %ld\n", heap_size);
            printf("\tSymmentric heap size\n");
            printf("SMA_COLL_CROSSOVER      %d\n", crossover);
            printf("\tCross-over between linear and tree collectives\n");
            printf("SMA_COLL_RADIX          %d\n", radix);
            printf("\tRadix for tree-based collectives\n");
            printf("SMA_BOUNCE_SIZE    %ld\n", eager_size);
            printf("\tMaximum message size to bounce buffer\n");
            fflush(NULL);
        }
    }

    /* finish up */
    shmem_runtime_barrier();
    return;

 cleanup:
#ifdef USE_PORTALS4
    shmem_transport_portals4_fini();
#endif
#ifdef USE_XPMEM
    shmem_transport_xpmem_fini();
#endif
    if (NULL != shmem_internal_data_base) {
        shmem_internal_symmetric_fini();
    }
    shmem_runtime_fini();
    abort();
}


char *
shmem_internal_nodename(void)
{
    return shmem_internal_my_hostname;
}
