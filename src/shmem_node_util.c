/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2018 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#include "shmem_internal.h"
#include "shmem_node_util.h"

int *shmem_internal_location_array = NULL;

static int node_util_is_initialized = 0;
static int node_util_is_started = 0;
static int n_local_pes = 0;
static shmem_node_util_addr_t shmem_node_util_addr;
static char hostname[SHMEM_INTERNAL_MAX_HOSTNAME_LEN];

int shmem_node_util_init(void)
{
    int ret, i;

    if (!node_util_is_initialized) {

        shmem_internal_location_array = malloc(sizeof(int) * shmem_internal_num_pes);
        if (NULL == shmem_internal_location_array) {
            RETURN_ERROR_STR("Out of memory when allocating node_util location array");
            return 1;
        }

        for (i = 0; i < shmem_internal_num_pes; i++) {
            shmem_internal_location_array[i] = -1;
        }

        ret = shmem_node_util_gethostname(hostname);
        if (ret != 0) {
            RETURN_ERROR_MSG("shmem_node_util_gethostname failed (%d)", ret);
            return ret;
        }

        size_t hostname_len = strlen(hostname);

        ret = shmem_runtime_put("hostname_len", &hostname_len, sizeof(size_t));
        if (ret != 0) {
            RETURN_ERROR_MSG("Failed during hostname_len store to KVS: (%d)", ret);
            return ret;
        }

        ret = shmem_runtime_put("hostname", hostname, hostname_len);
        if (ret != 0) {
            RETURN_ERROR_MSG("Failed during hostname store to KVS: (%d)", ret);
            return ret;
        }

        shmem_node_util_addr.addr = hostname;
        shmem_node_util_addr.addrlen = hostname_len;

        node_util_is_initialized = 1;
    } else {
        RAISE_WARN_STR("Initialized node utility more than once");
    }

    return 0;
}


void shmem_node_util_fini(void)
{
    free(shmem_internal_location_array);
    return;
}

/* This function should only be called after the shmem_runtime KVS has synchronized */
int shmem_node_util_startup(void)
{
    int ret, i;
    shmem_node_util_addr_t addr_cmp = {0};

    if (node_util_is_initialized) {
        if (!node_util_is_started) {
            for (i = 0; i < shmem_internal_num_pes; i++) {
                ret = shmem_runtime_get(i, "hostname_len", &addr_cmp.addrlen, sizeof(size_t));
                if (ret != 0) {
                    RETURN_ERROR_MSG("Failed during hostname_len read from KVS: (%d)", ret);
                    return ret;
                }

                addr_cmp.addr = malloc(sizeof(char) * addr_cmp.addrlen);
                if (addr_cmp.addr == NULL) {
                    RETURN_ERROR_STR("Out of memory when allocating node_util address");
                    return 1;
                }

                ret = shmem_runtime_get(i, "hostname", addr_cmp.addr, addr_cmp.addrlen);
                if (ret != 0) {
                    RETURN_ERROR_MSG("Failed during hostname read from KVS: (%d)", ret);
                    return ret;
                }
                if (shmem_node_util_same_node(&shmem_node_util_addr, &addr_cmp)) {
                    shmem_node_util_set_local_rank(i, n_local_pes++);
                }
                free(addr_cmp.addr);
            }
            shmem_internal_assert(n_local_pes > 0);
            node_util_is_started = 1;
        } else {
            RAISE_WARN_STR("Called node utility startup routine more than once");
        }
    } else {
        RAISE_ERROR_STR("Node utility has not initialized");
    }


    return 0;
}


int shmem_node_util_get_local_n_pes()
{
#ifdef USE_MEMCPY
    return 1;
#else
    return n_local_pes;
#endif
}
