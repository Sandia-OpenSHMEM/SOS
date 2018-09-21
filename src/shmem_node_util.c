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
#include <unistd.h>

#include "shmem_internal.h"
#include "shmem_node_util.h"

int *shmem_internal_location_array = NULL;

static int node_util_is_initialized = 0;
static int node_util_is_started = 0;
static int n_local_pes = 0;
static shmem_transport_addr_t addr;

int shmem_node_util_init(void)
{
    int ret, i;

    if (!node_util_is_initialized) {
        addr = shmem_transport_get_local_addr();

        shmem_internal_location_array = malloc(sizeof(int) * shmem_internal_num_pes);
        if (NULL == shmem_internal_location_array) {
            RETURN_ERROR_STR("Out of memory when allocating node_util location array");
            return 1;
        }

        for (i = 0; i < shmem_internal_num_pes; i++) {
            shmem_internal_location_array[i] = -1;
        }

        ret = shmem_runtime_put("nodename-len", &addr.addrlen, sizeof(size_t));
        if (ret != 0) {
            RETURN_ERROR_MSG("Failed during nodename-len store to KVS: (%d)", ret);
            return ret;
        }

        ret = shmem_runtime_put("nodename", addr.addr, addr.addrlen);
        if (ret != 0) {
            RETURN_ERROR_MSG("Failed during nodename store to KVS: (%d)", ret);
            return ret;
        }

        node_util_is_initialized = 1;
    } else {
        if (shmem_internal_params.DEBUG) {
            RAISE_WARN_STR("Initialized node utility more than once");
        }
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
    shmem_transport_addr_t addr_cmp = {0};
    size_t addr_len;

    if (node_util_is_initialized) {
        if (!node_util_is_started) {
            for (i = 0; i < shmem_internal_num_pes; i++) {
                ret = shmem_runtime_get(i, "nodename-len", &addr_len, sizeof(size_t));
                if (ret != 0) {
                    RETURN_ERROR_MSG("Failed during nodename-len read from KVS: (%d)", ret);
                    return ret;
                }

                addr_cmp.addrlen = addr_len;
                addr_cmp.addr = malloc(addr_len);
                if (addr_cmp.addr == NULL) {
                    RETURN_ERROR_STR("Out of memory when allocating node_util address");
                    return 1;
                }
                ret = shmem_runtime_get(i, "nodename", addr_cmp.addr, addr_len);
                if (ret != 0) {
                    RETURN_ERROR_MSG("Failed during nodename read from KVS: (%d)", ret);
                    return ret;
                }
                if (shmem_transport_same_node(&addr, &addr_cmp)) {
                    shmem_node_util_set_local_rank(i, n_local_pes++);
                }
                free(addr_cmp.addr);
            }
#ifdef USE_ON_NODE_COMMS
            if (n_local_pes <= 0) {
                RETURN_ERROR_STR("Failed to find any node-local PEs");
                return 1;
            }
#endif
            node_util_is_started = 1;
        } else {
            if (shmem_internal_params.DEBUG) {
                RAISE_WARN_STR("Called node utility startup routine more than once");
            }
        }
    } else {
        RAISE_ERROR_STR("Node utility has not initialized");
    }


    return 0;
}


int shmem_node_util_n_local_pes()
{
    return n_local_pes;
}

/* Store 'addr' field of shmem_transport_addr_t into 'dest_buf' */
void shmem_node_util_get_addr(int pe, void *dest_buf)
{
    int ret;
    shmem_transport_addr_t addr;

    if (pe == shmem_internal_my_pe) {
        dest_buf = shmem_transport_get_local_addr().addr;
        return;
    } else {
        if (node_util_is_started) {
            ret = shmem_runtime_get(pe, "nodename", &addr, sizeof(shmem_transport_addr_t));
            if (ret != 0) {
                RAISE_ERROR_MSG("Get nodename from KVS failed: (%d)", ret);
            }
            dest_buf = &addr.addr;
        } else {
            RAISE_ERROR_STR("Node utility has not started");
        }
        return;
    }
}
