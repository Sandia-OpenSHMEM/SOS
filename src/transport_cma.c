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
#include "shmem_comm.h"
#include "shmem_node_util.h"
#include "runtime.h"

pid_t shmem_transport_cma_my_pid;
pid_t *shmem_transport_cma_peers = NULL;

typedef struct pmi_shmem_data {
    pid_t           lpid;   /* OS specific */
} pmi_cma_data_t;


int
shmem_transport_cma_init(void)
{
    int ret;
    pmi_cma_data_t cma_data;

    shmem_transport_cma_my_pid = cma_data.lpid = getpid();

    /* Share information */
    ret = shmem_runtime_put("cma-procid", &cma_data, sizeof(pmi_cma_data_t));
    if (0 != ret) {
        RETURN_ERROR_MSG("runtime_put failed: %d\n", ret);
    }

    return ret;
}


int
shmem_transport_cma_startup(void)
{
    int i, ret, peer_num, num_on_node = 0;
    pmi_cma_data_t cma_data;

    for (i = 0 ; i < shmem_internal_num_pes; ++i) {
        if (-1 != shmem_node_util_get_rank_same_node(i)) {
            num_on_node++;
        }
    }

    if (num_on_node > 255) {
        RETURN_ERROR_STR("Too many local ranks for CMA transport");
        return 1;
    }

    /* allocate space for local peers */
    shmem_transport_cma_peers = calloc(num_on_node, sizeof(pid_t));
    if (NULL == shmem_transport_cma_peers) return 1;

    /* get local peer pids */
    for (i = 0 ; i < shmem_internal_num_pes; ++i) {
        peer_num = shmem_node_util_get_rank_same_node(i);
        if (-1 == peer_num) continue;

        ret = shmem_runtime_get(i, "cma-procid", &cma_data,
                                sizeof(pmi_cma_data_t));
        if (0 != ret) {
            RETURN_ERROR_MSG("runtime_get failed: %d\n", ret);
            return 1;
        }

        /* update the connectivity map... */
        shmem_transport_cma_peers[peer_num] = cma_data.lpid;
    }

    return 0;
}

int
shmem_transport_cma_fini(void)
{
    if (NULL != shmem_transport_cma_peers) {
        free(shmem_transport_cma_peers);
    }

    return 0;
}
