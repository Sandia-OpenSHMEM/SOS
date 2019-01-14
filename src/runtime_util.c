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
#include <limits.h>

#include "shmem_internal.h"
#include "runtime.h"

#ifdef MAXHOSTNAMELEN
#define MAX_HOSTNAME_LEN MAXHOSTNAMELEN
#else
#define MAX_HOSTNAME_LEN HOST_NAME_MAX
#endif


/* Put the hostname into the runtime KVS */
int shmem_runtime_util_put_hostname(void)
{
    char hostname[MAX_HOSTNAME_LEN+1];
    int ret;

    ret = gethostname(hostname, MAX_HOSTNAME_LEN);
    if (ret != 0) {
        RETURN_ERROR_MSG("gethostname failed (%d)", ret);
        return ret;
    }

    /* gethostname() doesn't guarantee null-termination, add NIL */
    hostname[MAX_HOSTNAME_LEN] = '\0';

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

    return 0;
}


/* Populate the topology array.  This function should only be called after
 * shmem_runtime_util_put_hostname and a subsequent runtime exchange. */
int shmem_runtime_util_populate_local(int *location_array, int size, int *local_size)
{
    int ret, i, n_local_pes = 0;
    char hostname[MAX_HOSTNAME_LEN+1];

    ret = gethostname(hostname, MAX_HOSTNAME_LEN);
    if (ret != 0) {
        RETURN_ERROR_MSG("gethostname failed (%d)", ret);
        return ret;
    }

    /* gethostname() doesn't guarantee null-termination, add NIL */
    hostname[MAX_HOSTNAME_LEN] = '\0';

    for (i = 0; i < size; i++) {
        char peer_hostname[MAX_HOSTNAME_LEN+1];
        size_t hlen;

        ret = shmem_runtime_get(i, "hostname_len", &hlen, sizeof(size_t));
        if (ret != 0) {
            RETURN_ERROR_MSG("Failed during hostname_len read from KVS (%d)", ret);
            return ret;
        }

        ret = shmem_runtime_get(i, "hostname", peer_hostname, hlen);
        if (ret != 0) {
            RETURN_ERROR_MSG("Failed during hostname read from KVS (%d)", ret);
            return ret;
        }
        if (strncmp(hostname, peer_hostname, hlen) == 0) {
            location_array[i] = n_local_pes;
            n_local_pes++;
        }
        else
            location_array[i] = -1;
    }

    if (n_local_pes < 1 || n_local_pes > size) {
        RETURN_ERROR_MSG("Invalid local size (%d)\n", n_local_pes);
        return 1;
    }

    *local_size = n_local_pes;

    return 0;
}
