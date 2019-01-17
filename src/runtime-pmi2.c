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

/*
 * Wrappers to interface with PMI2 runtime
 */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if defined(PMI_PORTALS4)
#error "PMI_PORTALS4 doesn't support PMI2"
#else
#include <pmi2.h>
#endif

/* crazy cray pmi defined PMI2 functions but uses PMI_SUCCESS (0) */
#ifndef PMI2_SUCCESS
#define PMI2_SUCCESS 0
#endif

#include "runtime.h"
#include "shmem_internal.h"

static int rank = -1;
static int size = 0, local_size = 0;
static char *kvs_name, *kvs_key, *kvs_value;
static int max_name_len, max_key_len, max_val_len;
static int initialized_pmi = 0;
static int *location_array = NULL;


int
shmem_runtime_init(int enable_local_ranks)
{
    int spawned, appnum;

    if (!PMI2_Initialized()) {
        if (PMI2_SUCCESS != PMI2_Init(&spawned, &size, &rank, &appnum)) {
            return 2;
        }
        else {
            initialized_pmi = 1;
        }
    }

    max_name_len = PMI2_MAX_VALLEN;
    kvs_name = (char*) malloc(max_name_len);
    if (NULL == kvs_name) return 4;

    max_key_len = PMI2_MAX_KEYLEN;
    kvs_key = (char*) malloc(max_key_len);
    if (NULL == kvs_key) return 6;

    max_val_len = PMI2_MAX_VALLEN;
    kvs_value = (char*) malloc(max_val_len);
    if (NULL == kvs_value) return 8;

    if (PMI2_SUCCESS != PMI2_Job_GetId(kvs_name, max_name_len)) {
        return 7;
    }

    if (enable_local_ranks) {
        location_array = malloc(sizeof(int) * size);
        if (NULL == location_array) return 8;
    }

    return 0;
}


int
shmem_runtime_fini(void)
{
    if (location_array) {
        free(location_array);
    }

    if (initialized_pmi == 1) {
        PMI2_Finalize();
        initialized_pmi = 0;
    }
    return 0;
}


void
shmem_runtime_abort(int exit_code, const char msg[])
{

#ifdef HAVE___BUILTIN_TRAP
    if (shmem_internal_params.TRAP_ON_ABORT)
        __builtin_trap();
#endif

    shmem_util_backtrace();

    PMI2_Abort(exit_code, msg);

    /* PMI_Abort should not return */
    abort();
}


int
shmem_runtime_get_rank(void)
{
    return rank;
}


int
shmem_runtime_get_size(void)
{
    return size;
}


int
shmem_runtime_get_local_rank(int pe)
{
    return location_array[pe];
}


int
shmem_runtime_get_local_size(void)
{
    return local_size;
}


int
shmem_runtime_exchange(void)
{
    int ret;

    if (location_array) {
        ret = shmem_runtime_util_put_hostname();
        if (ret != 0) {
            RETURN_ERROR_MSG("KVS hostname put (%d)", ret);
            return 4;
        }
    }

    if (PMI2_SUCCESS != PMI2_KVS_Fence()) {
        return 5;
    }

    if (location_array) {
        ret = shmem_runtime_util_populate_local(location_array, size, &local_size);
        if (0 != ret) {
            RETURN_ERROR_MSG("Local PE mapping failed (%d)\n", ret);
            return 7;
        }
    }

    return 0;
}


int
shmem_runtime_put(char *key, void *value, size_t valuelen)
{
    snprintf(kvs_key, max_key_len, "shmem-%lu-%s", (long unsigned) rank, key);
    if (0 != shmem_runtime_util_encode(value, valuelen, kvs_value,
                                       max_val_len)) {
        return 1;
    }
    if (PMI2_SUCCESS != PMI2_KVS_Put(kvs_key, kvs_value)) {
        return 2;
    }

    return 0;
}

int
shmem_runtime_get(int pe, char *key, void *value, size_t valuelen)
{
    int len;

    snprintf(kvs_key, max_key_len, "shmem-%lu-%s", (long unsigned) pe, key);
    if (PMI2_SUCCESS != PMI2_KVS_Get(kvs_name, PMI2_ID_NULL,
                                     kvs_key, kvs_value, max_val_len, &len)) {
        return 1;
    }
    if (0 != shmem_runtime_util_decode(kvs_value, value, valuelen)) {
        return 2;
    }

    return 0;
}


/*
 * Note this isn't exactly a barrier in PMI2
 */
void
shmem_runtime_barrier(void)
{
    PMI2_KVS_Fence();
}
