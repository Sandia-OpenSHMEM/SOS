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
 * Wrappers to interface with PMI runtime
 */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#if defined(PMI_PORTALS4)
#include <portals4/pmi.h>
#else
#include <pmi.h>
#endif

#include "runtime.h"
#include "shmem_internal.h"
#include "uthash.h"

static int rank = -1;
static int size = 0, node_size = 0;
static char *kvs_name, *kvs_key, *kvs_value;
static int max_name_len, max_key_len, max_val_len;
static int initialized_pmi = 0;
static int *location_array = NULL;

#define SINGLETON_KEY_LEN 128
#define SINGLETON_VAL_LEN 256

typedef struct {
    char key[SINGLETON_KEY_LEN];
    char val[SINGLETON_VAL_LEN];
    UT_hash_handle hh;
} singleton_kvs_t;

singleton_kvs_t *singleton_kvs = NULL;

int
shmem_runtime_init(int enable_node_ranks)
{
    int initialized;

    if (PMI_SUCCESS != PMI_Initialized(&initialized)) {
        return 1;
    }

    if (!initialized) {
        if (PMI_SUCCESS != PMI_Init(&initialized)) {
            return 2;
        }
        else {
            initialized_pmi = 1;
        }
    }

    if (PMI_SUCCESS != PMI_Get_rank(&rank)) {
        return 3;
    }

    if (PMI_SUCCESS != PMI_Get_size(&size)) {
        return 4;
    }

    if (size > 1) {
        if (PMI_SUCCESS != PMI_KVS_Get_name_length_max(&max_name_len)) {
            return 5;
        }
        kvs_name = (char*) malloc(max_name_len);
        if (NULL == kvs_name) return 6;

        if (PMI_SUCCESS != PMI_KVS_Get_key_length_max(&max_key_len)) {
            return 7;
        }
        if (PMI_SUCCESS != PMI_KVS_Get_value_length_max(&max_val_len)) {
            return 8;
        }
        if (PMI_SUCCESS != PMI_KVS_Get_my_name(kvs_name, max_name_len)) {
            return 9;
        }

        if (enable_node_ranks) {
            location_array = malloc(sizeof(int) * size);
            if (NULL == location_array) return 10;
        }
    }
    else {
        /* Use a local KVS for singleton runs */
        max_key_len = SINGLETON_KEY_LEN;
        max_val_len = SINGLETON_VAL_LEN;
        kvs_name = NULL;
        max_name_len = 0;
    }

    kvs_key = (char*) malloc(max_key_len);
    if (NULL == kvs_key) return 11;

    kvs_value = (char*) malloc(max_val_len);
    if (NULL == kvs_value) return 12;

    return 0;
}


int
shmem_runtime_fini(void)
{
    if (location_array) {
        free(location_array);
    }

    if (initialized_pmi) {
        PMI_Finalize();
        initialized_pmi = 0;
    }

    return 0;
}


void
shmem_runtime_abort(int exit_code, const char msg[])
{

    shmem_util_backtrace();

#ifdef HAVE___BUILTIN_TRAP
    if (shmem_internal_params.TRAP_ON_ABORT)
        __builtin_trap();
#endif

    if (size == 1) {
        fprintf(stderr, "%s\n", msg);
        exit(exit_code);
    }

    PMI_Abort(exit_code, msg);

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
shmem_runtime_get_node_rank(int pe)
{
    shmem_internal_assert(pe < size && pe >= 0);

    if (size == 1) {
        return 0;
    } else {
        return location_array[pe];
    }
}


int
shmem_runtime_get_node_size(void)
{
    if (size == 1) {
        return 1;
    } else {
        return node_size;
    }
}


int
shmem_runtime_exchange(void)
{
    int ret;

    /* Use singleton KVS for single process jobs */
    if (size == 1)
        return 0;

    if (location_array) {
        ret = shmem_runtime_util_put_hostname();
        if (ret != 0) {
            RETURN_ERROR_MSG("KVS hostname put (%d)", ret);
            return 4;
        }
    }

    if (PMI_SUCCESS != PMI_KVS_Commit(kvs_name)) {
        return 5;
    }

    if (PMI_SUCCESS != PMI_Barrier()) {
        return 6;
    }

    if (location_array) {
        ret = shmem_runtime_util_populate_node(location_array, size, &node_size);
        if (0 != ret) {
            RETURN_ERROR_MSG("Node PE mapping failed (%d)\n", ret);
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

    if (size == 1) {
        singleton_kvs_t *e = malloc(sizeof(singleton_kvs_t));
        if (e == NULL) return 3;
        shmem_internal_assertp(max_key_len <= SINGLETON_KEY_LEN);
        shmem_internal_assertp(max_val_len <= SINGLETON_VAL_LEN);
        strncpy(e->key, kvs_key, max_key_len);
        strncpy(e->val, kvs_value, max_val_len);
        HASH_ADD_STR(singleton_kvs, key, e);
    } else {
        if (PMI_SUCCESS != PMI_KVS_Put(kvs_name, kvs_key, kvs_value)) {
            return 2;
        }
    }

    return 0;
}


int
shmem_runtime_get(int pe, char *key, void *value, size_t valuelen)
{
    snprintf(kvs_key, max_key_len, "shmem-%lu-%s", (long unsigned) pe, key);
    if (size == 1) {
        singleton_kvs_t *e;
        HASH_FIND_STR(singleton_kvs, kvs_key, e);
        if (e == NULL)
            return 3;
        kvs_value = e->val;
    }
    else {
        if (PMI_SUCCESS != PMI_KVS_Get(kvs_name, kvs_key, kvs_value, max_val_len)) {
            return 1;
        }
    }
    if (0 != shmem_runtime_util_decode(kvs_value, value, valuelen)) {
        return 2;
    }

    return 0;
}


void
shmem_runtime_barrier(void)
{
    PMI_Barrier();
}
