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
static int size = 0, local_size = 0;
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

static int
encode(const void *inval, int invallen, char *outval, int outvallen)
{
    static unsigned char encodings[] = {
        '0','1','2','3','4','5','6','7', \
        '8','9','a','b','c','d','e','f' };
    int i;

    if (invallen * 2 + 1 > outvallen) {
        return 1;
    }

    for (i = 0; i < invallen; i++) {
        outval[2 * i] = encodings[((unsigned char *)inval)[i] & 0xf];
        outval[2 * i + 1] = encodings[((unsigned char *)inval)[i] >> 4];
    }

    outval[invallen * 2] = '\0';

    return 0;
}


static int
decode(const char *inval, void *outval, size_t outvallen)
{
    size_t i;
    char *ret = (char*) outval;

    if (outvallen != strlen(inval) / 2) {
        return 1;
    }

    for (i = 0 ; i < outvallen ; ++i) {
        if (*inval >= '0' && *inval <= '9') {
            ret[i] = *inval - '0';
        } else {
            ret[i] = *inval - 'a' + 10;
        }
        inval++;
        if (*inval >= '0' && *inval <= '9') {
            ret[i] |= ((*inval - '0') << 4);
        } else {
            ret[i] |= ((*inval - 'a' + 10) << 4);
        }
        inval++;
    }

    return 0;
}


int
shmem_runtime_init(int enable_local_ranks)
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

        if (enable_local_ranks) {
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
shmem_runtime_get_local_rank(int pe)
{
    if (size == 1) {
        return 0;
    } else {
        return location_array[pe];
    }
}


int
shmem_runtime_get_local_size(void)
{
    if (size == 1) {
        return 1;
    } else {
        return local_size;
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
    if (0 != encode(value, valuelen, kvs_value, max_val_len)) {
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
    if (0 != decode(kvs_value, value, valuelen)) {
        return 2;
    }

    return 0;
}


void
shmem_runtime_barrier(void)
{
    PMI_Barrier();
}
