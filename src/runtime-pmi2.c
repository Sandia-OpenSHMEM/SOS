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
#include "shmem_node_util.h"

static int rank = -1;
static int size = 0;
static char *kvs_name, *kvs_key, *kvs_value;
static int max_name_len, max_key_len, max_val_len;
static int initialized_pmi = 0;
static int initialized_node_util = 0;
static uint32_t local_size;

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
decode(const char *inval, void *outval, int outvallen)
{
    size_t i;
    char *ret = (char*) outval;

    size_t outlen = strlen(inval) / 2;

    if (outvallen < outlen) {
        return 1;
    }

    for (i = 0 ; i < outlen; ++i) {
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
shmem_runtime_init(void)
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

    return 0;
}


int
shmem_runtime_fini(void)
{
    if (initialized_node_util) {
        shmem_node_util_fini();
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
shmem_runtime_get_local_rank(int pe)
{
    return shmem_node_util_get_local_rank(pe);
}


int
shmem_runtime_get_size(void)
{
    return size;
}


int
shmem_runtime_get_local_size(void)
{
    return local_size;
}


int
shmem_runtime_exchange(int need_node_util)
{
    if (need_node_util) {
        ret = shmem_node_util_init();
        if (ret != 0) {
            RAISE_ERROR_MSG("Node utility init failed (%d)", ret);
        } else {
            initialized_node_util = 1;
        }
    }

    if (PMI2_SUCCESS != PMI2_KVS_Fence()) {
        return 5;
    }

    if (need_node_util) {
        ret = shmem_node_util_startup();
        if (0 != ret) {
            RETURN_ERROR_MSG("Node utility startup failed (%d)\n", ret);
        }
        local_size = shmem_node_util_get_local_n_pes();
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
    if (0 != decode(kvs_value, value, valuelen)) {
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
