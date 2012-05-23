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
 *
 * Run-time support for the built-in runtime that is part of the
 * shared memory implementation Portals
 */

#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <portals4.h>
#if defined(PMI_SLURM)
#include <slurm/pmi.h>
#elif defined(PMI_PORTALS4)
#include <portals4/pmi.h>
#else
#include <pmi.h>
#endif

#include "runtime.h"

static int rank = -1;
static int size = 0;
static char *kvs_name, *kvs_key, *kvs_value;
static int max_name_len, max_key_len, max_val_len;

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
    int i;
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
shmem_runtime_init(void)
{
    int initialized;

    if (PMI_SUCCESS != PMI_Initialized(&initialized)) {
        return 1;
    }

    if (!initialized) {
        if (PMI_SUCCESS != PMI_Init(&initialized)) {
            return 2;
        }
    }

    if (PMI_SUCCESS != PMI_KVS_Get_name_length_max(&max_name_len)) {
        return 3;
    }
    kvs_name = (char*) malloc(max_name_len);
    if (NULL == kvs_name) return 4;

    if (PMI_SUCCESS != PMI_KVS_Get_key_length_max(&max_key_len)) {
        return 5;
    }
    kvs_key = (char*) malloc(max_key_len);
    if (NULL == kvs_key) return 6;

    if (PMI_SUCCESS != PMI_KVS_Get_value_length_max(&max_val_len)) {
        return 7;
    }
    kvs_value = (char*) malloc(max_val_len);
    if (NULL == kvs_value) return 8;

    if (PMI_SUCCESS != PMI_KVS_Get_my_name(kvs_name, max_name_len)) {
        return 7;
    }

    if (PMI_SUCCESS != PMI_Get_rank(&rank)) {
        return 9;
    }

    if (PMI_SUCCESS != PMI_Get_size(&size)) {
        return 10;
    }

    return 0;
}


int
shmem_runtime_fini(void)
{
    PMI_Finalize();

    return 0;
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
shmem_runtime_exchange(void)
{
    if (PMI_SUCCESS != PMI_KVS_Commit(kvs_name)) {
        return 5;
    }

    if (PMI_SUCCESS != PMI_Barrier()) {
        return 5;
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
    if (PMI_SUCCESS != PMI_KVS_Put(kvs_name, kvs_key, kvs_value)) {
        return 2;
    }

    return 0;
}


int
shmem_runtime_get(int pe, char *key, void *value, size_t valuelen)
{
    snprintf(kvs_key, max_key_len, "shmem-%lu-%s", (long unsigned) pe, key);
    if (PMI_SUCCESS != PMI_KVS_Get(kvs_name, kvs_key, kvs_value, max_val_len)) {
        return 1;
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
