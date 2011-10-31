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
#ifdef PMI_SLURM
#include <slurm/pmi.h>
#else
#include <pmi.h>
#endif

#include "runtime.h"

static int rank = -1;
static int size = 0;
static ptl_process_t *mapping = NULL;
static ptl_handle_ni_t phys_ni_h;

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
shmem_internal_runtime_init(void)
{
    PMI_BOOL initialized;
    int i, ret, max_name_len, max_key_len, max_val_len;
    char *name, *key, *val;
    ptl_process_t my_id;

    if (PMI_SUCCESS != PMI_Initialized(&initialized)) {
        return 1;
    }

    if (PMI_TRUE != initialized) {
        if (PMI_SUCCESS != PMI_Init(&initialized)) {
            return 2;
        }
    }

    if (PMI_SUCCESS != PMI_KVS_Get_name_length_max(&max_name_len)) {
        return 6;
    }
    name = (char*) malloc(max_name_len);
    if (NULL == name) return 7;

    if (PMI_SUCCESS != PMI_KVS_Get_key_length_max(&max_key_len)) {
        return 1;
    }
    key = (char*) malloc(max_key_len);
    if (NULL == key) return 1;

    if (PMI_SUCCESS != PMI_KVS_Get_value_length_max(&max_val_len)) {
        return 1;
    }
    val = (char*) malloc(max_val_len);
    if (NULL == val) return 1;

    if (PMI_SUCCESS != PMI_Get_rank(&rank)) {
        return 3;
    }

    if (PMI_SUCCESS != PMI_Get_size(&size)) {
        return 4;
    }

    ret = PtlNIInit(PTL_IFACE_DEFAULT,
                    PTL_NI_NO_MATCHING | PTL_NI_PHYSICAL,
                    PTL_PID_ANY,
                    NULL,
                    NULL,
                    &phys_ni_h);
    if (PTL_OK != ret) return 1;

    ret = PtlGetId(phys_ni_h, &my_id);
    if (PTL_OK != ret) return 1;

    if (PMI_SUCCESS != PMI_KVS_Get_my_name(name, max_name_len)) {
        return 7;
    }

    /* put my information */
    snprintf(key, max_key_len, "shmem-%lu-nid", (long unsigned) rank);
    if (0 != encode(&my_id.phys.nid, sizeof(my_id.phys.nid), val, 
                    max_val_len)) {
        return 1;
    }
    if (PMI_SUCCESS != PMI_KVS_Put(name, key, val)) {
        return 8;
    }

    snprintf(key, max_key_len, "shmem-%lu-pid", (long unsigned) rank);
    if (0 != encode(&my_id.phys.pid, sizeof(my_id.phys.pid), val, 
                    max_val_len)) {
        return 1;
    }
    if (PMI_SUCCESS != PMI_KVS_Put(name, key, val)) {
        return 8;
    }

    if (PMI_SUCCESS != PMI_KVS_Commit(name)) {
        return 5;
    }

    if (PMI_SUCCESS != PMI_Barrier()) {
        return 5;
    }

    /* get everyone's information */
    mapping = malloc(sizeof(ptl_process_t) * size);
    if (NULL == mapping) return 9;

    for (i = 0 ; i < size ; ++i) {
        snprintf(key, max_key_len, "shmem-%lu-nid", (long unsigned) i);
        if (PMI_SUCCESS != PMI_KVS_Get(name, key, val, max_val_len)) {
            return 1;
        }
        if (0 != decode(val, &mapping[i].phys.nid, 
                        sizeof(mapping[i].phys.nid))) {
            return 1;
        }

        snprintf(key, max_key_len, "shmem-%lu-pid", (long unsigned) i);
        if (PMI_SUCCESS != PMI_KVS_Get(name, key, val, max_val_len)) {
            return 1;
        }
        if (0 != decode(val, &mapping[i].phys.pid,
                        sizeof(mapping[i].phys.pid))) {
            return 1;
        }
    }

    return 0;
}

int
shmem_internal_runtime_fini(void)
{
    if (NULL != mapping) free(mapping);

    PtlNIFini(phys_ni_h);

    PMI_Finalize();

    return 0;
}

ptl_process_t*
shmem_internal_runtime_get_mapping(void)
{
    return mapping;
}


int
shmem_internal_runtime_get_rank(void)
{
    return rank;
}


int
shmem_internal_runtime_get_size(void)
{
    return size;
}


void
shmem_internal_runtime_barrier(void)
{
    PMI_Barrier();
}
