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


#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <mpi.h>

#include "runtime.h"
#include "shmem_internal.h"
#include "shmem_env.h"
#include "uthash.h"

#define MAX_KV_COUNT 20 //if more key/values are needed for future features, change this here (should be 2 * the number of keys)
#define MAX_KV_LENGTH 64

static int rank = -1;
static int size = 0;
static MPI_Comm SHMEM_RUNTIME_WORLD;
static int kv_length = 0;
static int to_finalize = 0;

char* kv_store_me;
char* kv_store_all;

char* 
kv_index(char* kv_set, int index)
{
    return kv_set + index * MAX_KV_LENGTH;
}

int
shmem_runtime_init(void)
{
    int initialized;
    if (MPI_SUCCESS != MPI_Initialized(&initialized)) {
        return 1;
    }
    if (!initialized) {
        int provided = 0;
        if(MPI_SUCCESS != MPI_Init_thread(NULL, NULL, shmem_internal_params.SHMEM_MPI_THREAD_LEVEL, &provided)){
            return 4;
        }
        if(provided != shmem_internal_params.SHMEM_MPI_THREAD_LEVEL){
            return -1;
        }
        to_finalize = 1;
    }
    if (MPI_SUCCESS != MPI_Comm_dup(MPI_COMM_WORLD, &SHMEM_RUNTIME_WORLD)){
    	return 5;
    }
    if (MPI_SUCCESS !=  MPI_Comm_rank(SHMEM_RUNTIME_WORLD, &rank)) {
        return 6;
    }

    if (MPI_SUCCESS != MPI_Comm_size(SHMEM_RUNTIME_WORLD, &size)) {
        return 7;
    }

    kv_store_me = (char*)malloc(MAX_KV_COUNT * sizeof(char)* MAX_KV_LENGTH);
    
    if (NULL == kv_store_me){
        return 8;
    }
    
    return 0;
}

int
shmem_runtime_fini(void)
{
    int finalized = 0;
    MPI_Finalized(&finalized);
    if (!finalized && to_finalize){
        MPI_Finalize();
    }

    free(kv_store_all);
    if (size != 1){
        free(kv_store_me);
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

    if (size == 1) {
        fprintf(stderr, "%s\n", msg);
        exit(exit_code);
    }

    MPI_Abort(SHMEM_RUNTIME_WORLD, exit_code);

    /* MPI_Abort should not return */
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
shmem_runtime_exchange(void)
{
    if (size == 1){
        kv_store_all = kv_store_me;
        return 0;
    }

    int chunkSize = kv_length * sizeof(char) * MAX_KV_LENGTH;
    
    kv_store_all = (char*)malloc(chunkSize * size);
    
    if (NULL == kv_store_all){
        return 9;
    }

    if (MPI_SUCCESS != MPI_Allgather(kv_store_me, chunkSize, MPI_CHAR, kv_store_all, chunkSize, MPI_CHAR, SHMEM_RUNTIME_WORLD)) {
        return 10;   
    }
    if (MPI_SUCCESS != MPI_Barrier(SHMEM_RUNTIME_WORLD)) {
        return 11;
    }
    return 0;
}

int 
shmem_runtime_put(char *key, void *value, size_t valuelen)
{
    if (kv_length < MAX_KV_COUNT){
        memcpy(kv_index(kv_store_me, kv_length), key, MAX_KV_LENGTH);
        kv_length++;
        memcpy(kv_index(kv_store_me, kv_length), value, MAX_KV_LENGTH);
        kv_length++;
    } 
    else {
        return MAX_KV_COUNT;
    }

    return 0;
}

int
shmem_runtime_get(int pe, char *key, void *value, size_t valuelen)
{
    int flag = 0;
    for (int i = pe * kv_length; i < kv_length * size; i+= 2){
        if (strcmp(kv_index(kv_store_all, i), key) == 0){
            memcpy(value, kv_index(kv_store_all, i+1), valuelen);
            flag = 1;
            break;
        }
    }
    if (0 == flag){
        return 12;
    }
    
    return 0;
}

void
shmem_runtime_barrier(void)
{
    MPI_Barrier(SHMEM_RUNTIME_WORLD); //if not finalized, call barrier??
}
