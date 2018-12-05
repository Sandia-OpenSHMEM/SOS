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
#include "shmem_node_util.h"
#include "shmem_env.h"
#include "uthash.h"

/* Note: Increase MAX_KV_COUNT if more key/values are needed.  MAX_KV_COUNT is
 * 2 * the number of key/value pairs. */
#define MAX_KV_COUNT 20
#define MAX_KV_LENGTH 64

static int rank = -1;
static int size = 0;
static MPI_Comm SHMEM_RUNTIME_WORLD, SHMEM_RUNTIME_SHARED;
static int kv_length = 0;
static int initialized_mpi = 0;
static int initialized_node_util = 0;
static int local_rank = 0;
static int local_size = 1;
static int *local_ranks;

char* kv_store_me;
char* kv_store_all;

static inline char*
kv_index(char* kv_set, int index)
{
    return kv_set + index * MAX_KV_LENGTH;
}

int
shmem_runtime_init(void)
{
    int initialized, mpi_thread_level, provided;
    if (MPI_SUCCESS != MPI_Initialized(&initialized)) {
        return 1;
    }
    if (!initialized) {

        if (shmem_internal_params.MPI_THREAD_LEVEL == NULL ||
            strcmp(shmem_internal_params.MPI_THREAD_LEVEL, "MPI_THREAD_SINGLE") == 0) {
            mpi_thread_level = MPI_THREAD_SINGLE;
        }
        else if (strcmp(shmem_internal_params.MPI_THREAD_LEVEL, "MPI_THREAD_FUNNELED") == 0) {
            mpi_thread_level = MPI_THREAD_FUNNELED;
        }
        else if (strcmp(shmem_internal_params.MPI_THREAD_LEVEL, "MPI_THREAD_SERIALIZED") == 0) {
            mpi_thread_level = MPI_THREAD_SERIALIZED;
        }
        else if (strcmp(shmem_internal_params.MPI_THREAD_LEVEL, "MPI_THREAD_MULTIPLE") == 0) {
            mpi_thread_level = MPI_THREAD_MULTIPLE;
        }
        else {
            RETURN_ERROR_MSG("Unrecognized MPI thread level '%s'\n",
                             shmem_internal_params.MPI_THREAD_LEVEL);
            return 2;
        }

        if (MPI_SUCCESS != MPI_Init_thread(NULL, NULL, mpi_thread_level, &provided)) {
            return 3;
        }
        if (provided != mpi_thread_level) {
            RETURN_ERROR_MSG("MPI init with thread level '%s' returned %d, required %d\n",
                             shmem_internal_params.MPI_THREAD_LEVEL,
                             provided, mpi_thread_level);
            return 4;
        }
        initialized_mpi = 1;
    }
    if (MPI_SUCCESS != MPI_Comm_dup(MPI_COMM_WORLD, &SHMEM_RUNTIME_WORLD)) {
        return 5;
    }
    if (MPI_SUCCESS != MPI_Comm_set_errhandler(SHMEM_RUNTIME_WORLD, MPI_ERRORS_ARE_FATAL)) {
        return 6;
    }

    MPI_Comm_rank(SHMEM_RUNTIME_WORLD, &rank);
    MPI_Comm_size(SHMEM_RUNTIME_WORLD, &size);

    kv_store_me = (char*)malloc(MAX_KV_COUNT * sizeof(char)* MAX_KV_LENGTH);

    if (NULL == kv_store_me) {
        return 8;
    }

    return 0;
}

int
shmem_runtime_fini(void)
{
    int ret = MPI_SUCCESS;
    int finalized = 0;

    if (initialized_node_util) {
        MPI_Comm_free(&SHMEM_RUNTIME_SHARED);
        free(local_ranks);
    }

    MPI_Comm_free(&SHMEM_RUNTIME_WORLD);

    MPI_Finalized(&finalized);

    if (initialized_mpi) {
        if (finalized) {
            RAISE_WARN_STR("MPI was already finalized");
        } else {
            ret = MPI_Finalize();
        }
        initialized_mpi = 0;
    }

    free(kv_store_all);
    if (size != 1) {
        free(kv_store_me);
    }

    return ret != MPI_SUCCESS;
}

void
shmem_runtime_abort(int exit_code, const char msg[])
{

#ifdef HAVE___BUILTIN_TRAP
    if (shmem_internal_params.TRAP_ON_ABORT)
        __builtin_trap();
#endif

    fprintf(stderr, "%s\n", msg);

    if (size == 1) {
        exit(exit_code);
    }

    int ret = MPI_Abort(SHMEM_RUNTIME_WORLD, exit_code);

    RAISE_WARN_MSG("MPI_Abort returned (%d)\n", ret);

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
    }

    shmem_internal_assert(size > 0 && pe < size && pe >= 0);
    if (local_ranks[pe] != MPI_UNDEFINED) {
        return local_ranks[pe];
    } else {
        return -1;
    }
}

int
shmem_runtime_get_local_size(void)
{
    return local_size;
}

int
shmem_runtime_exchange(int need_node_util)
{
    if (size == 1) {
        local_size = 1;
        kv_store_all = kv_store_me;
        return 0;
    }

    if (need_node_util) {
        MPI_Group world_group, local_group;

        MPI_Comm_split_type(SHMEM_RUNTIME_WORLD, MPI_COMM_TYPE_SHARED, rank, MPI_INFO_NULL, &SHMEM_RUNTIME_SHARED);

        MPI_Comm_rank(SHMEM_RUNTIME_SHARED, &local_rank);
        MPI_Comm_size(SHMEM_RUNTIME_SHARED, &local_size);

        MPI_Comm_group(SHMEM_RUNTIME_WORLD, &world_group);
        MPI_Comm_group(SHMEM_RUNTIME_SHARED, &local_group);

        int *world_ranks = malloc(size * sizeof(int));
        local_ranks = malloc(size * sizeof(int));

        for (int i=0; i<size; i++) {
            world_ranks[i] = i;
        }

        MPI_Group_translate_ranks(world_group, size, world_ranks, local_group, local_ranks);

        MPI_Group_free(&world_group);
        MPI_Group_free(&local_group);
        free(world_ranks);

        initialized_node_util = 1;
    }

    int chunkSize = kv_length * sizeof(char) * MAX_KV_LENGTH;

    kv_store_all = (char*)malloc(chunkSize * size);

    if (NULL == kv_store_all) {
        return 1;
    }

    MPI_Allgather(kv_store_me, chunkSize, MPI_CHAR, kv_store_all, chunkSize,
                  MPI_CHAR, SHMEM_RUNTIME_WORLD);

    return 0;
}

int
shmem_runtime_put(char *key, void *value, size_t valuelen)
{
    if (kv_length < MAX_KV_COUNT) {
        memcpy(kv_index(kv_store_me, kv_length), key, MAX_KV_LENGTH);
        kv_length++;
        memcpy(kv_index(kv_store_me, kv_length), value, MAX_KV_LENGTH);
        kv_length++;
    }
    else {
        return 1;
    }

    return 0;
}

int
shmem_runtime_get(int pe, char *key, void *value, size_t valuelen)
{
    int flag = 0;
    for (int i = pe * kv_length; i < kv_length * size; i+= 2) {
        if (strcmp(kv_index(kv_store_all, i), key) == 0) {
            memcpy(value, kv_index(kv_store_all, i+1), valuelen);
            flag = 1;
            break;
        }
    }
    if (0 == flag) {
        return 1;
    }

    return 0;
}

void
shmem_runtime_barrier(void)
{
    MPI_Barrier(SHMEM_RUNTIME_WORLD);
}
