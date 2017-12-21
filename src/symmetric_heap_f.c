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

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_collectives.h"
#include "shmem_synchronization.h"


void* dlmalloc(size_t);
void  dlfree(void*);
void* dlrealloc(void*, size_t);
void* dlmemalign(size_t, size_t);


#define FC_SHPALLOC FC_FUNC_(shpalloc, SHPALLOC)
void SHMEM_FUNCTION_ATTRIBUTES FC_SHPALLOC(void **addr, fortran_integer_t *length, fortran_integer_t *errcode, fortran_integer_t *want_abort);
void
FC_SHPALLOC(void **addr, fortran_integer_t *length, fortran_integer_t *errcode, fortran_integer_t *want_abort)
{
    size_t len;
    SHMEM_ERR_CHECK_INITIALIZED();

    len = ((size_t) *length) * 4;

    *errcode = 0;
    if (len == 0) {
        if (0 == *want_abort) {
            *errcode = -1;
            return;
        } else {
            RAISE_ERROR_STR("shpalloc failure (invalid length).  Aborting job.");
        }
    }

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    *addr = dlmalloc(len); /* length is number of 32 bit words */
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

    if (*addr == NULL) {
        if (0 == *want_abort) {
            *errcode = -2;
            return;
        } else {
            RAISE_ERROR_STR("shpalloc failure.  Aborting job.");
        }
    }

    shmem_internal_barrier_all();
}


#define FC_SHPDEALLOC FC_FUNC_(shpdeallc, SHPDEALLOC)
void SHMEM_FUNCTION_ATTRIBUTES FC_SHPDEALLOC(void **addr, fortran_integer_t *errcode, fortran_integer_t *want_abort);
void
FC_SHPDEALLOC(void **addr, fortran_integer_t *errcode, fortran_integer_t *want_abort)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC_HEAP(*addr);

    shmem_internal_barrier_all();

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    dlfree(*addr);
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);
    *errcode = 0;
}


#define FC_SHPCLMOVE FC_FUNC_(shpclmove, SHPCLMOVE)
void SHMEM_FUNCTION_ATTRIBUTES FC_SHPCLMOVE(void **addr, fortran_integer_t *length, fortran_integer_t *errcode, fortran_integer_t *want_abort);
void
FC_SHPCLMOVE(void **addr, fortran_integer_t *length, fortran_integer_t *errcode, fortran_integer_t *want_abort)
{
    void *ret;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_SYMMETRIC_HEAP(*addr);

    if (*length <= 0) {
        if (0 == *want_abort) {
            RAISE_ERROR_STR("shpclmove failure (invalid length)");
        } else {
            *errcode = -1;
            return;
        }
    }

    shmem_internal_barrier_all();

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    ret = dlrealloc(*addr, *length * 4); /* length is number of 32 bit words */
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);

    if (*addr != NULL) {
        if (*addr == ret) {
            *errcode = 0;
        } else {
            *errcode = 1;
        }
        *addr = ret;
    } else {
        if (0 == *want_abort) {
            RAISE_ERROR_STR("shpclmove failure");
        } else {
            *errcode = -2;
            return;
        }
    }

    shmem_internal_barrier_all();
}


