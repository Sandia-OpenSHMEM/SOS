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
 */

#include "config.h"

#include <stdlib.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_collectives.h"
#include "shmem_synchronization.h"


void* dlmalloc(size_t);
void  dlfree(void*);
void* dlrealloc(void*, size_t);
void* dlmemalign(size_t, size_t);


#define FC_SHPALLOC FC_FUNC_(shpalloc, SHPALLOC)
void FC_SHPALLOC(void **addr, fortran_integer_t *length, fortran_integer_t *errcode, fortran_integer_t *want_abort);
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
            fprintf(stderr, "[%03d] ERROR: shpalloc failure (zero length).  Aborting job.\n",
                    shmem_internal_my_pe);
            exit(1);
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
            fprintf(stderr, "[%03d] ERROR: shpalloc failure.  Aborting job.\n",
                    shmem_internal_my_pe);
            exit(1);
        }
    }

    shmem_internal_barrier_all();
}


#define FC_SHPDEALLOC FC_FUNC_(shpdeallc, SHPDEALLOC)
void FC_SHPDEALLOC(void **addr, fortran_integer_t *errcode, fortran_integer_t *want_abort);
void
FC_SHPDEALLOC(void **addr, fortran_integer_t *errcode, fortran_integer_t *want_abort)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    
    if (*errcode != 0) {
        if (0 == *want_abort) {
            return;
        } else {
            fprintf(stderr, "[%03d] ERROR: shpalloc failure (invalid length).  Aborting job.\n",
                    shmem_internal_my_pe);
            exit(1);
        }
    }

    SHMEM_MUTEX_LOCK(shmem_internal_mutex_alloc);
    dlfree(*addr);
    SHMEM_MUTEX_UNLOCK(shmem_internal_mutex_alloc);
    *errcode = 0;

    shmem_internal_barrier_all();
}


#define FC_SHPCLMOVE FC_FUNC_(shpclmove, SHPCLMOVE)
void FC_SHPCLMOVE(void **addr, fortran_integer_t *length, fortran_integer_t *errcode, fortran_integer_t *want_abort);
void
FC_SHPCLMOVE(void **addr, fortran_integer_t *length, fortran_integer_t *errcode, fortran_integer_t *want_abort)
{
    void *ret;

    SHMEM_ERR_CHECK_INITIALIZED();

    if (*length <= 0) {
        if (0 == *want_abort) {
            fprintf(stderr, "[%03d] shpclmove failure (invalid length)\n",
                    shmem_internal_my_pe);
            abort();
        } else {
            *errcode = -1;
            return;
        }
    }

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
            fprintf(stderr, "[%03d] shpclmove failure\n",
                    shmem_internal_my_pe);
            abort();
        } else {
            *errcode = -2;
            return;
        }
    }

    shmem_internal_barrier_all();
}


