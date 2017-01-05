/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2016 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"

#define FC_SHMEM_CLEAR_CACHE_INV FC_FUNC_(shmem_clear_cache_inv, SHMEM_CLEAR_CACHE_INV)
void FC_SHMEM_CLEAR_CACHE_INV(void);
void
FC_SHMEM_CLEAR_CACHE_INV(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


#define FC_SHMEM_SET_CACHE_INV FC_FUNC(shmem_set_cache_inv, SHMEM_SET_CACHE_INV)
void FC_SHMEM_SET_CACHE_INV(void);
void
FC_SHMEM_SET_CACHE_INV(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


#define FC_SHMEM_CLEAR_CACHE_LINE_INV FC_FUNC(shmem_clear_cache_line_inv, SHMEM_CLEAR_CACHE_LINE_INV)
void FC_SHMEM_CLEAR_CACHE_LINE_INV(void *target);
void
FC_SHMEM_CLEAR_CACHE_LINE_INV(void *target)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


#define FC_SHMEM_SET_CACHE_LINE_INV FC_FUNC(shmem_set_cache_line_inv, SHMEM_SET_CACHE_LINE_INV)
void FC_SHMEM_SET_CACHE_LINE_INV(void *target);
void
FC_SHMEM_SET_CACHE_LINE_INV(void *target)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


#define FC_SHMEM_UDCFLUSH FC_FUNC(shmem_udcflush, SHMEM_UDCFLUSH)
void FC_SHMEM_UDCFLUSH(void);
void
FC_SHMEM_UDCFLUSH(void)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}


#define FC_SHMEM_UDCFLUSH_LINE FC_FUNC(shmem_udcflush_line, SHMEM_UDCFLUSH_LINE)
void FC_SHMEM_UDCFLUSH_LINE(void *target);
void
FC_SHMEM_UDCFLUSH_LINE(void *target)
{
    SHMEM_ERR_CHECK_INITIALIZED();

    /* Intentionally a no-op */
}
