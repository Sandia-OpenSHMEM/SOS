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

#include "shmem_internal.h"
#include "transport.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmemx_pcntr_get_issued_write = pshmemx_pcntr_get_issued_write
#define shmemx_pcntr_get_issued_write pshmemx_pcntr_get_issued_write

#pragma weak shmemx_pcntr_get_issued_read = pshmemx_pcntr_get_issued_read
#define shmemx_pcntr_get_issued_read pshmemx_pcntr_get_issued_read

#pragma weak shmemx_pcntr_get_completed_write = pshmemx_pcntr_get_completed_write
#define shmemx_pcntr_get_completed_write pshmemx_pcntr_get_completed_write

#pragma weak shmemx_pcntr_get_completed_read = pshmemx_pcntr_get_completed_read
#define shmemx_pcntr_get_completed_read pshmemx_pcntr_get_completed_read

#pragma weak shmemx_pcntr_get_completed_target = pshmemx_pcntr_get_completed_target
#define shmemx_pcntr_get_completed_target pshmemx_pcntr_get_completed_target

#pragma weak shmemx_pcntr_get_all = pshmemx_pcntr_get_all
#define shmemx_pcntr_get_all pshmemx_pcntr_get_all

#endif /* ENABLE_PROFILING */

void SHMEM_FUNCTION_ATTRIBUTES 
shmemx_pcntr_get_issued_write(shmem_ctx_t ctx, uint64_t *cntr_value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    *cntr_value = shmem_transport_pcntr_get_issued_write((shmem_transport_ctx_t *) ctx);
    return;
}

void SHMEM_FUNCTION_ATTRIBUTES 
shmemx_pcntr_get_issued_read(shmem_ctx_t ctx, uint64_t *cntr_value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    *cntr_value = shmem_transport_pcntr_get_issued_read((shmem_transport_ctx_t *) ctx);
    return;
}

void SHMEM_FUNCTION_ATTRIBUTES 
shmemx_pcntr_get_completed_write(shmem_ctx_t ctx, uint64_t *cntr_value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    *cntr_value = shmem_transport_pcntr_get_completed_write((shmem_transport_ctx_t *) ctx);
    return;
}

void SHMEM_FUNCTION_ATTRIBUTES 
shmemx_pcntr_get_completed_read(shmem_ctx_t ctx, uint64_t *cntr_value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    *cntr_value = shmem_transport_pcntr_get_completed_read((shmem_transport_ctx_t *) ctx);
    return;
}

void SHMEM_FUNCTION_ATTRIBUTES
shmemx_pcntr_get_completed_target(uint64_t *cntr_value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    *cntr_value = shmem_transport_pcntr_get_completed_target();
    return;
}

void SHMEM_FUNCTION_ATTRIBUTES
shmemx_pcntr_get_all(shmem_ctx_t ctx, shmemx_pcntr_t *pcntr)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    shmem_transport_pcntr_get_all((shmem_transport_ctx_t *) ctx, pcntr);
    return;
}

