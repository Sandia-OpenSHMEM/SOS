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

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "transport.h"
#include "shmem_synchronization.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_pcntr_get_pending_put = pshmem_pcntr_get_pending_put
#define shmem_pcntr_get_pending_put pshmem_pcntr_get_pending_put

#pragma weak shmem_pcntr_get_pending_get = pshmem_pcntr_get_pending_get
#define shmem_pcntr_get_pending_get pshmem_pcntr_get_pending_get

#endif /* ENABLE_PROFILING */

SHMEM_FUNCTION_ATTRIBUTES void
shmem_pcntr_get_pending_put(shmem_ctx_t ctx, uint64_t *cntr_value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    *cntr_value = shmem_transport_get_pending_put_cntr((shmem_transport_ctx_t *) ctx);
    return;
}

SHMEM_FUNCTION_ATTRIBUTES void
shmem_pcntr_get_pending_get(shmem_ctx_t ctx, uint64_t *cntr_value)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    *cntr_value = shmem_transport_get_pending_get_cntr((shmem_transport_ctx_t *) ctx);
    return;
}

