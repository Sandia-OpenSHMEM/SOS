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
#include "shmem.h"
#include "shmemx.h"
#include "transport.h"

#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmemx_domain_create = pshmemx_domain_create
#define shmemx_domain_create pshmemx_domain_create
#pragma weak shmemx_domain_destroy = pshmemx_domain_destroy
#define shmemx_domain_destroy pshmemx_domain_destroy

#pragma weak shmemx_ctx_create = pshmemx_ctx_create
#define shmemx_ctx_create pshmemx_ctx_create
#pragma weak shmemx_ctx_destroy = pshmemx_ctx_destroy
#define shmemx_ctx_destroy pshmemx_ctx_destroy

#pragma weak shmemx_ctx_fence = pshmemx_ctx_fence
#define shmemx_ctx_fence pshmemx_ctx_fence
#pragma weak shmemx_ctx_quiet = pshmemx_ctx_quiet
#define shmemx_ctx_quiet pshmemx_ctx_quiet

#endif /* ENABLE_PROFILING */

int shmemx_domain_create(int thread_level, int num_domains, shmemx_domain_t domains[])
{
    return shmem_transport_domain_create(thread_level, num_domains, domains);
}

void shmemx_domain_destroy(int num_domains, shmemx_domain_t domains[])
{
    shmem_transport_domain_destroy(num_domains, domains);
}

int shmemx_ctx_create(shmemx_domain_t domain, shmemx_ctx_t *ctx)
{
    return shmem_transport_ctx_create(domain, ctx);
}

void shmemx_ctx_destroy(shmemx_ctx_t ctx)
{
    shmem_transport_ctx_destroy(ctx);
}

void shmemx_ctx_fence(shmemx_ctx_t ctx)
{
    shmem_transport_ctx_fence(ctx);
}

void shmemx_ctx_quiet(shmemx_ctx_t ctx)
{
    shmem_transport_ctx_quiet(ctx);
}

