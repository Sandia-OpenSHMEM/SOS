/* -*- C -*-
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "transport_none.h"
#include "shmem.h"
#include "shmem_team.h"

shmem_transport_ctx_t shmem_transport_ctx_default;
shmem_ctx_t SHMEM_CTX_DEFAULT = (shmem_ctx_t) &shmem_transport_ctx_default;

int shmem_transport_init(void)
{
    shmem_transport_ctx_default.options = 0;
    shmem_transport_ctx_default.team    = &shmem_internal_team_world;

    return 0;
}
