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

#include <stdio.h>
#include <stdlib.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_remote_pointer.h"


#ifdef ENABLE_PROFILING
#include "pshmem.h"

#pragma weak shmem_ptr = pshmem_ptr
#define shmem_ptr pshmem_ptr

#pragma weak shmem_team_ptr = pshmem_team_ptr
#define shmem_team_ptr pshmem_team_ptr

#endif /* ENABLE_PROFILING */


void SHMEM_FUNCTION_ATTRIBUTES *
shmem_ptr(const void *dest, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest, 1);

    return shmem_internal_ptr(dest, pe);
}

void SHMEM_FUNCTION_ATTRIBUTES *
shmem_team_ptr(shmem_team_t team, const void *dest, int pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest, 1);

    if (team == SHMEM_TEAM_INVALID) {
        return NULL;
    }

    int global_pe = shmem_team_translate_pe(team, pe, SHMEM_TEAM_WORLD);

    return shmem_internal_ptr(dest, global_pe);
}
