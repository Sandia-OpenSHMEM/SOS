/* -*- C -*-
 *
 * Copyright (c) 2022 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef SHMEM_SPACE_H
#define SHMEM_SPACE_H

#include <shmemx.h>

struct shmem_internal_space_t {
    void *                                 base_addr;
    shmemx_space_config_t                  config;
};
typedef struct shmem_internal_space_t shmem_internal_space_t;

extern shmem_internal_space_t shmem_internal_space_default;

/* Space Management Routines */

int shmem_internal_space_init(void);
void shmem_internal_space_fini(void);

#endif
