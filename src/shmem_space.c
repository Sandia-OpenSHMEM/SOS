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


#include "config.h"

#include "shmem_space.h"
#include "shmem_internal.h"

shmem_internal_space_t shmem_internal_space_default;
shmemx_space_t SHMEMX_SPACE_DEFAULT = (shmemx_space_t) &shmem_internal_space_default;

int shmem_space_init(void) {

  /* Initialize SHMEMX_SPACE_DEFAULT */
  shmem_internal_space_default.base_addr = shmem_internal_heap_base;
  (shmem_internal_space_default.config).space_size = shmem_internal_heap_length;
  SHMEMX_SPACE_DEFAULT = (shmemx_space_t) &shmem_internal_space_default;

  return 0;
}

void shmem_space_fini(void) {

}
