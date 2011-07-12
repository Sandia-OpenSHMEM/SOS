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

#ifndef SHMEM_INTERNAL_RUNTIME_H
#define SHMEM_INTERNAL_RUNTIME_H

int shmem_internal_runtime_init(void);
int shmem_internal_runtime_fini(void);
ptl_process_t* shmem_internal_runtime_get_mapping(void);
int shmem_internal_runtime_get_rank(void);
int shmem_internal_runtime_get_size(void);
void shmem_internal_runtime_barrier(void);

#endif
