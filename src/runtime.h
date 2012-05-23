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

int shmem_runtime_init(void);
int shmem_runtime_fini(void);

int shmem_runtime_get_rank(void);
int shmem_runtime_get_size(void);

int shmem_runtime_exchange(void);
int shmem_runtime_put(char *key, void *value, size_t valuelen);
int shmem_runtime_get(int pe, char *key, void *value, size_t valuelen);

void shmem_runtime_barrier(void);

#endif
