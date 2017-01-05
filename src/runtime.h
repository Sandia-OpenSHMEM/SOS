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

#ifndef SHMEM_INTERNAL_RUNTIME_H
#define SHMEM_INTERNAL_RUNTIME_H

#include "shmem_decl.h"

int shmem_runtime_init(void);
int shmem_runtime_fini(void);
void shmem_runtime_abort(int exit_code, const char msg[]) __shmem_attribute_noreturn__ ;

int shmem_runtime_get_rank(void);
int shmem_runtime_get_size(void);

int shmem_runtime_exchange(void);
int shmem_runtime_put(char *key, void *value, size_t valuelen);
int shmem_runtime_get(int pe, char *key, void *value, size_t valuelen);

void shmem_runtime_barrier(void);

#endif
