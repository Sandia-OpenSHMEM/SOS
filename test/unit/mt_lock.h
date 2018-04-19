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

/* MTL -- MultiThreaded Lock library for OpenSHMEM
 *
 * Wraps the OpenSHMEM lock API, allowing multiple threads at each PE to
 * request the same SHMEM lock concurrently. */

#ifndef MT_LOCK_H
#define MT_LOCK_H

#include "shmem.h"

void mtl_clear_lock(long *lockp);
void mtl_set_lock(long *lockp);
int mtl_test_lock(long *lockp);

/* When finished using MTL, the following routine can be used to free memory
 * used by the library. */
void mtl_cleanup(void);

#endif /* #ifndef MT_LOCK_H */
