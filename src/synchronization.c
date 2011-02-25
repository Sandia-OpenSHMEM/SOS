/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include "shmem.h"
#include "shmem_internal.h"

#include <portals4.h>
#include <portals4_runtime.h>
#include <stdlib.h>
#include <unistd.h>


void
shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{

}


void
shmem_barrier_all(void)
{

}


void
shmem_quiet(void)
{

}


void
shmem_fence(void)
{

}

void
shmem_short_wait(short *var, short value)
{

}


void
shmem_short_wait_until(short *var, int cond, short value)
{

}


void
shmem_int_wait(int *var, int value)
{

}


void
shmem_int_wait_until(int *var, int cond, int value)
{

}


void
shmem_long_wait(long *var, long value)
{

}


void
shmem_long_wait_until(long *var, int cond, long value)
{

}


void
shmem_longlong_wait(long long *var, long long value)
{

}


void
shmem_longlong_wait_until(long long *var, int cond, long long value)
{

}


void
shmem_wait(long *ivar, long cmp_value)
{

}


void
shmem_wait_until(long *ivar, int cmp, long value)
{

}
