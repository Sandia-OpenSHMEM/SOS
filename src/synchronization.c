/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#include "config.h"

#include <portals4.h>
#include <portals4_runtime.h>
#include <stdlib.h>

#include "mpp/shmem.h"
#include "shmem_internal.h"


void
shmem_barrier(int PE_start, int logPE_stride, int PE_size, long *pSync)
{

}


void
shmem_barrier_all(void)
{
    shmem_quiet();
    runtime_barrier();
}


void
shmem_quiet(void)
{
    int ret;
    ptl_ct_event_t ct;

    /* wait for remote completion (acks) of all pending events */
    ret = PtlCTWait(source_ct_h, pending_counter, &ct);
    if (PTL_OK != ret) { abort(); }
    if (ct.failure != 0) { abort(); }
}


void
shmem_fence(void)
{
    /* intentionally a no-op */
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
