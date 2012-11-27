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

#include "config.h"

#include <portals4.h>
#include <stdio.h>
#include <stdlib.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"


#define FC_SHMEM_SWAP FC_FUNC_(shmem_swap, SHMEM_SWAP)
fortran_integer_t FC_SHMEM_SWAP(fortran_integer_t *target, 
				fortran_integer_t *value,
				fortran_integer_t *pe);
fortran_integer_t
FC_SHMEM_SWAP(fortran_integer_t *target,
	      fortran_integer_t *value, 
	      fortran_integer_t *pe)
{
    fortran_integer_t newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, value, &newval, SIZEOF_FORTRAN_INTEGER, 
			*pe, DTYPE_FORTRAN_INTEGER);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_INT4_SWAP FC_FUNC_(shmem_int4_swap, SHMEM_INT4_SWAP)
fortran_integer_kind_4__t FC_SHMEM_INT4_SWAP(fortran_integer_kind_4__t *target, 
				fortran_integer_kind_4__t *value,
				fortran_integer_t *pe);
fortran_integer_kind_4__t
FC_SHMEM_INT4_SWAP(fortran_integer_kind_4__t *target,
	      fortran_integer_kind_4__t *value, 
	      fortran_integer_t *pe)
{
    fortran_integer_kind_4__t newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, &value, &newval, SIZEOF_FORTRAN_INTEGER_KIND_4_, 
			*pe, DTYPE_FORTRAN_INTEGER_KIND_4_);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_INT8_SWAP FC_FUNC_(shmem_int8_swap, SHMEM_INT8_SWAP)
fortran_integer_kind_8__t FC_SHMEM_INT8_SWAP(fortran_integer_kind_8__t *target, 
				fortran_integer_kind_8__t *value,
				fortran_integer_t *pe);
fortran_integer_kind_8__t
FC_SHMEM_INT8_SWAP(fortran_integer_kind_8__t *target,
	      fortran_integer_kind_8__t *value, 
	      fortran_integer_t *pe)
{
    fortran_integer_kind_8__t newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, &value, &newval, SIZEOF_FORTRAN_INTEGER_KIND_8_, 
			*pe, DTYPE_FORTRAN_INTEGER_KIND_8_);
    shmem_internal_get_wait();
    return newval;
}


#define FC_SHMEM_REAL4_SWAP FC_FUNC_(shmem_real4_swap, SHMEM_REAL4_SWAP)
fortran_real_kind_4__t FC_SHMEM_REAL4_SWAP(fortran_real_kind_4__t *target, 
				fortran_real_kind_4__t *value,
				fortran_integer_t *pe);
fortran_real_kind_4__t
FC_SHMEM_REAL4_SWAP(fortran_real_kind_4__t *target,
	      fortran_real_kind_4__t *value, 
	      fortran_integer_t *pe)
{
    fortran_real_kind_4__t newval;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_swap(target, &value, &newval, SIZEOF_FORTRAN_REAL_KIND_4_, 
			*pe, DTYPE_FORTRAN_REAL_KIND_4_);
    shmem_internal_get_wait();
    return newval;
}
