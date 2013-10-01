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

#include "shmem_internal.h"
#include "shmem_accessibility.h"


#define FC_SHMEM_PE_ACCESSIBLE FC_FUNC_(shmem_pe_accessible, SHMEM_PE_ACCESSIBLE)
fortran_integer_t FC_SHMEM_PE_ACCESSIBLE(fortran_integer_t *pe);
fortran_integer_t
FC_SHMEM_PE_ACCESSIBLE(fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

     return shmem_internal_pe_accessible(*pe);
}


#define FC_SHMEM_ADDR_ACCESSIBLE FC_FUNC_(shmem_addr_accessible, SHMEM_ADDR_ACCESSIBLE)
fortran_integer_t FC_SHMEM_ADDR_ACCESSIBLE(void *addr, fortran_integer_t *pe);
fortran_integer_t
FC_SHMEM_ADDR_ACCESSIBLE(void *addr, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

     return shmem_internal_addr_accessible(addr, *pe);
}
