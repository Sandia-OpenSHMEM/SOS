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

#include "config.h"

#include "shmem_internal.h"
#include "shmem_accessibility.h"


#define FC_SHMEM_PE_ACCESSIBLE FC_FUNC_(shmem_pe_accessible, SHMEM_PE_ACCESSIBLE)
fortran_integer_t FC_SHMEM_PE_ACCESSIBLE(fortran_integer_t *pe);
fortran_integer_t
FC_SHMEM_PE_ACCESSIBLE(fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();

     return shmem_internal_pe_accessible(*pe);
}


#define FC_SHMEM_ADDR_ACCESSIBLE FC_FUNC_(shmem_addr_accessible, SHMEM_ADDR_ACCESSIBLE)
fortran_integer_t FC_SHMEM_ADDR_ACCESSIBLE(void *addr, fortran_integer_t *pe);
fortran_integer_t
FC_SHMEM_ADDR_ACCESSIBLE(void *addr, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);

     return shmem_internal_addr_accessible(addr, *pe);
}
