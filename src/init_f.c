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


#define FC_START_PES FC_FUNC_(start_pes, START_PES)
void FC_START_PES(fortran_integer_t *npes);
void
FC_START_PES(fortran_integer_t *npes)
{
    shmem_internal_init();
}
