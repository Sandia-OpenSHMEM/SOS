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

#include <sys/time.h>

#include "shmem_internal.h"


#define FC_NUM_PES FC_FUNC_(num_pes, NUM_PES)
fortran_integer_t FC_NUM_PES(void);
fortran_integer_t
FC_NUM_PES(void)
{
     return shmem_internal_num_pes;
}


#define FC_SHMEM_N_PES FC_FUNC_(shmem_n_pes, SHMEM_N_PES)
fortran_integer_t FC_SHMEM_N_PES(void);
fortran_integer_t
FC_SHMEM_N_PES(void)
{
     return shmem_internal_num_pes;
}


#define FC_MY_PE FC_FUNC_(my_pe, MY_PE)
fortran_integer_t FC_MY_PE(void);
fortran_integer_t
FC_MY_PE(void)
{
     return shmem_internal_my_pe;
}


#define FC_SHMEM_MY_PE FC_FUNC_(shmem_my_pe, SHMEM_MY_PE)
fortran_integer_t FC_SHMEM_MY_PE(void);
fortran_integer_t
FC_SHMEM_MY_PE(void)
{
     return shmem_internal_my_pe;
}

#define FC_SHMEM_WTIME FC_FUNC_(shmem_wtime, SHMEM_WTIME)
fortran_double_precision_t FC_SHMEM_WTIME(void);
fortran_double_precision_t
FC_SHMEM_WTIME(void)
{
     double wtime;
     struct timeval tv;

     gettimeofday(&tv, NULL);
     wtime = tv.tv_sec;
     wtime += (double)tv.tv_usec / 1000000.0;
     return wtime;
}
