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

#include <stdio.h>
#include <stdint.h>

#ifndef PORTALS_SHMEM_INTERNAL_H
#define PORTALS_SHMEM_INTERNAL_H

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

extern int shmem_internal_my_pe;
extern int shmem_internal_num_pes;

extern int shmem_internal_initialized;
extern int shmem_internal_finalized;


#define RAISE_ERROR(ret)                                                \
    do {                                                                \
        fprintf(stderr, "[%03d] ERROR: %s:%d return code %d\n",         \
                shmem_internal_my_pe, __FILE__, __LINE__, (int) ret);   \
        abort();                                                        \
    } while (0)


#define RAISE_ERROR_STR(str)                                            \
    do {                                                                \
        fprintf(stderr, "[%03d] ERROR: %s:%d: %s\n",                    \
                shmem_internal_my_pe, __FILE__, __LINE__, str);         \
        abort();                                                        \
    } while (0)

        

/* initialization routines */
int shmem_internal_symmetric_init(size_t requested_length, int use_malloc);
int shmem_internal_symmetric_fini(void);
int shmem_internal_collectives_init(int requested_crossover, 
                                    int requested_radix);

/* internal allocation, without a barrier */
void *shmalloc_init(size_t size);

#endif
