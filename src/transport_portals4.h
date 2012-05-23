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

#ifndef TRANSPORT_PORTALS_H
#define TRANSPORT_PORTALS_H

#include <portals4.h>

extern ptl_handle_ni_t shmem_internal_ni_h;
extern ptl_pt_index_t shmem_internal_data_pt;
extern ptl_pt_index_t shmem_internal_heap_pt;
extern ptl_handle_md_t shmem_internal_put_md_h;
extern ptl_handle_md_t shmem_internal_get_md_h;
extern ptl_handle_ct_t shmem_internal_target_ct_h;
extern ptl_handle_ct_t shmem_internal_put_ct_h;
extern ptl_handle_ct_t shmem_internal_get_ct_h;
#ifdef ENABLE_EVENT_COMPLETION
extern ptl_handle_eq_t shmem_internal_put_eq_h;
#endif
extern ptl_handle_eq_t shmem_internal_err_eq_h;
extern ptl_size_t shmem_internal_max_put_size;
extern ptl_size_t shmem_internal_max_atomic_size;
extern ptl_size_t shmem_internal_max_fetch_atomic_size;

extern ptl_size_t shmem_internal_pending_put_counter;
extern ptl_size_t shmem_internal_pending_get_counter;


int shmem_transport_portals4_init(void);

int shmem_transport_portals4_connectivity(void);

int shmem_transport_portals4_startup(void);

int shmem_transport_portals4_fini(void);

#endif
