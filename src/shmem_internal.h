/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#ifndef PORTALS_SHMEM_INTERNAL_H
#define PORTALS_SHMEM_INTERNAL_H

#define SHMEM_IDX 10

extern ptl_handle_ni_t ni_h;
extern ptl_pt_index_t pt_entry;
extern ptl_handle_md_t md_h;
extern ptl_handle_le_t le_h;
extern ptl_handle_ct_t target_ct_h;
extern ptl_handle_ct_t source_ct_h;
extern ptl_handle_eq_t source_eq_h;
extern ptl_handle_eq_t err_eq_h;
extern ptl_size_t max_ordered_size;
extern ptl_size_t pending_counter;


/* Symmetric heap routines */
int symmetric_init(void);

#endif
