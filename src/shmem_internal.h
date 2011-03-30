/* -*- C -*-
 *
 * Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
 */

#ifndef PORTALS_SHMEM_INTERNAL_H
#define PORTALS_SHMEM_INTERNAL_H

#define DATA_IDX 10
#define HEAP_IDX 11

extern ptl_handle_ni_t ni_h;
extern ptl_pt_index_t data_pt;
extern ptl_pt_index_t heap_pt;
extern ptl_handle_md_t put_md_h;
extern ptl_handle_md_t get_md_h;
extern ptl_handle_ct_t target_ct_h;
extern ptl_handle_ct_t put_ct_h;
extern ptl_handle_ct_t get_ct_h;
#ifdef ENABLE_EVENT_COMPLETION
extern ptl_handle_eq_t put_eq_h;
#endif
extern ptl_handle_eq_t err_eq_h;
extern ptl_size_t max_ordered_size;
extern ptl_size_t pending_put_counter;
extern ptl_size_t pending_get_counter;

#define GET_REMOTE_ACCESS(target, pt, offset)                           \
    do {                                                                \
        if (((void*) target > shmem_data_base) &&                       \
            ((char*) target < (char*) shmem_data_base + shmem_data_length)) { \
            pt = data_pt;                                               \
            offset = (char*) target - (char*) shmem_data_base;          \
        } else if (((void*) target > shmem_heap_base) &&                \
                   ((char*) target < (char*) shmem_heap_base + shmem_heap_length)) { \
        pt = heap_pt;                                                   \
        offset = (char*) target - (char*) shmem_heap_base;              \
    } else {                                                            \
        printf("target outside of symmetric areas\n");                  \
        abort();                                                        \
    }                                                                   \
    } while (0)

extern void *shmem_heap_base;
extern long shmem_heap_length;
extern void *shmem_data_base;
extern long shmem_data_length;

/* Symmetric heap routines */
int symmetric_init(void);

#endif
