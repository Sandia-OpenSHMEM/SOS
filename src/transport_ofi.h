/* -*- C -*-
 *
 * Copyright (c) 2016 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef TRANSPORT_OFI_H
#define TRANSPORT_OFI_H

#include <stdio.h>
#include <stdlib.h>
#include <rdma/fabric.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_tagged.h>
#include <rdma/fi_rma.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_atomic.h>
#include <netdb.h>
#if USE_PORTALS4
#include <portals4.h>
#endif
#include <string.h>
#include <unistd.h>
#include <stddef.h>
#include "shmem_free_list.h"
#include "shmem_internal.h"
#include "shmem_atomic.h"

extern struct fid_ep*			shmem_transport_ofi_epfd;
extern struct fid_ep*			shmem_transport_ofi_cntr_epfd;
extern struct fid_cq*              	shmem_transport_ofi_put_nb_cqfd;
#ifndef ENABLE_HARD_POLLING
extern struct fid_cntr*            	shmem_transport_ofi_target_cntrfd;
#endif
extern struct fid_cntr*            	shmem_transport_ofi_put_cntrfd;
extern struct fid_cntr*            	shmem_transport_ofi_get_cntrfd;
#ifndef ENABLE_MR_SCALABLE
extern uint64_t*                        shmem_transport_ofi_target_heap_keys;
extern uint64_t*                        shmem_transport_ofi_target_data_keys;
#ifndef ENABLE_REMOTE_VIRTUAL_ADDRESSING
extern uint8_t**                       shmem_transport_ofi_target_heap_addrs;
extern uint8_t**                       shmem_transport_ofi_target_data_addrs;
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */
#endif /* ENABLE_MR_SCALABLE */
extern uint64_t          		shmem_transport_ofi_pending_put_counter;
extern uint64_t 	       	 	shmem_transport_ofi_pending_get_counter;
extern uint64_t				shmem_transport_ofi_pending_cq_count;
extern uint64_t				shmem_transport_ofi_max_poll;
extern size_t          		 	shmem_transport_ofi_max_buffered_send;
extern size_t    			shmem_transport_ofi_max_msg_size;
extern size_t    			shmem_transport_ofi_bounce_buffer_size;
#ifdef ENABLE_THREADS
extern shmem_internal_mutex_t           shmem_transport_ofi_lock;
#endif

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

#define OFI_RET_CHECK(ret)                                                      \
    do {                                                                        \
        if (ret) {                                                              \
            RAISE_ERROR_MSG("OFI error #%zd: %s \n", ret, fi_strerror(ret));    \
        }                                                                       \
    } while (0)

#define OFI_CQ_ERROR(cq, err)                                                   \
    do {                                                                        \
        const char *errmsg = fi_cq_strerror(cq, (err)->prov_errno,              \
                                            (err)->err_data, NULL, 0);          \
        RAISE_ERROR_STR(errmsg);                                                \
    } while (0)

#ifdef ENABLE_MR_SCALABLE
static inline
void shmem_transport_ofi_get_mr(const void *addr, int dest_pe,
                                uint8_t **mr_addr, uint64_t *key) {
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
    *key = 0;
    *mr_addr = (uint8_t*) addr;
#else
    if ((void*) addr >= shmem_internal_data_base &&
        (uint8_t*) addr < (uint8_t*) shmem_internal_data_base + shmem_internal_data_length) {

        *key = 0;
        *mr_addr = (uint8_t*) ((uint8_t *) addr - (uint8_t *) shmem_internal_data_base);

    } else if ((void*) addr >= shmem_internal_heap_base &&
              (uint8_t*) addr < (uint8_t*) shmem_internal_heap_base + shmem_internal_heap_length) {

        *key = 1;
        *mr_addr = (uint8_t*) ((uint8_t *) addr - (uint8_t *) shmem_internal_heap_base);
    } else {
        *key = 0;
        *mr_addr = NULL;
        RAISE_ERROR_MSG("[%03d] ERROR in %s: address (0x%p) outside of symmetric areas\n",
               shmem_internal_my_pe, __func__, addr);
    }
#endif /* ENABLE_REMOTE_VIRTUAL_ADDRESSING */

}

#else
static inline
void shmem_transport_ofi_get_mr(const void *addr, int dest_pe,
                                uint8_t **mr_addr, uint64_t *key) {
    if ((void*) addr >= shmem_internal_data_base &&
        (uint8_t*) addr < (uint8_t*) shmem_internal_data_base + shmem_internal_data_length) {
        *key = shmem_transport_ofi_target_data_keys[dest_pe];
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
        *mr_addr = (uint8_t *) addr;
#else
        *mr_addr = shmem_transport_ofi_target_data_addrs[dest_pe] +
            ((uint8_t *) addr - (uint8_t *) shmem_internal_data_base);
#endif
    }

    else if ((void*) addr >= shmem_internal_heap_base &&
             (uint8_t*) addr < (uint8_t*) shmem_internal_heap_base + shmem_internal_heap_length) {
        *key = shmem_transport_ofi_target_heap_keys[dest_pe];
#ifdef ENABLE_REMOTE_VIRTUAL_ADDRESSING
        *mr_addr = (uint8_t *) addr;
#else
        *mr_addr = shmem_transport_ofi_target_heap_addrs[dest_pe] +
            ((uint8_t *) addr - (uint8_t *) shmem_internal_heap_base);
#endif
    }

    else {
        *key = -1;
        *mr_addr = NULL;
        RAISE_ERROR_MSG("[%03d] ERROR in %s: address (0x%p) outside of symmetric areas\n",
               shmem_internal_my_pe, __func__, addr);
    }
}
#endif

typedef enum fi_datatype shm_internal_datatype_t;
typedef enum fi_op       shm_internal_op_t;

// Datatypes
#define SHM_INTERNAL_FLOAT           FI_FLOAT
#define SHM_INTERNAL_DOUBLE          FI_DOUBLE
#define SHM_INTERNAL_LONG_DOUBLE     FI_LONG_DOUBLE
#define SHM_INTERNAL_FLOAT_COMPLEX   FI_FLOAT_COMPLEX
#define SHM_INTERNAL_DOUBLE_COMPLEX  FI_DOUBLE_COMPLEX
#define SHM_INTERNAL_SIGNED_BYTE     FI_INT8
#define SHM_INTERNAL_INT8            FI_INT8
#define SHM_INTERNAL_INT16           FI_INT16
#define SHM_INTERNAL_INT32           FI_INT32
#define SHM_INTERNAL_INT64           FI_INT64
#define SHM_INTERNAL_SHORT           DTYPE_SHORT
#define SHM_INTERNAL_INT             DTYPE_INT
#define SHM_INTERNAL_LONG            DTYPE_LONG
#define SHM_INTERNAL_LONG_LONG       DTYPE_LONG_LONG
#define SHM_INTERNAL_FORTRAN_INTEGER DTYPE_FORTRAN_INTEGER

 // Operations
#define SHM_INTERNAL_BAND            FI_BAND
#define SHM_INTERNAL_BOR             FI_BOR
#define SHM_INTERNAL_BXOR            FI_BXOR
#define SHM_INTERNAL_MIN             FI_MIN
#define SHM_INTERNAL_MAX             FI_MAX
#define SHM_INTERNAL_SUM             FI_SUM
#define SHM_INTERNAL_PROD            FI_PROD

#define SHMEM_TRANSPORT_OFI_TYPE_BOUNCE 0x01
#define SHMEM_TRANSPORT_OFI_TYPE_LONG   0x02


extern fi_addr_t *addr_table;

#ifdef USE_AV_MAP
#define GET_DEST(dest) ((fi_addr_t)(addr_table[(dest)]))
#else
#define GET_DEST(dest) ((fi_addr_t)(dest))
#endif


struct shmem_transport_ofi_frag_t {
    shmem_free_list_item_t item;
    uint8_t mytype;
};

typedef struct shmem_transport_ofi_frag_t shmem_transport_ofi_frag_t;

struct shmem_transport_ofi_bounce_buffer_t {
    shmem_transport_ofi_frag_t frag;
    uint8_t data[];
};

typedef struct shmem_transport_ofi_bounce_buffer_t shmem_transport_ofi_bounce_buffer_t;

typedef int shmem_transport_ct_t;

extern shmem_free_list_t *shmem_transport_ofi_bounce_buffers;

int shmem_transport_init(long eager_size);
int shmem_transport_startup(void);
void shmem_transport_print_info(void);
int shmem_transport_fini(void);

extern size_t SHMEM_Dtsize[FI_DATATYPE_LAST];

static inline void shmem_transport_get_wait(void);

static inline
void shmem_transport_ofi_drain_cq(void)
{

	ssize_t ret = 0;
	struct fi_cq_entry buf;

	if (!shmem_transport_ofi_pending_cq_count) {
		return;
	}

	do {
		ret = fi_cq_read(shmem_transport_ofi_put_nb_cqfd,
				(void *)&buf, 1);
		/*error cases*/
		if (ret < 0 && ret != -FI_EAGAIN ) {
			if(ret == -FI_EAVAIL) {
				struct fi_cq_err_entry e = {0};
             	 		fi_cq_readerr(shmem_transport_ofi_put_nb_cqfd,
				              (void *)&e, 0);
				OFI_CQ_ERROR(shmem_transport_ofi_put_nb_cqfd, &e);
			} else {
				OFI_RET_CHECK(ret);
			}
		}

		if(ret == 1) {
			shmem_transport_ofi_frag_t *frag =
                (shmem_transport_ofi_frag_t *) buf.op_context;

			if(SHMEM_TRANSPORT_OFI_TYPE_BOUNCE == frag->mytype) {
				shmem_free_list_free(
					shmem_transport_ofi_bounce_buffers,
                    (shmem_transport_ofi_bounce_buffer_t *) frag);
                        }
                        else {
                            RAISE_ERROR_STR("Unrecognized completion object");
			}

			shmem_transport_ofi_pending_cq_count--;

		}


	} while(ret > 0);

}

static inline shmem_transport_ofi_bounce_buffer_t * create_bounce_buffer(const void *source, const size_t len)
{
	shmem_transport_ofi_bounce_buffer_t *buff;

	buff = (shmem_transport_ofi_bounce_buffer_t*)
	shmem_free_list_alloc(shmem_transport_ofi_bounce_buffers);

	/*if LL empty = error, should've been avoided with EQ drain*/
	if (NULL == buff)
            RAISE_ERROR_STR("Error allocating bounce buffer");

        shmem_internal_assert(buff->frag.mytype == SHMEM_TRANSPORT_OFI_TYPE_BOUNCE);

	memcpy(buff->data, source, len);

	return buff;
}

static inline void shmem_transport_put_quiet(void)
{
    SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

    /* wait until all outstanding queue operations have completed */
    while (shmem_transport_ofi_pending_cq_count) {
        shmem_transport_ofi_drain_cq();
    }

    /* wait for put counter to meet outstanding count value */
#ifdef ENABLE_COMPLETION_POLLING
    uint64_t success, fail;
    do {
        success = fi_cntr_read(shmem_transport_ofi_put_cntrfd);
        fail = fi_cntr_readerr(shmem_transport_ofi_put_cntrfd);

        if (success < shmem_transport_ofi_pending_put_counter && fail == 0) {
            SPINLOCK_BODY();
        }
        else if (fail) {
            struct fi_cq_err_entry e = {0};
            fi_cq_readerr(shmem_transport_ofi_put_nb_cqfd, (void *)&e, 0);
            SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
            OFI_CQ_ERROR(shmem_transport_ofi_put_nb_cqfd, &e);
        }
    } while (success < shmem_transport_ofi_pending_put_counter);
#else
    int ret = fi_cntr_wait(shmem_transport_ofi_put_cntrfd,
                           shmem_transport_ofi_pending_put_counter, -1);
    if (ret) {
        struct fi_cq_err_entry e = {0};
        fi_cq_readerr(shmem_transport_ofi_put_nb_cqfd, (void *)&e, 0);
        SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
        OFI_CQ_ERROR(shmem_transport_ofi_put_nb_cqfd, &e);
    }
#endif
    SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}

static inline int shmem_transport_quiet(void)
{

	shmem_transport_put_quiet();

	shmem_transport_get_wait();

	return 0;
}


static inline
int
shmem_transport_fence(void)
{
#if WANT_TOTAL_DATA_ORDERING == 0
	/*unordered network model*/
  return shmem_transport_quiet();
#else
  return 0;
#endif

}

/*RM requires polling until space is available*/
static inline int try_again(const int ret, uint64_t *polled) {

	if (ret) {
		if (ret == -FI_EAGAIN) {
			shmem_transport_ofi_drain_cq();
			(*polled)++;
			if ((*polled) <= shmem_transport_ofi_max_poll) {
				return 1;
			}
		}
		OFI_RET_CHECK(ret);
	}

	return 0;
}

static inline
void
shmem_transport_put_small(void *target, const void *source, size_t len, int pe)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;
        uint64_t key;
        uint8_t *addr;

        shmem_transport_ofi_get_mr(target, pe, &addr, &key);

        shmem_internal_assert(len <= shmem_transport_ofi_max_buffered_send);

	do {

		ret = fi_inject_write(shmem_transport_ofi_cntr_epfd,
				source,
				len,
				GET_DEST(dst),
				(uint64_t) addr,
				key);

	} while(try_again(ret,&polled));

        shmem_internal_assert(ret == 0);
	/* automatically get local completion but need remote completion for fence/quiet*/
	shmem_transport_ofi_pending_put_counter++;
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}

static inline
void
shmem_transport_ofi_put_large(void *target, const void *source, size_t len, int pe)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);
	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    uint8_t *frag_source = (uint8_t *) source;
    uint64_t frag_target = (uint64_t) addr;
    size_t frag_len = len;

     /* operation generates counting events and must be completed by
      * quiet. */
     while (frag_source < ((uint8_t *) source) + len) {
        frag_len = MIN(shmem_transport_ofi_max_msg_size, (size_t) (((uint8_t *) source) + len - frag_source));
        polled = 0;

        do {
            ret = fi_write(shmem_transport_ofi_cntr_epfd,
                           frag_source, frag_len, NULL,
                           GET_DEST(dst), frag_target,
                           key, NULL);
        } while (try_again(ret,&polled));

        shmem_transport_ofi_pending_put_counter++;

        frag_source += frag_len;
        frag_target += frag_len;
    }
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}

static inline
void
shmem_transport_put_nb(void *target, const void *source, size_t len,
                       int pe, long *completion)
{
	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;
        uint64_t key;
        uint8_t *addr;


	if (len <= shmem_transport_ofi_max_buffered_send) {

        shmem_transport_put_small(target, source, len, pe);

	} else if (len <= shmem_transport_ofi_bounce_buffer_size) {

          SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);
        shmem_transport_ofi_get_mr(target, pe, &addr, &key);

		shmem_transport_ofi_bounce_buffer_t *buff = create_bounce_buffer(source, len);
		polled = 0;

		do {
			ret = fi_write(shmem_transport_ofi_epfd,
					buff->data, len, NULL,
					GET_DEST(dst), (uint64_t) addr,
					key, buff);
		} while(try_again(ret,&polled));

		shmem_transport_ofi_pending_cq_count++;
                SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);

    } else {
        shmem_transport_ofi_put_large(target, source,len, pe);
        (*completion)++;
    }
}

/*compatibility with Portals transport*/
static inline
void
shmem_transport_put_wait(long *completion) {

    shmem_internal_assert((*completion) >= 0);

    if((*completion) > 0) {
        shmem_transport_put_quiet();
        (*completion)--;
    }
}

static inline
void
shmem_transport_put_nbi(void *target, const void *source, size_t len, int pe)
{
	if (len <= shmem_transport_ofi_max_buffered_send) {

        shmem_transport_put_small(target, source, len, pe);

    } else {

        shmem_transport_ofi_put_large(target, source, len, pe);
    }
}


static inline
void
shmem_transport_get(void *target, const void *source, size_t len, int pe)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);
	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;
        uint64_t key;
        uint8_t *addr;

        shmem_transport_ofi_get_mr(source, pe, &addr, &key);

        if (len <= shmem_transport_ofi_max_msg_size) {
            do {
                ret = fi_read(shmem_transport_ofi_cntr_epfd,
                              target,
                              len,
                              NULL,
                              GET_DEST(dst),
                              (uint64_t) addr,
                              key,
                              NULL);
            } while (try_again(ret,&polled));

            shmem_transport_ofi_pending_get_counter++;
        }
        else {
            uint8_t *frag_target = (uint8_t *) target;
            uint64_t frag_source = (uint64_t) addr;
            size_t frag_len = len;

            while (frag_target < ((uint8_t *) target) + len) {
                frag_len = MIN(shmem_transport_ofi_max_msg_size, (size_t) (((uint8_t *) target) + len - frag_target));
                polled = 0;

                do {
                    ret = fi_read(shmem_transport_ofi_cntr_epfd,
                                  frag_target, frag_len, NULL,
                                  GET_DEST(dst), frag_source,
                                  key, NULL);
                } while (try_again(ret,&polled));

                shmem_transport_ofi_pending_get_counter++;

                frag_source += frag_len;
                frag_target += frag_len;
            }
        }
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


static inline
void
shmem_transport_get_wait(void)
{
    SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

    /* wait for get counter to meet outstanding count value */
#ifdef ENABLE_COMPLETION_POLLING
    uint64_t success, fail;
    do {
        success = fi_cntr_read(shmem_transport_ofi_get_cntrfd);
        fail = fi_cntr_readerr(shmem_transport_ofi_get_cntrfd);

        if (success < shmem_transport_ofi_pending_get_counter && fail == 0) {
            SPINLOCK_BODY();
        }
        else if (fail) {
            struct fi_cq_err_entry e = {0};
            fi_cq_readerr(shmem_transport_ofi_put_nb_cqfd, (void *)&e, 0);
            SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
            OFI_CQ_ERROR(shmem_transport_ofi_put_nb_cqfd, &e);
        }
    } while (success < shmem_transport_ofi_pending_get_counter);
#else
    int ret = fi_cntr_wait(shmem_transport_ofi_get_cntrfd,
                           shmem_transport_ofi_pending_get_counter, -1);
    if (ret) {
        struct fi_cq_err_entry e = {0};
        fi_cq_readerr(shmem_transport_ofi_put_nb_cqfd, (void *)&e, 0);
        SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
        OFI_CQ_ERROR(shmem_transport_ofi_put_nb_cqfd, &e);
    }
#endif

  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


static inline
void
shmem_transport_swap(void *target, const void *source, void *dest,
                     size_t len, int pe, int datatype)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);
	int ret = 0;
        uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;
        uint64_t key;
        uint8_t *addr;

        shmem_transport_ofi_get_mr(target, pe, &addr, &key);

        shmem_internal_assert(len <= sizeof(double complex));
        shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

	do {
        	ret = fi_fetch_atomic(shmem_transport_ofi_cntr_epfd,
                	source,
                	1,
			NULL,
			dest,
			NULL,
			GET_DEST(dst),
			(uint64_t) addr,
			key,
			datatype,
			FI_ATOMIC_WRITE,
			NULL);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_get_counter++;
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


static inline
void
shmem_transport_cswap(void *target, const void *source, void *dest,
                      const void *operand, size_t len, int pe, int datatype)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

	int ret = 0;
        uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;
        uint64_t key;
        uint8_t *addr;

        shmem_transport_ofi_get_mr(target, pe, &addr, &key);

        shmem_internal_assert(len <= sizeof(double complex));
        shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

	do {
		ret = fi_compare_atomic(shmem_transport_ofi_cntr_epfd,
			source,
			1,
			NULL,
			operand,
			NULL,
			dest,
			NULL,
			GET_DEST(dst),
			(uint64_t) addr,
			key,
			datatype,
			FI_CSWAP,
			NULL);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_get_counter++;
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


static inline
void
shmem_transport_mswap(void *target, const void *source, void *dest,
                      const void *mask, size_t len, int pe, int datatype)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

	int ret = 0;
        uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;
        uint64_t key;
        uint8_t *addr;

        shmem_transport_ofi_get_mr(target, pe, &addr, &key);

        shmem_internal_assert(len <= sizeof(double complex));
        shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

	do {
		ret = fi_compare_atomic(shmem_transport_ofi_cntr_epfd,
			source,
			1,
			NULL,
			mask,
			NULL,
			dest,
			NULL,
		        GET_DEST(dst),
			(uint64_t) addr,
			key,
			datatype,
			FI_MSWAP,
			NULL);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_get_counter++;
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


static inline
void
shmem_transport_atomic_small(void *target, const void *source, size_t len,
                                       int pe, int op, int datatype)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;
        uint64_t key;
        uint8_t *addr;

        shmem_transport_ofi_get_mr(target, pe, &addr, &key);

        shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

	do {
		ret = fi_inject_atomic(shmem_transport_ofi_cntr_epfd,
			source,
			1,
			GET_DEST(dst),
			(uint64_t) addr,
			key,
			datatype,
			op);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_put_counter++;
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


static inline
void
shmem_transport_atomic_set(void *target, const void *source, size_t len,
                           int pe, int datatype)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(target, pe, &addr, &key);

    shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

    do {
        ret = fi_inject_atomic(shmem_transport_ofi_cntr_epfd,
                               source,
                               1,
                               GET_DEST(dst),
                               (uint64_t) addr,
                               key,
                               datatype,
                               FI_ATOMIC_WRITE);
    } while (try_again(ret, &polled));

    shmem_transport_ofi_pending_put_counter++;
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


static inline
void
shmem_transport_atomic_fetch(void *target, const void *source, size_t len,
                            int pe, int datatype)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);

    int ret = 0;
    uint64_t dst = (uint64_t) pe;
    uint64_t polled = 0;
    uint64_t key;
    uint8_t *addr;

    shmem_transport_ofi_get_mr(source, pe, &addr, &key);

    shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

    do {
        ret = fi_fetch_atomic(shmem_transport_ofi_cntr_epfd,
                              NULL,
                              1,
                              NULL,
                              (void *) target,
                              NULL,
                              GET_DEST(dst),
                              (uint64_t) addr,
                              key,
                              datatype,
                              FI_ATOMIC_READ,
                              NULL);
    } while (try_again(ret, &polled));

    shmem_transport_ofi_pending_get_counter++;
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


static inline
void
shmem_transport_atomic_nb(void *target, const void *source, size_t full_len,
                                   int pe, int op, int datatype,
                                   long *completion)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);
	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	size_t len = full_len/SHMEM_Dtsize[datatype];
	uint64_t polled = 0;
        uint64_t key;
        uint8_t *addr;
	size_t max_atomic_size = 0;

        shmem_internal_assert(SHMEM_Dtsize[datatype] * len == full_len);

	ret = fi_atomicvalid(shmem_transport_ofi_epfd, datatype, op,
				&max_atomic_size);
	max_atomic_size = max_atomic_size * SHMEM_Dtsize[datatype];
	if (max_atomic_size > shmem_transport_ofi_max_msg_size
		|| ret || max_atomic_size == 0) {
		RAISE_ERROR_MSG("atomic_nb error: datatype %d x op %d not supported\n",
                    datatype, op);
	}

        shmem_transport_ofi_get_mr(target, pe, &addr, &key);

	if ( full_len <= MIN(shmem_transport_ofi_max_buffered_send,
        max_atomic_size)) {

		polled = 0;

		do {
			ret = fi_inject_atomic(shmem_transport_ofi_cntr_epfd,
                		source,
                      		len, //count
				GET_DEST(dst),
				(uint64_t) addr,
				key,
                        	datatype,
                        	op);
		} while(try_again(ret,&polled));

		shmem_transport_ofi_pending_put_counter++;

        } else if (full_len <=
			MIN(shmem_transport_ofi_bounce_buffer_size, max_atomic_size)) {

			shmem_transport_ofi_bounce_buffer_t *buff = create_bounce_buffer(source, full_len);

			polled = 0;

		do {
			ret = fi_atomic(shmem_transport_ofi_epfd,
				buff->data,
				len,
				NULL,
				GET_DEST(dst),
				(uint64_t) addr,
				key,
				datatype,
				op,
				buff);
		} while(try_again(ret,&polled));

			shmem_transport_ofi_pending_cq_count++;

        } else {
		size_t sent = 0;

		while (sent < len) {

			size_t chunksize = MIN((len-sent),
				(max_atomic_size/SHMEM_Dtsize[datatype]));
			polled = 0;
		do {
			ret = fi_atomic(shmem_transport_ofi_cntr_epfd,
				(void *)((char *)source +
					(sent*SHMEM_Dtsize[datatype])),
				chunksize,
				NULL,
				GET_DEST(dst),
				((uint64_t) addr +
				 	(sent*SHMEM_Dtsize[datatype])),
				key,
				datatype,
				op,
				NULL);
		} while(try_again(ret,&polled));

			shmem_transport_ofi_pending_put_counter++;
			sent += chunksize;
		}
        }
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


static inline
void
shmem_transport_fetch_atomic(void *target, const void *source, void *dest,
                             size_t len, int pe, int op, int datatype)
{
  SHMEM_MUTEX_LOCK(shmem_transport_ofi_lock);
        int ret = 0;
        uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;
        uint64_t key;
        uint8_t *addr;

        shmem_transport_ofi_get_mr(target, pe, &addr, &key);

        shmem_internal_assert(len <= sizeof(double complex));
        shmem_internal_assert(SHMEM_Dtsize[datatype] == len);

	do {
        	ret = fi_fetch_atomic(shmem_transport_ofi_cntr_epfd,
			source,
			1,
			NULL,
			dest,
			NULL,
			GET_DEST(dst),
			(uint64_t) addr,
			key,
			datatype,
			op,
			NULL);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_get_counter++;
  SHMEM_MUTEX_UNLOCK(shmem_transport_ofi_lock);
}


/* detect atomic limitation on the fly and provide software reduction support
    if needed */
static inline
int shmem_transport_atomic_supported(shm_internal_op_t op,
                                     shm_internal_datatype_t datatype)
{
   size_t size = 0;
   int ret = fi_atomicvalid(shmem_transport_ofi_epfd, datatype, op, &size);
   return !(ret != 0 || size == 0);
}


static inline
void
shmem_transport_put_ct_nb(shmem_transport_ct_t *ct, void *target,
                              const void *source, size_t len, int pe, long *completion)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
void shmem_transport_get_ct(shmem_transport_ct_t *ct, void *target,
                                const void *source, size_t len, int pe)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
void shmem_transport_ct_create(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
void shmem_transport_ct_free(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
long shmem_transport_ct_get(shmem_transport_ct_t *ct)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
    return -1;
}

static inline
void shmem_transport_ct_set(shmem_transport_ct_t *ct, long value)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
void shmem_transport_ct_wait(shmem_transport_ct_t *ct, long wait_for)
{
    RAISE_ERROR_STR("OFI transport does not currently support CT operations");
}

static inline
void shmem_transport_spinlock_body(void)
{
    struct fi_cq_err_entry e = {0};
    ssize_t ret = fi_cq_readerr(shmem_transport_ofi_put_nb_cqfd,
                                (void *)&e, 0);
    if (ret != -FI_EAGAIN)
        OFI_CQ_ERROR(shmem_transport_ofi_put_nb_cqfd, &e);
}

static inline
uint64_t shmem_transport_received_cntr_get(void)
{
#ifndef ENABLE_HARD_POLLING
    return fi_cntr_read(shmem_transport_ofi_target_cntrfd);
#else
    RAISE_ERROR_STR("OFI transport configured for hard polling");
    return 0;
#endif
}

static inline
void shmem_transport_received_cntr_wait(uint64_t ge_val)
{
#ifndef ENABLE_HARD_POLLING
    int ret = fi_cntr_wait(shmem_transport_ofi_target_cntrfd, ge_val, -1);

    OFI_RET_CHECK(ret);
#else
    RAISE_ERROR_STR("OFI transport configured for hard polling");
#endif
}

#endif /* TRANSPORT_OFI_H */
