/*
 *
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license. For
 *  license information, see the LICENSE file in the top level directory.
 *
 */

#ifndef TRANSPORT_OFI_H
#define TRANSPORT_OFI_H

#include <assert.h>
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
#include "shmem_free_list.h"
#include <string.h>
#include "shmem_internal.h"
#include <unistd.h>
#include <stddef.h>

extern struct fid_fabric*       	shmem_transport_ofi_fabfd;
extern struct fid_domain*          	shmem_transport_ofi_domainfd;
extern struct fid_ep*			shmem_transport_ofi_epfd;
extern struct fid_ep*			shmem_transport_ofi_cntr_epfd;
extern struct fid_stx*  		shmem_transport_ofi_stx;
extern struct fid_av*             	shmem_transport_ofi_avfd;
extern struct fid_cq*              	shmem_transport_ofi_put_nb_cqfd;
extern struct fid_cntr*            	shmem_transport_ofi_target_cntrfd;
extern struct fid_cntr*            	shmem_transport_ofi_put_cntrfd;
extern struct fid_cntr*            	shmem_transport_ofi_get_cntrfd;
extern struct fid_mr*              	shmem_transport_ofi_target_mrfd;
extern uint64_t          		shmem_transport_ofi_pending_put_counter;
extern uint64_t 	       	 	shmem_transport_ofi_pending_get_counter;
extern uint64_t				shmem_transport_ofi_pending_cq_count;
extern uint64_t				shmem_transport_ofi_max_poll;
extern size_t          		 	shmem_transport_ofi_max_buffered_send;
extern size_t    		 	shmem_transport_ofi_max_atomic_size;
extern size_t const    			shmem_transport_ofi_queue_slots;
extern size_t    			shmem_transport_ofi_bounce_buffer_size;

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
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
#define SHM_INTERNAL_INT32           FI_INT32
#define SHM_INTERNAL_INT64           FI_INT64

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

typedef struct fi_context fi_context_t;

struct shmem_transport_ofi_frag_t {
    shmem_free_list_item_t item;
    uint8_t mytype;
    fi_context_t context;
};

typedef struct shmem_transport_ofi_frag_t shmem_transport_ofi_frag_t;

struct shmem_transport_ofi_bounce_buffer_t {
    shmem_transport_ofi_frag_t frag;
    uint8_t data[];
};

typedef struct shmem_transport_ofi_bounce_buffer_t shmem_transport_ofi_bounce_buffer_t;

struct shmem_transport_ofi_long_frag_t {
    shmem_transport_ofi_frag_t frag;
    int reference;
    long *completion;
};
typedef struct shmem_transport_ofi_long_frag_t shmem_transport_ofi_long_frag_t;

typedef int shmem_transport_ct_t;

extern shmem_free_list_t *shmem_transport_ofi_bounce_buffers;

extern shmem_free_list_t *shmem_transport_ofi_frag_buffers;

#define OFI_ABORT(...) { fprintf(stderr, __FILE__ ":%d: \n", __LINE__); \
                            fprintf(stderr, __VA_ARGS__);  }

#define OFI_RET_CHECK(ret) do { if (ret) { RAISE_ERROR(ret); } } while (0)

int shmem_transport_init(long eager_size);
int shmem_transport_startup(void);
int shmem_transport_fini(void);

extern int SHMEM_Dtsize[FI_DATATYPE_LAST];

static inline
int
shmem_transport_ofi_drain_cq(void)
{

	ssize_t ret = 0;
	struct fi_cq_entry buf;

	if (!shmem_transport_ofi_pending_cq_count) {
		return 0;
	}

	do
	{
		ret = fi_cq_read(shmem_transport_ofi_put_nb_cqfd,
				(void *)&buf, 1);
		//error cases
		if (ret < 0 && ret != -FI_EAGAIN ) {
			if(ret == -FI_EAVAIL) {
				struct fi_cq_err_entry e;
             	 		fi_cq_readerr(shmem_transport_ofi_put_nb_cqfd,
				              (void *)&e, 0);
				RAISE_ERROR(e.err);
			}
			else
				RAISE_ERROR(ret);
		}

	} while(ret == -FI_EAGAIN);

	shmem_transport_ofi_frag_t *frag =
        container_of(buf.op_context, struct shmem_transport_ofi_frag_t,
		     context);

	if(SHMEM_TRANSPORT_OFI_TYPE_BOUNCE == frag->mytype) {
		shmem_free_list_free(shmem_transport_ofi_bounce_buffers,
					frag);
		shmem_transport_ofi_pending_cq_count--;
	} else {
		shmem_transport_ofi_long_frag_t *long_frag =
		(shmem_transport_ofi_long_frag_t*) frag;

		assert(long_frag->frag.mytype == SHMEM_TRANSPORT_OFI_TYPE_LONG);
	 	(*(long_frag->completion))--;
		if (0 >= --long_frag->reference) {
			long_frag->reference = 0;
			shmem_free_list_free(shmem_transport_ofi_frag_buffers,
			frag);
		}
		shmem_transport_ofi_pending_cq_count--;
	}

	return 0;

}

static inline shmem_transport_ofi_bounce_buffer_t * create_bounce_buffer(const void *source, const size_t len)
{
	shmem_transport_ofi_bounce_buffer_t *buff;

	while(shmem_transport_ofi_pending_cq_count >=
			(shmem_transport_ofi_queue_slots-2)) {
		shmem_transport_ofi_drain_cq();
        }

	buff = (shmem_transport_ofi_bounce_buffer_t*)
	shmem_free_list_alloc(shmem_transport_ofi_bounce_buffers);

	/*if LL empty = error, should've been avoided with EQ drain*/
	if (NULL == buff)
		RAISE_ERROR(-1);

	assert(buff->frag.mytype == SHMEM_TRANSPORT_OFI_TYPE_BOUNCE);

	memcpy(buff->data, source, len);

	return buff;
}


static inline shmem_transport_ofi_long_frag_t * create_long_frag(long *completion)
{
	shmem_transport_ofi_long_frag_t *long_frag;

	while(shmem_transport_ofi_pending_cq_count >=
			(shmem_transport_ofi_queue_slots-2)) {
		shmem_transport_ofi_drain_cq();
        }

	long_frag = (shmem_transport_ofi_long_frag_t*)
	shmem_free_list_alloc(shmem_transport_ofi_frag_buffers);

	if (NULL == long_frag)
		RAISE_ERROR(-1);

	assert(long_frag->frag.mytype == SHMEM_TRANSPORT_OFI_TYPE_LONG);
	assert(long_frag->reference == 0);
	long_frag->completion = completion;

	return long_frag;
}

static inline int shmem_transport_quiet(void)
{
	int ret = 0;

	while(shmem_transport_ofi_pending_cq_count) {
		ret = shmem_transport_ofi_drain_cq();
		OFI_RET_CHECK(ret);
	}

	/* wait for put counter to meet outstanding count value    */
	ret = fi_cntr_wait(shmem_transport_ofi_put_cntrfd,
			shmem_transport_ofi_pending_put_counter,-1);
	OFI_RET_CHECK(ret);

	return ret;
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

	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;

	do {

		ret = fi_inject_write(shmem_transport_ofi_cntr_epfd,
				source,
				len,
				GET_DEST(dst),
				(uint64_t) target,
				0);

	} while(try_again(ret,&polled));

	assert(ret == 0);
	/* automatically get local completion but need remote completion for fence/quiet*/
	shmem_transport_ofi_pending_put_counter++;
}

static inline
void
shmem_transport_put_nb(void *target, const void *source, size_t len,
                       int pe, long *completion)
{
	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;

	if (len <= shmem_transport_ofi_max_buffered_send) {

		polled = 0;

		do {
			ret = fi_inject_write(shmem_transport_ofi_cntr_epfd,
						source,
						len,
						GET_DEST(dst),
						(uint64_t) target,
						0);

		} while(try_again(ret,&polled));

		shmem_transport_ofi_pending_put_counter++;

	} else if (len <= shmem_transport_ofi_bounce_buffer_size) {

		shmem_transport_ofi_bounce_buffer_t *buff = create_bounce_buffer(source, len);
		polled = 0;

		do {
			ret = fi_write(shmem_transport_ofi_epfd,
					buff->data, len, NULL,
					GET_DEST(dst), (uint64_t) target,
					0, &buff->frag.context);
		} while(try_again(ret,&polled));

		shmem_transport_ofi_pending_cq_count++;

	} else {
		shmem_transport_ofi_long_frag_t *long_frag = create_long_frag(completion);

		polled = 0;

		do {
			ret = fi_write(shmem_transport_ofi_epfd,
					source, len, NULL,
					GET_DEST(dst), (uint64_t) target,
					0, &long_frag->frag.context);
		} while(try_again(ret,&polled));

		(*(long_frag->completion))++;
		long_frag->reference++;
		shmem_transport_ofi_pending_cq_count++;
	}
}

static inline
void
shmem_transport_put_wait(long *completion)
{
	while (*completion > 0) {
		shmem_transport_ofi_drain_cq();
	}
}

static inline
void
shmem_transport_get(void *target, const void *source, size_t len, int pe)
{
	int ret = 0;
        uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;

	do {
 		ret = fi_read(shmem_transport_ofi_cntr_epfd,
				target,
				len,
				NULL,
				GET_DEST(dst),
				(uint64_t) source,
				0,
				NULL);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_get_counter++;
}


static inline
void
shmem_transport_get_wait(void)
{
	int ret = 0;

	/* wait for get counter to meet outstanding count value    */
	ret = fi_cntr_wait(shmem_transport_ofi_get_cntrfd,
			shmem_transport_ofi_pending_get_counter,-1);
	OFI_RET_CHECK(ret);
}


static inline
void
shmem_transport_swap(void *target, void *source, void *dest, size_t len,
                              int pe, int datatype)
{
	int ret = 0;
        uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;

	assert(len <= sizeof(double complex));

	do {
        	ret = fi_fetch_atomic(shmem_transport_ofi_cntr_epfd,
                	source,
                	1,
			NULL,
			dest,
			NULL,
			GET_DEST(dst),
			(uint64_t) target,
			0,
			datatype,
			FI_ATOMIC_WRITE,
			NULL);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_get_counter++;
}


static inline
void
shmem_transport_cswap(void *target, void *source, void *dest, void *operand, size_t len,
                               int pe, int datatype)
{

	int ret = 0;
        uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;

        assert(len <= sizeof(double complex));

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
			(uint64_t) target,
			0,
			datatype,
			FI_CSWAP,
			NULL);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_get_counter++;
}


static inline
void
shmem_transport_mswap(void *target, void *source, void *dest, void *mask, size_t len,
                               int pe, int datatype)
{

	int ret = 0;
        uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;

	assert(len <= sizeof(double complex));

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
			(uint64_t) target,
			0,
			datatype,
			FI_MSWAP,
			NULL);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_get_counter++;
}


static inline
void
shmem_transport_atomic_small(void *target, void *source, size_t len,
                                       int pe, int op, int datatype)
{

	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;

	do {
		ret = fi_inject_atomic(shmem_transport_ofi_cntr_epfd,
			source,
			1,
			GET_DEST(dst),
			(uint64_t) target,
			0,
			datatype,
			op);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_put_counter++;
}


static inline
void
shmem_transport_atomic_nb(void *target, void *source, size_t full_len,
                                   int pe, int op, int datatype,
                                   long *completion)
{
	int ret = 0;
	uint64_t dst = (uint64_t) pe;
	size_t len = full_len/SHMEM_Dtsize[datatype];
	uint64_t polled = 0;

	if ( full_len <= shmem_transport_ofi_max_buffered_send) {

		polled = 0;

		do {
			ret = fi_inject_atomic(shmem_transport_ofi_cntr_epfd,
                		source,
                      		len, //count
				GET_DEST(dst),
                        	(uint64_t) target,
                        	0,
                        	datatype,
                        	op);
		} while(try_again(ret,&polled));

		shmem_transport_ofi_pending_put_counter++;

        } else if (full_len <=
			MIN(shmem_transport_ofi_bounce_buffer_size,
				shmem_transport_ofi_max_atomic_size)) {

			shmem_transport_ofi_bounce_buffer_t *buff = create_bounce_buffer(source, full_len);

			polled = 0;

		do {
			ret = fi_atomic(shmem_transport_ofi_epfd,
				buff->data,
				len,
				NULL,
				GET_DEST(dst),
				(uint64_t) target,
				0,
				datatype,
				op,
				&buff->frag.context);
		} while(try_again(ret,&polled));

			shmem_transport_ofi_pending_cq_count++;

        } else {
		size_t sent = 0;
		shmem_transport_ofi_long_frag_t *long_frag = create_long_frag(completion);

		while (sent < len) {

			while(shmem_transport_ofi_pending_cq_count >=
					(shmem_transport_ofi_queue_slots-2)) {
				shmem_transport_ofi_drain_cq();
			}

			size_t chunksize = MIN((len-sent),
				(shmem_transport_ofi_max_atomic_size/SHMEM_Dtsize[datatype]));

			polled = 0;
		do {
			ret = fi_atomic(shmem_transport_ofi_epfd,
				(void *)((char *)source +
					(sent*SHMEM_Dtsize[datatype])),
				chunksize,
				NULL,
				GET_DEST(dst),
				((uint64_t) target +
				 	(sent*SHMEM_Dtsize[datatype])),
				0,
				datatype,
				op,
				&long_frag->frag.context);
		} while(try_again(ret,&polled));

			(*(long_frag->completion))++;
			long_frag->reference++;
			shmem_transport_ofi_pending_cq_count++;
			sent += chunksize;
		}
        }
}


static inline
void
shmem_transport_fetch_atomic(void *target, void *source, void *dest, size_t len,
                                      int pe, int op, int datatype)
{
        int ret = 0;
        uint64_t dst = (uint64_t) pe;
	uint64_t polled = 0;

	assert(len <= sizeof(double complex));

	do {
        	ret = fi_fetch_atomic(shmem_transport_ofi_cntr_epfd,
			source,
			1,
			NULL,
			dest,
			NULL,
			GET_DEST(dst),
			(uint64_t) target,
			0,
			datatype,
			op,
			NULL);
	} while(try_again(ret,&polled));

	shmem_transport_ofi_pending_get_counter++;
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

#endif /* TRANSPORT_OFI_H */
