#ifndef SHR_TRANSPORT_H
#define SHR_TRANSPORT_H

#ifdef USE_XPMEM
#include "transport_xpmem.h"
#endif

#ifdef USE_CMA
#include "transport_cma.h"
#endif


static inline int
shmem_shr_transport_init(void)
{
    int ret = 0;

#if USE_XPMEM
    ret = shmem_transport_xpmem_init();
    if (0 != ret)
        RETURN_ERROR_MSG("XPMEM init failed (%d)\n", ret);

#elif USE_CMA
    ret = shmem_transport_cma_init();
    if (0 != ret)
        RETURN_ERROR_MSG("CMA init failed (%d)\n", ret);
#endif

    return ret;
}


static inline int
shmem_shr_transport_startup(void)
{
    int ret = 0;

#if USE_XPMEM
    ret = shmem_transport_xpmem_startup();
    if (0 != ret) {
        RETURN_ERROR_MSG("XPMEM startup failed (%d)\n", ret);
    }

#elif USE_CMA
    ret = shmem_transport_cma_startup();
    if (0 != ret) {
        RETURN_ERROR_MSG("CMA startup failed (%d)\n", ret);
    }
#endif

    return ret;
}


static inline void
shmem_shr_transport_fini(void)
{
#if USE_XPMEM
    shmem_transport_xpmem_fini();
#elif USE_CMA
    shmem_transport_cma_fini();
#endif
}


static inline int
shmem_shr_transport_use_write(shmem_ctx_t ctx, void *target, const void *source,
                              size_t len, int pe)
{
#if USE_CMA
    return  -1 != shmem_internal_get_shr_rank(pe) &&
           len <= shmem_internal_params.CMA_PUT_MAX;
#else
    return -1 != shmem_internal_get_shr_rank(pe);
#endif
}


static inline int
shmem_shr_transport_use_read(shmem_ctx_t ctx, void *target, const void *source,
                             size_t len, int pe)
{
#if USE_CMA
    return  -1 != shmem_internal_get_shr_rank(pe) &&
           len <= shmem_internal_params.CMA_GET_MAX;
#else
    return -1 != shmem_internal_get_shr_rank(pe);
#endif
}


static inline int
shmem_shr_transport_use_atomic(shmem_ctx_t ctx, void *target, const void *source,
                               size_t len, int pe, shm_internal_datatype_t datatype)
{
    return 0;
}


static inline void
shmem_shr_transport_put_scalar(shmem_ctx_t ctx, void *target,
                               const void *source, size_t len, int pe)
{
#if USE_MEMCPY
    memcpy(target, source, len);
#elif USE_XPMEM
    shmem_transport_xpmem_put(target, source, len, pe,
                              shmem_internal_get_shr_rank(pe));
#elif USE_CMA
    shmem_transport_cma_put(target, source, len, pe,
                            shmem_internal_get_shr_rank(pe));
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline void
shmem_shr_transport_put(shmem_ctx_t ctx, void *target, const void *source,
                        size_t len, int pe)
{
#if USE_MEMCPY
    memcpy(target, source, len);
#elif USE_XPMEM
    shmem_transport_xpmem_put(target, source, len, pe,
                              shmem_internal_get_shr_rank(pe));
#elif USE_CMA
    shmem_transport_cma_put(target, source, len, pe,
                            shmem_internal_get_shr_rank(pe));
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline void
shmem_shr_transport_put_signal(shmem_ctx_t ctx, void *target,
                               const void *source, size_t len,
                               uint64_t *sig_addr, uint64_t signal, int pe)
{
#if USE_MEMCPY
    memcpy(target, source, len);
    *sig_addr = signal;
#elif USE_XPMEM
    shmem_transport_xpmem_put(target, source, len, pe,
                              shmem_internal_get_shr_rank(pe));
    shmem_internal_membar_store(); /* Memory fence to ensure target PE observes
                                      stores in the correct order */
    shmem_transport_xpmem_put(sig_addr, &signal, sizeof(uint64_t), pe,
                              shmem_internal_get_shr_rank(pe));
#elif USE_CMA
    shmem_transport_cma_put(target, source, len, pe,
                            shmem_internal_get_shr_rank(pe));
    shmem_internal_membar_store(); /* Memory fence to ensure target PE observes
                                      stores in the correct order */
    shmem_transport_cma_put(sig_addr, &signal, sizeof(uint64_t), pe,
                            shmem_internal_get_shr_rank(pe));
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline void
shmem_shr_transport_get(shmem_ctx_t ctx, void *target, const void *source,
                        size_t len, int pe)
{
#if USE_MEMCPY
    memcpy(target, source, len);
#elif USE_XPMEM
    shmem_transport_xpmem_get(target, source, len, pe,
                              shmem_internal_get_shr_rank(pe));
#elif USE_CMA
    shmem_transport_cma_get(target, source, len, pe,
                            shmem_internal_get_shr_rank(pe));
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline void
shmem_shr_transport_swap(shmem_ctx_t ctx, void *target, void *source,
                         void *dest, size_t len, int pe,
                         shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}


static inline
void
shmem_shr_transport_cswap(shmem_ctx_t ctx, void *target, void *source,
                          void *dest, void *operand, size_t len,
                          int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}


static inline
void
shmem_shr_transport_mswap(shmem_ctx_t ctx, void *target, void *source,
                          void *dest, void *mask, size_t len,
                          int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}


static inline
void
shmem_shr_transport_atomic(shmem_ctx_t ctx, void *target, const void *source,
                           size_t len, int pe, shm_internal_op_t op,
                           shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}


static inline
void
shmem_shr_transport_atomic_fetch(shmem_ctx_t ctx, void *target,
                                 const void *source, size_t len,
                                 int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}


static inline
void
shmem_shr_transport_atomic_set(shmem_ctx_t ctx, void *target,
                               const void *source, size_t len,
                               int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}


static inline
void
shmem_shr_transport_atomicv(shmem_ctx_t ctx, void *target, const void *source,
                            size_t len, int pe, shm_internal_op_t op,
                            shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}



static inline void
shmem_shr_transport_fetch_atomic(shmem_ctx_t ctx, void *target, void *source,
                                 void *dest, size_t len, int pe,
                                 shm_internal_op_t op,
                                 shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

#endif /* SHR_TRANSPORT_H */
