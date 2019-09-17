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
#if USE_SHR_ATOMICS
    return -1 != shmem_internal_get_shr_rank(pe);
#else
    return 0;
#endif
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
#if USE_SHR_ATOMICS
    int noderank = shmem_internal_get_shr_rank(pe);
    char *remote_ptr;

    if (noderank == -1)
        RAISE_ERROR_MSG("No shared memory path to peer %d\n", pe);

    XPMEM_GET_REMOTE_ACCESS(target, noderank, remote_ptr);

    switch (datatype) {
        case SHM_INTERNAL_INT:
            {
                __atomic_exchange((int *)remote_ptr, (int *)source, (int *)dest, __ATOMIC_SEQ_CST);
            }
            break;
        case SHM_INTERNAL_LONG:
            {
                __atomic_exchange((long *)remote_ptr, (long *)source, (long *)dest, __ATOMIC_SEQ_CST);
            }
            break;
        case SHM_INTERNAL_UINT:
            {
                __atomic_exchange((unsigned int *)remote_ptr, (unsigned int *)source, (unsigned int *)dest, __ATOMIC_SEQ_CST);
            }
            break;
        case SHM_INTERNAL_ULONG:
            {
                __atomic_exchange((unsigned long *)remote_ptr, (unsigned long *)source, (unsigned long *)dest, __ATOMIC_SEQ_CST);
            }
            break;
        case SHM_INTERNAL_FLOAT:
            {
                __atomic_exchange((float *)remote_ptr, (float *)source, (float *)dest, __ATOMIC_SEQ_CST);
            }
            break;
        case SHM_INTERNAL_DOUBLE:
            {
                __atomic_exchange((double *)remote_ptr, (double *)source, (double *)dest, __ATOMIC_SEQ_CST);
            }
            break;
        default:
            RAISE_ERROR_MSG("Unsupported datatype dtype=%d\n", datatype);
    }
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void
shmem_shr_transport_cswap(shmem_ctx_t ctx, void *target, void *source,
                          void *dest, void *operand, size_t len,
                          int pe, shm_internal_datatype_t datatype)
{
#if USE_SHR_ATOMICS
    int noderank = shmem_internal_get_shr_rank(pe);
    char *remote_ptr;

    if (noderank == -1)
        RAISE_ERROR_MSG("No shared memory path to peer %d\n", pe);

    XPMEM_GET_REMOTE_ACCESS(target, noderank, remote_ptr);

    switch (datatype) {
        case SHM_INTERNAL_INT:
            {
                bool done = false;

                while (!done) {
                    int v = __atomic_load_n((int *)remote_ptr, __ATOMIC_SEQ_CST);
                    if (v == *(int*)operand) {
                        done = __atomic_compare_exchange((int *)remote_ptr,
                                                         (int *)operand,
                                                         (int *)source,
                                                         false,
                                                         __ATOMIC_SEQ_CST,
                                                         __ATOMIC_SEQ_CST);
                    } else {
                        /* Compare failed */
                        done = true;
                    }
                    *(int *)dest = v;
                }
            }
            break;
        case SHM_INTERNAL_LONG:
            {
                bool done = false;

                while (!done) {
                    long v = __atomic_load_n((long *)remote_ptr, __ATOMIC_SEQ_CST);
                    if (v == *(long*)operand) {
                        done = __atomic_compare_exchange((long *)remote_ptr,
                                                         (long *)operand,
                                                         (long *)source,
                                                         false,
                                                         __ATOMIC_SEQ_CST,
                                                         __ATOMIC_SEQ_CST);
                    } else {
                        /* Compare failed */
                        done = true;
                    }
                    *(long *)dest = v;
                }
            }
            break;
        default:
            RAISE_ERROR_MSG("Unsupported datatype dtype=%d\n", datatype);
    }
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void
shmem_shr_transport_mswap(shmem_ctx_t ctx, void *target, void *source,
                          void *dest, void *mask, size_t len,
                          int pe, shm_internal_datatype_t datatype)
{
#if USE_SHR_ATOMICS
    int noderank = shmem_internal_get_shr_rank(pe);
    char *remote_ptr;

    if (noderank == -1)
        RAISE_ERROR_MSG("No shared memory path to peer %d\n", pe);

    XPMEM_GET_REMOTE_ACCESS(target, noderank, remote_ptr);

    switch (datatype) {
        bool done = false;

        case SHM_INTERNAL_INT:
            while (!done) {
                int v = __atomic_load_n((int *)remote_ptr, __ATOMIC_SEQ_CST);

                int new = ((unsigned int) v & !*(unsigned int *)mask) | (*(unsigned int *)source & *(unsigned int *)mask);

                printf("%d: Swap (%d, %p) %x -> %x\n", shmem_internal_my_pe, pe, target, (unsigned int)v, (unsigned int)new);

                done = __atomic_compare_exchange((int *)remote_ptr,
                                                 &v,
                                                 &new,
                                                 false,
                                                 __ATOMIC_SEQ_CST,
                                                 __ATOMIC_SEQ_CST);
                *(int *)dest = v;
            }
            break;
        default:
            RAISE_ERROR_STR("No path to peer");
    }
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void
shmem_shr_transport_atomic(shmem_ctx_t ctx, void *target, const void *source,
                           size_t len, int pe, shm_internal_op_t op,
                           shm_internal_datatype_t datatype)
{
#if USE_SHR_ATOMICS
    int noderank = shmem_internal_get_shr_rank(pe);
    char *remote_ptr;

    if (noderank == -1)
        RAISE_ERROR_MSG("No shared memory path to peer %d\n", pe);

    XPMEM_GET_REMOTE_ACCESS(target, noderank, remote_ptr);

    switch (op) {
    case SHM_INTERNAL_SUM:
        switch (datatype) {
            case SHM_INTERNAL_INT:
                {
                    __atomic_fetch_add((int *)remote_ptr, *((int *)source), __ATOMIC_SEQ_CST);
                }
                break;
            case SHM_INTERNAL_LONG:
                {
                    __atomic_fetch_add((long *)remote_ptr, *((long *)source), __ATOMIC_SEQ_CST); /* FIXME: ACQ_REL */
                }
                break;
            default:
                RAISE_ERROR_MSG("Unsupported datatype op=%d, dtype=%d\n", op, datatype);
        }
        break;
    default:
        RAISE_ERROR_MSG("Unsupported atomic operation %d\n", op);
    }
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void
shmem_shr_transport_atomic_fetch(shmem_ctx_t ctx, void *target,
                                 const void *source, size_t len,
                                 int pe, shm_internal_datatype_t datatype)
{
#if USE_SHR_ATOMICS
    RAISE_ERROR_STR("No path to peer");
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void
shmem_shr_transport_atomic_set(shmem_ctx_t ctx, void *target,
                               const void *source, size_t len,
                               int pe, shm_internal_datatype_t datatype)
{
#if USE_SHR_ATOMICS
    RAISE_ERROR_STR("No path to peer");
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}


static inline
void
shmem_shr_transport_atomicv(shmem_ctx_t ctx, void *target, const void *source,
                            size_t len, int pe, shm_internal_op_t op,
                            shm_internal_datatype_t datatype)
{
#if USE_SHR_ATOMICS
    RAISE_ERROR_STR("No path to peer");
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}



static inline void
shmem_shr_transport_fetch_atomic(shmem_ctx_t ctx, void *target, void *source,
                                 void *dest, size_t len, int pe,
                                 shm_internal_op_t op,
                                 shm_internal_datatype_t datatype)
{
#if USE_SHR_ATOMICS
    int noderank = shmem_internal_get_shr_rank(pe);
    char *remote_ptr;

    if (noderank == -1)
        RAISE_ERROR_MSG("No shared memory path to peer %d\n", pe);

    XPMEM_GET_REMOTE_ACCESS(target, noderank, remote_ptr);

    switch (op) {
    case SHM_INTERNAL_SUM:
        switch (datatype) {
            case SHM_INTERNAL_INT:
                {
                    *(int *)dest = __atomic_fetch_add((int *)remote_ptr, *((int *)source), __ATOMIC_SEQ_CST);
                }
                break;
            case SHM_INTERNAL_LONG:
                {
                    *(long *)dest = __atomic_fetch_add((long *)remote_ptr, *((long *)source), __ATOMIC_SEQ_CST); /* FIXME: ACQ_REL */
                }
                break;
            default:
                RAISE_ERROR_MSG("Unsupported datatype op=%d, dtype=%d\n", op, datatype);
        }
        break;
    default:
        RAISE_ERROR_MSG("Unsupported atomic operation %d\n", op);
    }
#else
    RAISE_ERROR_STR("No path to peer");
#endif
}

#endif /* SHR_TRANSPORT_H */
