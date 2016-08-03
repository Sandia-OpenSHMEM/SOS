#ifndef TRANSPORT_NONE_H
#define TRANSPORT_NONE_H

// Datatypes
#define SHM_INTERNAL_FLOAT           -1
#define SHM_INTERNAL_DOUBLE          -1
#define SHM_INTERNAL_LONG_DOUBLE     -1
#define SHM_INTERNAL_FLOAT_COMPLEX   -1
#define SHM_INTERNAL_DOUBLE_COMPLEX  -1
#define SHM_INTERNAL_SIGNED_BYTE     -1
#define SHM_INTERNAL_INT32           -1
#define SHM_INTERNAL_INT64           -1
#define SHM_INTERNAL_SHORT           -1
#define SHM_INTERNAL_INT             -1
#define SHM_INTERNAL_LONG            -1
#define SHM_INTERNAL_LONG_LONG       -1
#define SHM_INTERNAL_FORTRAN_INTEGER -1

 // Operations
#define SHM_INTERNAL_BAND            -1
#define SHM_INTERNAL_BOR             -1
#define SHM_INTERNAL_BXOR            -1
#define SHM_INTERNAL_MIN             -1
#define SHM_INTERNAL_MAX             -1
#define SHM_INTERNAL_SUM             -1
#define SHM_INTERNAL_PROD            -1

typedef int shm_internal_datatype_t;
typedef int shm_internal_op_t;
typedef int shmem_transport_ct_t;

static inline
int
shmem_transport_init(long eager_size)
{
    return 0;
}

static inline
int
shmem_transport_startup(void)
{
    return 0;
}

static inline
int
shmem_transport_fini(void)
{
    return 0;
}

static inline
int
shmem_transport_quiet(void)
{
    return 0;
}

static inline
int
shmem_transport_fence(void)
{
    return 0;
}

static inline
void
shmem_transport_put_small(void *target, const void *source, size_t len, int pe)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_put_nb(void *target, const void *source, size_t len,
                       int pe, long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_put_wait(long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_put_nbi(void *target, const void *source, size_t len,
                       int pe)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_get(void *target, const void *source, size_t len, int pe)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_get_wait(void)
{
    RAISE_ERROR_STR("No path to peer");
}


static inline
void
shmem_transport_swap(void *target, const void *source, void *dest,
                     size_t len, int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_cswap(void *target, const void *source, void *dest,
                      const void *operand, size_t len, int pe,
                      shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_mswap(void *target, const void *source, void *dest,
                      const void *mask, size_t len, int pe,
                      shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_small(void *target, const void *source, size_t len,
                             int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_nb(void *target, const void *source, size_t len,
                          int pe, shm_internal_op_t op, shm_internal_datatype_t datatype,
                          long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_fetch_atomic(void *target, const void *source, void *dest, size_t len,
                             int pe, shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_fetch(void *target, const void *source, void *dest, size_t len,
                             int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_atomic_set(void *target, const void *source, void *dest, size_t len,
                             int pe, shm_internal_datatype_t datatype)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
int shmem_transport_atomic_supported(shm_internal_op_t op, shm_internal_datatype_t datatype)
{
    return 0;
}

static inline
void shmem_transport_ct_create(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_ct_free(shmem_transport_ct_t **ct_ptr)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
long shmem_transport_ct_get(shmem_transport_ct_t *ct)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_ct_set(shmem_transport_ct_t *ct, long value)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_ct_wait(shmem_transport_ct_t *ct, long wait_for)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void
shmem_transport_put_ct_nb(shmem_transport_ct_t *ct, void *target, const void
                          *source, size_t len, int pe, long *completion)
{
    RAISE_ERROR_STR("No path to peer");
}

static inline
void shmem_transport_get_ct(shmem_transport_ct_t *ct, void
                            *target, const void *source, size_t len, int pe)
{
    RAISE_ERROR_STR("No path to peer");
}

/**
 * Query the value of the transport's received messages counter.
 */
static inline
uint64_t shmem_transport_received_cntr_get(void)
{
    RAISE_ERROR_STR("No remote peers");
    return 0;
}

/**
 * Wait for the transport's received messages counter to be greater than or
 * equal to the given value.
 *
 * @param ge_val Function returns when received messages >= ge_val
 */
static inline
void shmem_transport_received_cntr_wait(uint64_t ge_val)
{
    RAISE_ERROR_STR("No remote peers");
}

#endif

