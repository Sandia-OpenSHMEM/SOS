#ifndef CONTEXT_H_INCL
#define CONTEXT_H_INCL

#include <stddef.h>

#define SHMEMX_CTX_DEFAULT 0
#define SHMEMX_DOMAIN_DEFAULT 0

typedef int shmemx_ctx_t;
typedef int shmemx_domain_t;

void shmemx_domain_create(int thread_level, int num_domains, shmemx_domain_t domains[]);

void shmemx_domain_destroy(int num_domains, shmemx_domain_t domains[]);
int shmemx_ctx_create(shmemx_domain_t domain, shmemx_ctx_t *ctx);
void shmemx_ctx_destroy(shmemx_ctx_t ctx);
void shmemx_ctx_fence(shmemx_ctx_t ctx);
void shmemx_ctx_quiet(shmemx_ctx_t ctx);

void shmemx_sync(int PE_start, int logPE_stride, int PE_size, long *pSync);
void shmemx_sync_all(void); 

#define SHMEMX_PUTGET_ALIGN_OPS_DECLARE(STYPE) \
    void shmemx_ctx_put##STYPE(void *dest, const void *source, size_t nelems, int pe, shmemx_ctx_t ctx);                                      \
    void shmemx_ctx_iput##STYPE(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx);       \
    void shmemx_ctx_get##STYPE(void *dest, const void *source, size_t nelems, int pe, shmemx_ctx_t ctx);                                      \
    void shmemx_ctx_iget##STYPE(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx);       \
    void shmemx_ctx_iget##STYPE(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx);       \
    void shmemx_ctx_put##STYPE##_nb(void *dest, const void *source, size_t nelems, int pe, shmemx_ctx_t ctx);                                 \
    void shmemx_ctx_iput##STYPE##_nb(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx);  \
    void shmemx_ctx_get##STYPE##_nb(void *dest, const void *source, size_t nelems, int pe, shmemx_ctx_t ctx);                                 \
    void shmemx_ctx_iget##STYPE##_nb(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx);  \
    void shmemx_ctx_iget##STYPE##_nb(void *dest, const void *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx);

#define SHMEMX_PG_OPS_DECLARE(STYPE, CTYPE) \
    void shmemx_ctx_##STYPE##_p(CTYPE *addr, CTYPE value, int pe, shmemx_ctx_t ctx);                                                          \
    CTYPE shmemx_ctx_##STYPE##_g(CTYPE *addr, int pe, shmemx_ctx_t ctx);

#define SHMEMX_PUTGET_OPS_DECLARE(STYPE, CTYPE) \
    void shmemx_ctx_##STYPE##_put(CTYPE *dest, const CTYPE *source, size_t nelems, int pe, shmemx_ctx_t ctx);                                 \
    void shmemx_ctx_##STYPE##_iput(CTYPE *dest, const CTYPE *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx);  \
    void shmemx_ctx_##STYPE##_put_nb(CTYPE *dest, const CTYPE *source, size_t nelems, int pe, shmemx_ctx_t ctx);                              \
    void shmemx_ctx_##STYPE##_iput_nb(CTYPE *dest, const CTYPE *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx); \
                                                                                                                                            \
    void shmemx_ctx_##STYPE##_get(CTYPE *dest, const CTYPE *source, size_t nelems, int pe, shmemx_ctx_t ctx);                                 \
    void shmemx_ctx_##STYPE##_iget(CTYPE *dest, const CTYPE *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx);  \
    void shmemx_ctx_##STYPE##_get_nb(CTYPE *dest, const CTYPE *source, size_t nelems, int pe, shmemx_ctx_t ctx);                              \
    void shmemx_ctx_##STYPE##_iget_nb(CTYPE *dest, const CTYPE *source, ptrdiff_t dst, ptrdiff_t sst, size_t nelems, int pe, shmemx_ctx_t ctx);

#define SHMEMX_ATOMIC_OPS_DECLARE(STYPE, CTYPE) \
    void shmemx_##STYPE##_add(CTYPE *dest, CTYPE value, int pe, shmemx_ctx_t ctx);                                                            \
    CTYPE shmemx_##STYPE##_cswap(CTYPE *dest, CTYPE cond, CTYPE value, int pe, shmemx_ctx_t ctx);                                             \
    CTYPE shmemx_##STYPE##_finc(CTYPE *dest, int pe, shmemx_ctx_t ctx);                                                                       \
    void shmemx_##STYPE##_inc(CTYPE *dest, int pe, shmemx_ctx_t ctx);                                                                         \
    CTYPE shmemx_##STYPE##_fadd(CTYPE *dest, CTYPE value, int pe, shmemx_ctx_t ctx);                                                          \

#define SHMEMX_SWAP_OP_DECLARE(STYPE, CTYPE) \
    CTYPE shmemx_##STYPE##_swap(CTYPE *dest, CTYPE value, int pe, shmemx_ctx_t ctx);

SHMEMX_PUTGET_ALIGN_OPS_DECLARE(32)
SHMEMX_PUTGET_ALIGN_OPS_DECLARE(64)
SHMEMX_PUTGET_ALIGN_OPS_DECLARE(128)
SHMEMX_PUTGET_ALIGN_OPS_DECLARE(mem)

SHMEMX_PG_OPS_DECLARE(char, char)
SHMEMX_PG_OPS_DECLARE(short, short)
SHMEMX_PG_OPS_DECLARE(int, int)
SHMEMX_PG_OPS_DECLARE(long, long)
SHMEMX_PG_OPS_DECLARE(longlong, long long)
SHMEMX_PG_OPS_DECLARE(float, float)
SHMEMX_PG_OPS_DECLARE(double, double)
SHMEMX_PG_OPS_DECLARE(longdouble, long double)

SHMEMX_PUTGET_OPS_DECLARE(short, short)
SHMEMX_PUTGET_OPS_DECLARE(int, int)
SHMEMX_PUTGET_OPS_DECLARE(long, long)
SHMEMX_PUTGET_OPS_DECLARE(longlong, long long)
SHMEMX_PUTGET_OPS_DECLARE(float, float)
SHMEMX_PUTGET_OPS_DECLARE(double, double)
SHMEMX_PUTGET_OPS_DECLARE(longdouble, long double)

SHMEMX_ATOMIC_OPS_DECLARE(int, int)
SHMEMX_ATOMIC_OPS_DECLARE(long, long)
SHMEMX_ATOMIC_OPS_DECLARE(longlong, long long)

SHMEMX_SWAP_OP_DECLARE(int, int)
SHMEMX_SWAP_OP_DECLARE(long, long)
SHMEMX_SWAP_OP_DECLARE(longlong, long long)
SHMEMX_SWAP_OP_DECLARE(float, float)
SHMEMX_SWAP_OP_DECLARE(double, double)

long shmemx_ctx_swap(long *dest, long value, int pe, shmemx_ctx_t ctx);

#endif /* CONTEXT_H_INCL */
