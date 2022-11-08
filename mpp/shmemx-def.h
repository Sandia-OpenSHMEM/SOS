#ifndef SHMEMX_DEF_H
#define SHMEMX_DEF_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

#define HAVE_SHMEMX_WTIME

/* Counting puts */
typedef char * shmemx_ct_t;

/* Counter */
typedef struct {
    uint64_t pending_put;
    uint64_t pending_get;
    uint64_t completed_put;
    uint64_t completed_get;
    uint64_t target;
} shmemx_pcntr_t;

/* Spaces */

typedef struct shmemx_impl_space_t {
    int dummy;
} *shmemx_space_t;

typedef struct {
    size_t space_size;
    int space_type;
} shmemx_space_config_t;

#if SHMEM_HAVE_ATTRIBUTE_VISIBILITY == 1
    __attribute__((visibility("default"))) extern shmemx_space_t SHMEMX_SPACE_DEFAULT;
#else
    extern shmemx_space_t SHMEMX_SPACE_DEFAULT;
#endif

#ifdef __cplusplus
}
#endif

#endif /* SHMEMX_DEF_H */
