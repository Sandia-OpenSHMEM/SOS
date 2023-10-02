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

#if SHMEM_HAVE_ATTRIBUTE_VISIBILITY == 1
    __attribute__((visibility("default"))) extern shmem_team_t SHMEMX_TEAM_HOST;
#else
    extern shmem_team_t SHMEMX_TEAM_HOST;
#endif

#ifdef __cplusplus
}
#endif

#endif /* SHMEMX_DEF_H */
