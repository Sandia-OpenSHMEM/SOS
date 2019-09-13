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

/* Teams */
typedef struct shmem_impl_team_t {
    int dummy;
} * shmemx_team_t;

typedef struct {
    int num_contexts;
} shmemx_team_config_t;

#if SHMEM_HAVE_ATTRIBUTE_VISIBILITY == 1
    __attribute__((visibility("default"))) extern shmemx_team_t SHMEMX_TEAM_WORLD;
    __attribute__((visibility("default"))) extern shmemx_team_t SHMEMX_TEAM_SHARED;
    __attribute__((visibility("default"))) extern shmemx_team_t SHMEMX_TEAM_HOST;
    __attribute__((visibility("default"))) extern shmemx_team_t SHMEMX_TEAM_LEADERS;
#else
    extern shmemx_team_t SHMEMX_TEAM_WORLD;
    extern shmemx_team_t SHMEMX_TEAM_SHARED;
    extern shmemx_team_t SHMEMX_TEAM_HOST;
    extern shmemx_team_t SHMEMX_TEAM_LEADERS;
#endif

#define SHMEMX_TEAM_INVALID NULL

#define SHMEMX_CTX_INVALID NULL

#define SHMEMX_TEAM_NUM_CONTEXTS       (1l<<0)

#ifdef __cplusplus
}
#endif

#endif /* SHMEMX_DEF_H */
