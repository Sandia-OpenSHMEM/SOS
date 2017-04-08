#ifndef SHMEM_ENV_H
#define SHMEM_ENV_H

#include <stdbool.h>
#include <stddef.h>
#include "config.h"

typedef long   shmem_internal_env_long;
typedef size_t shmem_internal_env_size;
typedef bool   shmem_internal_env_bool;
typedef const char* shmem_internal_env_string;

#define SHPRI_long "ld"
#define SHPRI_size "zd"
#define SHPRI_bool "d"
#define SHPRI_string "s"

enum shmem_internal_env_categories {
    SHMEM_INTERNAL_ENV_CAT_OPENSHMEM, SHMEM_INTERNAL_ENV_CAT_COLLECTIVES,
    SHMEM_INTERNAL_ENV_CAT_TRANSPORT, SHMEM_INTERNAL_ENV_CAT_INTRANODE,
    SHMEM_INTERNAL_ENV_CAT_OTHER
};

#define SHMEM_INTERNAL_ENV_DEF(NAME, KIND, DEFAULT, CATEGORY, SHORT_DESC) \
  shmem_internal_env_##KIND NAME;

struct shmem_internal_params_s {
#include "shmem_env_defs.h"
};

#undef SHMEM_INTERNAL_ENV_DEF

extern struct shmem_internal_params_s shmem_internal_params;

void shmem_internal_parse_env();

#endif /* SHMEM_ENV_H */
