#include "config.h"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* for asprintf */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <errno.h>

#include "shmem_env.h"
#include "shmem_internal.h"

struct shmem_internal_params_s shmem_internal_params;

/* atol() + optional scaled suffix recognition: 1K, 2M, 3G, 1T */
static long
atol_scaled(char *s)
{
    long val;
    char *e;
    errno = 0;

    val = strtol(s,&e,0);
    if(errno != 0 || e == s) {
        shmem_runtime_abort(1, "env var conversion");
    }
    if (e == NULL || *e =='\0')
        return val;

    if (*e == 'K' || *e == 'k')
        val *= 1024L;
    else if (*e == 'M' || *e == 'm')
        val *= 1024L*1024L;
    else if (*e == 'G' || *e == 'g')
        val *= 1024L*1024L*1024L;
    else if (*e == 'T' || *e == 't')
        val *= 1024L*1024L*1024L*1024L;

    return val;
}


static long
errchk_atol(char *s)
{
    long val;
    char *e;
    errno = 0;

    val = strtol(s,&e,0);
    if(errno != 0) {
        perror("env var conversion");
        exit(1);
    }

    return val;
}

static char *
shmem_internal_getenv(const char* name)
{
    char *env_name, *env_value;
    int ret;

    ret = asprintf(&env_name, "SHMEM_%s", name);
    if (ret < 0) {
        RAISE_ERROR(ret);
    }
    env_value = getenv(env_name);
    free(env_name);
    if (env_value != NULL) {
        return env_value;
    }

    ret = asprintf(&env_name, "SMA_%s", name);
    if (ret < 0) {
        RAISE_ERROR(ret);
    }
    env_value = getenv(env_name);
    free(env_name);
    if (env_value != NULL) {
        return env_value;
    }

    return NULL;
}

static void
shmem_internal_getenv_string(const char *name,
                             const shmem_internal_env_string default_val,
                             shmem_internal_env_string *out, bool *provided) {
    char *env = shmem_internal_getenv(name);
    *provided = (env != NULL);
    *out = (*provided) ? env : default_val;
}

static void
shmem_internal_getenv_long(const char *name,
                           shmem_internal_env_long default_val,
                           shmem_internal_env_long *out, bool *provided) {
    char *env = shmem_internal_getenv(name);
    *provided = (env != NULL);
    *out = (*provided) ? errchk_atol(env) : default_val;
}

static void
shmem_internal_getenv_size(const char *name,
                           shmem_internal_env_size default_val,
                           shmem_internal_env_size *out, bool *provided) {
    char *env = shmem_internal_getenv(name);
    *provided = (env != NULL);
    *out = (*provided) ? atol_scaled(env) : default_val;
}

static void
shmem_internal_getenv_bool(const char *name,
                           shmem_internal_env_bool default_val,
                           shmem_internal_env_bool *out, bool *provided) {
    char *env = shmem_internal_getenv(name);
    *provided = (env != NULL);
    *out = (*provided) ? !default_val : default_val;
}

void shmem_internal_parse_env(void) {
#define SHMEM_INTERNAL_ENV_DEF(NAME, KIND, DEFAULT, CATEGORY, SHORT_DESC) \
    shmem_internal_getenv_##KIND(#NAME, DEFAULT, &(shmem_internal_params.NAME), &(shmem_internal_params.NAME##_provided));
#include "shmem_env_defs.h"
#undef SHMEM_INTERNAL_ENV_DEF
}

void shmem_internal_print_env(void) {
#define SHMEM_INTERNAL_ENV_DEF(NAME, KIND, DEFAULT, CATEGORY, SHORT_DESC)       \
    if (CATEGORY == SHMEM_INTERNAL_ENV_CAT_OPENSHMEM)                               \
        printf("  SHMEM_%-20s %"SHPRI_##KIND" (type: %s, default: %"SHPRI_##KIND")\n\t%s\n", \
               #NAME, shmem_internal_params.NAME, #KIND, (shmem_internal_env_##KIND) DEFAULT, SHORT_DESC);
#include "shmem_env_defs.h"
#undef SHMEM_INTERNAL_ENV_DEF

printf("\nAdditional options:\n");
#define SHMEM_INTERNAL_ENV_DEF(NAME, KIND, DEFAULT, CATEGORY, SHORT_DESC)       \
    if (CATEGORY == SHMEM_INTERNAL_ENV_CAT_OTHER)                                   \
        printf("  SHMEM_%-20s %"SHPRI_##KIND" (type: %s, default: %"SHPRI_##KIND")\n\t%s\n", \
               #NAME, shmem_internal_params.NAME, #KIND, (shmem_internal_env_##KIND) DEFAULT, SHORT_DESC);
#include "shmem_env_defs.h"
#undef SHMEM_INTERNAL_ENV_DEF

printf("\nCollectives options:\n");
#define SHMEM_INTERNAL_ENV_DEF(NAME, KIND, DEFAULT, CATEGORY, SHORT_DESC)       \
    if (CATEGORY == SHMEM_INTERNAL_ENV_CAT_COLLECTIVES)                             \
        printf("  SHMEM_%-20s %"SHPRI_##KIND" (type: %s, default: %"SHPRI_##KIND")\n\t%s\n", \
               #NAME, shmem_internal_params.NAME, #KIND, (shmem_internal_env_##KIND) DEFAULT, SHORT_DESC);
#include "shmem_env_defs.h"
#undef SHMEM_INTERNAL_ENV_DEF

printf("\nNetwork transport: %s\n",
#if defined(USE_OFI)
       "OFI"
#elif defined(USE_PORTALS4)
       "Portals 4"
#else
       "none"
#endif
       );

#define SHMEM_INTERNAL_ENV_DEF(NAME, KIND, DEFAULT, CATEGORY, SHORT_DESC)       \
    if (CATEGORY == SHMEM_INTERNAL_ENV_CAT_TRANSPORT)                               \
        printf("  SHMEM_%-20s %"SHPRI_##KIND" (type: %s, default: %"SHPRI_##KIND")\n\t%s\n", \
               #NAME, shmem_internal_params.NAME, #KIND, (shmem_internal_env_##KIND) DEFAULT, SHORT_DESC);
#include "shmem_env_defs.h"
#undef SHMEM_INTERNAL_ENV_DEF

printf("\nOn-node transport: %s\n",
#if defined(USE_CMA)
       "Linux CMA"
#elif defined(USE_XPMEM)
       "XPMEM"
#elif defined(USE_MEMCPY)
       "memcpy"
#else
       "none"
#endif
       );

#define SHMEM_INTERNAL_ENV_DEF(NAME, KIND, DEFAULT, CATEGORY, SHORT_DESC)       \
    if (CATEGORY == SHMEM_INTERNAL_ENV_CAT_INTRANODE)                               \
        printf("  SHMEM_%-20s %"SHPRI_##KIND" (type: %s, default: %"SHPRI_##KIND")\n\t%s\n", \
               #NAME, shmem_internal_params.NAME, #KIND, (shmem_internal_env_##KIND) DEFAULT, SHORT_DESC);
#include "shmem_env_defs.h"
#undef SHMEM_INTERNAL_ENV_DEF
}
