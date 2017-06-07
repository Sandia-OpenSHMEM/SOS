#include "config.h"

#define _GNU_SOURCE /* for asprintf */
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


static inline long
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


static long
shmem_internal_getenv_long_sized(const char* name, int is_sized, long default_value)
{
    char *env_name, *env_value;
    int ret;

    ret = asprintf(&env_name, "SMA_%s", name);
    if (ret < 0) {
        RAISE_ERROR(ret);
    }
    env_value = getenv(env_name);
    free(env_name);
    if (env_value != NULL) {
        if (is_sized) {
            return atol_scaled(env_value);
        } else {
            return errchk_atol(env_value);
        }
    }

    ret = asprintf(&env_name, "SHMEM_%s", name);
    if (ret < 0) {
        RAISE_ERROR(ret);
    }
    env_value = getenv(env_name);
    free(env_name);
    if (env_value != NULL) {
        if (is_sized) {
            return atol_scaled(env_value);
        } else {
            return errchk_atol(env_value);
        }
    }

    return default_value;
}


static char *
shmem_internal_getenv_str(const char* name)
{
    char *env_name, *env_value;
    int ret;

    ret = asprintf(&env_name, "SMA_%s", name);
    if (ret < 0) {
        RAISE_ERROR(ret);
    }
    env_value = getenv(env_name);
    free(env_name);
    if (env_value != NULL) {
        return env_value;
    }

    ret = asprintf(&env_name, "SHMEM_%s", name);
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
static const char * shmem_internal_getenv_string(const char *name, const char *default_val) {
    char *env = shmem_internal_getenv_str(name);
    if (env != NULL)
        return env;
    else
        return default_val;
}

static long shmem_internal_getenv_long(const char *name, long default_val) {
    return shmem_internal_getenv_long_sized(name, 0, default_val);
}

static size_t shmem_internal_getenv_size(const char *name, size_t default_val) {
    return shmem_internal_getenv_long_sized(name, 1, default_val);
}

static bool shmem_internal_getenv_bool(const char *name, bool default_val) {
    char *env = shmem_internal_getenv_str(name);
    return (env != NULL) ? !default_val : default_val;
}

void shmem_internal_parse_env(void) {
#define SHMEM_INTERNAL_ENV_DEF(NAME, KIND, DEFAULT, CATEGORY, SHORT_DESC) \
    shmem_internal_params.NAME = shmem_internal_getenv_##KIND(#NAME, DEFAULT, &(shmem_internal_params.NAME##_provided));
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
#elif defined(XPMEM)
       "XPMEM"
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
