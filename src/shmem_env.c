/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#ifndef _GNU_SOURCE
#define _GNU_SOURCE /* for asprintf */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <errno.h>
#include <math.h>

#include "shmem_env.h"
#include "shmem_internal.h"

struct shmem_internal_params_s shmem_internal_params;

/* atol() + optional scaled suffix recognition: 1K, 2M, 3G, 1T */
static int
atol_scaled(char *str, shmem_internal_env_size *out)
{
    int scale, n;
    double p = -1.0;
    char f;

    n = sscanf(str, "%lf%c", &p, &f);

    if (n == 2) {
        switch (f) {
            case 'k':
            case 'K':
                scale = 10;
                break;
            case 'm':
            case 'M':
                scale = 20;
                break;
            case 'g':
            case 'G':
                scale = 30;
                break;
            case 't':
            case 'T':
                scale = 40;
                break;
            default:
                return 1;
        }
    }
    else if (p < 0) {
        return 1;
    } else
        scale = 0;

    *out = (shmem_internal_env_size) ceil(p * (1lu << scale));
    return 0;
}


static long
errchk_atol(char *s)
{
    long val;
    char *e;
    errno = 0;

    val = strtol(s,&e,0);
    if (errno != 0 || e == s) {
        RAISE_ERROR_MSG("Environment variable conversion failed (%s)\n", s);
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
        RAISE_ERROR_MSG("Error in asprintf: SHMEM_%s\n", name);
    }
    env_value = getenv(env_name);
    free(env_name);
    if (env_value != NULL) {
        return env_value;
    }

    ret = asprintf(&env_name, "SMA_%s", name);
    if (ret < 0) {
        RAISE_ERROR_MSG("Error in asprintf: SMA_%s\n", name);
    }
    env_value = getenv(env_name);
    free(env_name);
    if (env_value != NULL) {
        return env_value;
    }

    return NULL;
}

static int
shmem_internal_getenv_string(const char *name,
                             const shmem_internal_env_string default_val,
                             shmem_internal_env_string *out, bool *provided) {
    char *env = shmem_internal_getenv(name);
    *provided = (env != NULL);
    *out = (*provided) ? env : default_val;
    return 0;
}

static int
shmem_internal_getenv_long(const char *name,
                           shmem_internal_env_long default_val,
                           shmem_internal_env_long *out, bool *provided) {
    char *env = shmem_internal_getenv(name);
    *provided = (env != NULL);
    *out = (*provided) ? errchk_atol(env) : default_val;
    return 0;
}

static int
shmem_internal_getenv_size(const char *name,
                           shmem_internal_env_size default_val,
                           shmem_internal_env_size *out, bool *provided) {
    char *env = shmem_internal_getenv(name);
    *provided = (env != NULL);
    if (*provided) {
        int ret = atol_scaled(env, out);
        if (ret) {
            RAISE_WARN_MSG("Invalid size in environment variable '%s' (%s)\n", name, env);
            return ret;
        }
    }
    else
        *out = default_val;
    return 0;
}

static int
shmem_internal_getenv_bool(const char *name,
                           shmem_internal_env_bool default_val,
                           shmem_internal_env_bool *out, bool *provided) {
    char *env = shmem_internal_getenv(name);
    *provided = (env != NULL);
    *out = (*provided) ? !default_val : default_val;
    return 0;
}

int shmem_internal_parse_env(void) {
    int ret;
#define SHMEM_INTERNAL_ENV_DEF(NAME, KIND, DEFAULT, CATEGORY, SHORT_DESC) \
    ret = shmem_internal_getenv_##KIND(#NAME, DEFAULT, &(shmem_internal_params.NAME), &(shmem_internal_params.NAME##_provided)); \
    if (ret) return ret;
#include "shmem_env_defs.h"
#undef SHMEM_INTERNAL_ENV_DEF
    return 0;
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
