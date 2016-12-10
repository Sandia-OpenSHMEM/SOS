/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2016 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */


#include "config.h"

#define _GNU_SOURCE /* for asprintf */
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include <errno.h>

#include "shmem_internal.h"


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

    if (*e == 'K')
        val *= 1024L;
    else if (*e == 'M')
        val *= 1024L*1024L;
    else if (*e == 'G')
        val *= 1024L*1024L*1024L;
    else if (*e == 'T')
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


long
shmem_util_getenv_long(const char* name, int is_sized, long default_value)
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


char *
shmem_util_getenv_str(const char* name)
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
