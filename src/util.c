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
#include <string.h>

#include "shmem_internal.h"


/* Wrap 'str' to fit within 'wraplen' columns.  After each line break, insert
 * 'indent' string (if provided).  Caller must free the returned buffer.
 */
char *
shmem_util_wrap(const char *str, const size_t wraplen, const char *indent)
{
    const size_t indent_len = indent != NULL ? strlen(indent) : 0;
    const size_t str_len = strlen(str);
    size_t linelen = 0;
    char *str_s = NULL, *out_s = NULL;

    /* Worst case is wrapping at 1/2 wraplen */
    char *out = malloc(str_len + 2*(str_len/wraplen + 1) * indent_len);
    char *out_p = out;
    char *str_p = (char*) str;

    if (out == NULL)
        RAISE_ERROR_STR("unable to allocate output buffer");

    while (*str_p != '\0') {
        /* Remember location of last space */
        if (*str_p == ' ') {
            str_s = str_p;
            out_s = out_p;
        }
        /* Reached end of line, try to wrap */
        if (linelen >= wraplen) {
            if (str_s != NULL) {
                out_p = out_s; /* Jump back to last space */
                str_p = str_s;
                *out_p = '\n'; /* Append newline and indent */
                out_p++;
                if (indent) {
                    strcpy(out_p, indent);
                    out_p += indent_len;
                }
                str_p++;
                out_s = str_s = NULL;
                linelen = 0;
                continue;
            }
        }
        *out_p = *str_p;
        out_p++;
        str_p++;
        linelen++;
    }
    *out_p = '\0';
    return out;
}
