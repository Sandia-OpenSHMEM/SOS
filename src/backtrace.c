/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2018 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 * "Copyright (c) 2000-2018 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS."
 */

#ifndef BACKTRACE_H
#define BACKTRACE_H

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "shmem_env.h"
#include "shmem_internal.h"

/* Backtrace through execinfo */
#if defined(USE_BT_EXECINFO)
#include <execinfo.h>
/* Maximum total number of backtraces */
#define MAX_BT_SIZE 1024

static
void backtrace_execinfo(void) {
    static void *btaddr[MAX_BT_SIZE];
    size_t size, i;
    char **fnnames = NULL;

    size = backtrace(btaddr, MAX_BT_SIZE);
    fnnames = backtrace_symbols(btaddr, size);

    if (fnnames) {
        for (i = 0; i < size; i++)
            fprintf(stderr, "%s\n", fnnames[i]);

        free(fnnames);
    }
}

#endif /* USE_BT_EXECINFO */

/* Backtrace through gdb */
#if defined(USE_BT_GDB)
/* Maximum path size for gdb command file */
#define MAX_BT_PATHSIZE 1024

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

static const char *bt_tmpdir = "/tmp";

static
int bt_mkstemp(char *filename, int limit) {
    const char template[] = "/sos_XXXXXX"; /* Last six chars must be "X" for a unique name */
    char *p;
    int len;

    len = strlen(bt_tmpdir);
    len = MIN(len, limit - 1);
    memcpy(filename, bt_tmpdir, len);
    p = filename + len;

    len = MIN(limit - len - 1, sizeof(template));
    memcpy(p, template, len);

    p[len] = '\0';

    if (strlen(filename) < limit) {
        return mkstemp(filename);
    } else {
        return -1;
    }
}

static
int create_command_file(char *filename) {
#ifdef ENABLE_THREADS
    const char commands[] = "\ninfo threads\nthread apply all backtrace 50\ndetach\nquit\n";
#else
    const char commands[] = "\nbacktrace 50\ndetach\nquit\n";
#endif
    const char shell_rm[]  = "shell /bin/rm -f ";

    int tmpfd, len;

    tmpfd = bt_mkstemp(filename, MAX_BT_PATHSIZE);
    if (tmpfd < 0) {
        return -1;
    }

    len = sizeof(shell_rm) - 1;
    if (len != write(tmpfd, shell_rm, len)) {
        return -2;
    }

    len = strlen(filename);
    if (len != write(tmpfd, filename, len)) {
        return -3;
    }

    len = sizeof(commands) - 1;
    if (len != write(tmpfd, commands, len)) {
        return -4;
    }

    if (0 != close(tmpfd)) {
        return -5;
    }

    return 0;
}

static
int system_execute(const char *cmd) {
  int rc;
  rc = system(cmd); // will return -1 on failure to spawn child process
  return rc;
}

static
void backtrace_gdb(void) {
    const char fmt[] = "%s -nx -batch -x %s -p %d";
    static char cmd[sizeof(fmt) + 3 * MAX_BT_PATHSIZE];
    char filename[MAX_BT_PATHSIZE];
    const char *gdb = (access(GDB_PATH, X_OK) ? "gdb" : GDB_PATH);
    int rc;

    rc = create_command_file(filename);
    if (rc != 0) {
        RETURN_ERROR_MSG("Error creating gdb input file %s with error code %d\n",
                         filename, rc);
        (void)unlink(filename);
        return;
    }

    rc = snprintf(cmd, sizeof(cmd), fmt, gdb, filename, (int)getpid());
    if ((rc < 0) || (rc >= sizeof(cmd))) {
        RETURN_ERROR_MSG("Error writing gdb commands to file %s\n", filename);
        (void)unlink(filename);
        return;
    }

    rc = system_execute(cmd);
    if (rc < 0) {
        RETURN_ERROR_MSG("Error executing gdb command file %s\n", filename);
    }
    (void)unlink(filename);
}

#endif /* USE_BT_GDB */

void shmem_util_backtrace(void) {
    char *method = shmem_internal_params.BACKTRACE;

    if (0 == strcmp(method, "execinfo")) {
#if defined(USE_BT_EXECINFO)
        backtrace_execinfo();
#else
        RAISE_WARN_STR("Backtrace support through execinfo is not available.");
#endif
    } else if (0 == strcmp(method, "gdb")) {
#if defined(USE_BT_GDB)
        backtrace_gdb();
#else
        RAISE_WARN_STR("Backtrace support through gdb is not available.");
#endif
    } else if (0 == strcmp(method, "auto")) {
#if defined(USE_BT_EXECINFO)
        backtrace_execinfo();
#elif defined(USE_BT_GDB)
        backtrace_gdb();
#else
        RAISE_WARN_STR("Backtrace support is not available.");
#endif
    } else if (0 == strcmp(method, "")) {
        /* disabled */
    } else {
        RAISE_WARN_MSG("Ignoring invalid backtrace method '%s'.\n",
                       method);
#if defined(USE_BT_EXECINFO)
        backtrace_execinfo();
#elif defined(USE_BT_GDB)
        backtrace_gdb();
#else
        RAISE_WARN_STR("Backtrace support is not available.");
#endif
    }
}

#endif /* BACKTRACE_H */

