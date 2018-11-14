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
 */

#ifndef BACKTRACE_H
#define BACKTRACE_H

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include "shmem_env.h"

/* Backtrace through execinfo */
#if defined(USE_BT_EXECINFO)
#include <execinfo.h>
/* Maximum total number of backtraces */
#define MAX_BT_SIZE 1024

static inline 
void backtrace_execinfo(void) {
    static void *btaddr[MAX_BT_SIZE];
    size_t size, i;
    char **fnnames = NULL;

    size = backtrace(btaddr, MAX_BT_SIZE); 
    fnnames = backtrace_symbols(btaddr, size);

    for (i = 0; i < size; i++) 
        fprintf(stderr, "%s\n", fnnames[i]); 

    free(fnnames);  
}

#endif

/* Backtrace through gdb */
#if defined(USE_BT_GDB)
/* Maximum path size for gdb command file */
#define MAX_BT_PATHSIZE 1024

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif

static char bt_exename[MAX_BT_PATHSIZE];
static const char *bt_tmpdir = "/tmp";

static inline
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

static inline
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

static inline 
int system_execute(const char *cmd) {
  int rc;
  rc = system(cmd); // will return -1 on failure to spawn child process
  return rc;
}

static inline 
void backtrace_gdb(void) {
    const char fmt[] = "%s -nx -batch -x %s '%s' %d";
    static char cmd[sizeof(fmt) + 3 * MAX_BT_PATHSIZE];
    char filename[MAX_BT_PATHSIZE];
    const char *gdb = (access(GDB_PATH, X_OK) ? "gdb" : GDB_PATH);
    int rc;
    rc = create_command_file(filename);
    if (rc != 0) {
        fprintf(stderr, "Error in creating gdb input file %s with error code %d\n", 
                         filename, rc);
        (void)unlink(filename); 
        return;
    }

    rc = snprintf(cmd, sizeof(cmd), fmt, gdb, filename, bt_exename, (int)getpid());
    if ((rc < 0) || (rc >= sizeof(cmd))) {
        fprintf(stderr, "Error in writing gdb commands to file %s\n", filename);
        (void)unlink(filename); 
        return;
    }

    rc = system_execute(cmd);
    if (rc < 0) {
        fprintf(stderr, "Error in executing gdb command file %s\n", filename);
    }
    (void)unlink(filename); 
}
#endif

static inline
void collect_backtrace(void) {
    char *method = shmem_internal_params.BACKTRACE_METHOD;

    if (0 == strcmp(method, "execinfo")) {
#if defined(USE_BT_EXECINFO)
        backtrace_execinfo();
#else
        fprintf(stderr, "Backtrace support through execinfo is not available.\n");
#endif
    } else if (0 == strcmp(method, "gdb")) {
#if defined(USE_BT_GDB)
        backtrace_gdb();
#else
        fprintf(stderr, "Backtrace support through gdb is not available.\n");
#endif
    } else {
        fprintf(stderr, "Ignoring bad user input for backtrace method %s. Choosing execinfo.\n",
                       method);
#if defined(USE_BT_EXECINFO)
        backtrace_execinfo();
#else
        fprintf("Backtrace support through execinfo is not available.\n");
#endif
    }
}

#endif /* BACKTRACE_H */

