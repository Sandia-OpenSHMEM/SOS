/*
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2023 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution. */

#include <sys/time.h>

/* This is the same implementation as shmemx_wtime() in SOS, but is provided
 * for the convenience of implementations that do not define shmemx_wtime() */
#ifndef HAVE_SHMEMX_WTIME
static inline double tests_sos_wtime(void)
{
    double wtime = 0.0;

#ifdef HAVE_CLOCK_GETTIME
    struct timespec tv;
    clock_gettime(CLOCK_MONOTONIC, &tv);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_nsec / 1.0e9;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1.0e6;
#endif
    return wtime;
}
#else
static inline double tests_sos_wtime(void)
{
    return shmemx_wtime();
}
#endif /* HAVE_SHMEMX_WTIME */
