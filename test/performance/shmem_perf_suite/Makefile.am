# -*- Makefile -*-
#
# Copyright (c) 2015 Intel Corporation. All rights reserved.
# This software is available to you under the BSD license. For
# license information, see the LICENSE file in the top level directory.
#

check_PROGRAMS = $(TESTS)

TESTS =

if ENABLE_LENGTHY_TESTS
TESTS += \
	shmem_latency_put_perf \
	shmem_latency_get_perf \
	shmem_bw_put_perf \
	shmem_bibw_put_perf \
	shmem_bw_get_perf \
	shmem_bibw_get_perf
else
check_PROGRAMS += \
	shmem_latency_put_perf \
	shmem_latency_get_perf \
	shmem_bw_put_perf \
	shmem_bibw_put_perf \
	shmem_bw_get_perf \
	shmem_bibw_get_perf
 
endif

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_CPPFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)
LDADD = $(top_builddir)/src/libsma.la

shmem_latency_put_perf_SOURCES = shmem_latency_put_perf.c

shmem_latency_get_perf_SOURCES = shmem_latency_get_perf.c

shmem_bw_put_perf_SOURCES = shmem_bw_put_perf.c

shmem_bibw_put_perf_SOURCES = shmem_bibw_put_perf.c

shmem_bw_get_perf_SOURCES = shmem_bw_get_perf.c

shmem_bibw_get_perf_SOURCES = shmem_bibw_get_perf.c

