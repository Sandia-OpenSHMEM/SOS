# -*- Makefile -*-
#
# Copyright (c) 2018 Intel Corporation. All rights reserved.
# This software is available to you under the BSD license.
#
# This file is part of the Sandia OpenSHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.

check_PROGRAMS = \
	shmem_latency_put_perf \
	shmem_latency_put_perf_nb \
	shmem_latency_get_perf \
	shmem_latency_get_perf_nb \
	shmem_bw_put_perf \
	shmem_bw_put_perf_nb \
	shmem_bibw_put_perf \
	shmem_bibw_put_perf_nb \
	shmem_bw_get_perf \
	shmem_bw_get_perf_nb \
	shmem_bibw_get_perf \
	shmem_bibw_get_perf_nb \
	shmem_bw_atomics_perf \
	shmem_bibw_atomics_perf

if HAVE_OPENMP
check_PROGRAMS += \
	shmem_bw_put_ctx_perf \
	shmem_bw_put_ctx_perf_nb \
	shmem_bibw_put_ctx_perf \
	shmem_bibw_put_ctx_perf_nb \
	shmem_latency_put_ctx_perf \
	shmem_latency_put_ctx_perf_nb \
	shmem_latency_get_ctx_perf \
	shmem_latency_get_ctx_perf_nb 
endif

if SHMEMX_TESTS
check_PROGRAMS += \
        shmem_bw_nb_atomics_perf \
        shmem_bibw_nb_atomics_perf
endif

noinst_HEADERS = \
	common.h \
	latency_common.h \
	round_t_latency.h \
	int_element_latency.h \
	bw_common.h \
	uni_dir.h \
	uni_dir_ctx.h \
	bi_dir.h \
	bi_dir_ctx.h \
	target_put.h \
	latency_ctx.h

if ENABLE_LENGTHY_TESTS
TESTS = $(check_PROGRAMS)
endif

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

if EXTERNAL_TESTS
bin_PROGRAMS = $(check_PROGRAMS)
AM_CPPFLAGS =
LDADD =
else
AM_CPPFLAGS = -I$(top_builddir)/mpp -I$(top_srcdir)/mpp
LDADD = $(top_builddir)/src/libsma.la
endif

if USE_PMI_SIMPLE
LDADD += $(top_builddir)/pmi-simple/libpmi_simple.la
endif

shmem_latency_put_perf_nb_SOURCES = shmem_latency_put_perf.c
shmem_latency_put_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_latency_get_perf_nb_SOURCES = shmem_latency_get_perf.c
shmem_latency_get_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bw_put_perf_nb_SOURCES = shmem_bw_put_perf.c
shmem_bw_put_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bw_get_perf_nb_SOURCES = shmem_bw_get_perf.c
shmem_bw_get_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bibw_put_perf_nb_SOURCES = shmem_bibw_put_perf.c
shmem_bibw_put_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bibw_get_perf_nb_SOURCES = shmem_bibw_get_perf.c
shmem_bibw_get_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bw_put_ctx_perf_CFLAGS = $(AM_OPENMP_CFLAGS)

shmem_bw_put_ctx_perf_nb_SOURCES = shmem_bw_put_ctx_perf.c
shmem_bw_put_ctx_perf_nb_CFLAGS = $(AM_OPENMP_CFLAGS) -DUSE_NONBLOCKING_API

shmem_bibw_put_ctx_perf_CFLAGS = $(AM_OPENMP_CFLAGS)

shmem_bibw_put_ctx_perf_nb_SOURCES = shmem_bibw_put_ctx_perf.c
shmem_bibw_put_ctx_perf_nb_CFLAGS = $(AM_OPENMP_CFLAGS) -DUSE_NONBLOCKING_API

shmem_latency_put_ctx_perf_CFLAGS = $(AM_OPENMP_CFLAGS)

shmem_latency_put_ctx_perf_nb_SOURCES = shmem_latency_put_ctx_perf.c
shmem_latency_put_ctx_perf_nb_CFLAGS = $(AM_OPENMP_CFLAGS) -DUSE_NONBLOCKING_API

shmem_latency_get_ctx_perf_CFLAGS = $(AM_OPENMP_CFLAGS)

shmem_latency_get_ctx_perf_nb_SOURCES = shmem_latency_get_ctx_perf.c
shmem_latency_get_ctx_perf_nb_CFLAGS = $(AM_OPENMP_CFLAGS) -DUSE_NONBLOCKING_API

AM_CPPFLAGS += -DENABLE_THREADS
