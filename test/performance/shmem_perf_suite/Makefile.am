# -*- Makefile -*-
#
# Copyright (c) 2017 Intel Corporation. All rights reserved.
# This software is available to you under the BSD license.
#
# This file is part of the Sandia OpenSHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.

check_PROGRAMS = \
	shmem_latency_put_perf \
	shmem_latency_get_perf \
	shmem_bw_put_perf \
	shmem_bw_put_perf_nb \
	shmem_bibw_put_perf \
	shmem_bibw_put_perf_nb \
	shmem_bw_get_perf \
	shmem_bw_get_perf_nb \
	shmem_bibw_get_perf \
	shmem_bibw_get_perf_nb \
	shmem_latency_nb_put_perf \
	shmem_latency_nb_get_perf \
	shmem_bw_atomics_perf \
	shmem_bibw_atomics_perf

if HAVE_OPENMP
check_PROGRAMS += \
	shmem_bw_put_ctx_perf \
	shmem_bw_put_ctx_perf_nb \
	shmem_bibw_put_ctx_perf \
	shmem_bibw_put_ctx_perf_nb
endif

noinst_HEADERS = \
	common.h \
	latency_common.h \
	round_t_latency.h \
	int_element_latency.h \
	bw_common.h \
	uni_dir.h \
	bi_dir.h \
	target_put.h

if ENABLE_LENGTHY_TESTS
TESTS = $(check_PROGRAMS)
endif

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_CPPFLAGS = -I$(top_builddir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)
if HAVE_OPENMP
AM_CPPFLAGS += -fopenmp
AM_LDFLAGS += -fopenmp
endif
LDADD = $(top_builddir)/src/libsma.la

if USE_PMI_SIMPLE
LDADD += $(top_builddir)/pmi-simple/libpmi_simple.la
endif

shmem_bw_put_perf_nb_SOURCES = shmem_bw_put_perf.c
shmem_bw_put_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bw_get_perf_nb_SOURCES = shmem_bw_get_perf.c
shmem_bw_get_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bibw_put_perf_nb_SOURCES = shmem_bibw_put_perf.c
shmem_bibw_put_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bibw_get_perf_nb_SOURCES = shmem_bibw_get_perf.c
shmem_bibw_get_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bw_put_ctx_perf_nb_SOURCES = shmem_bw_put_ctx_perf.c
shmem_bw_put_ctx_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API

shmem_bibw_put_ctx_perf_nb_SOURCES = shmem_bibw_put_ctx_perf.c
shmem_bibw_put_ctx_perf_nb_CFLAGS = -DUSE_NONBLOCKING_API
