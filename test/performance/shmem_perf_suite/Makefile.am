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
	shmem_bibw_put_perf \
	shmem_bw_get_perf \
	shmem_bibw_get_perf \
	shmem_latency_nb_put_perf \
	shmem_latency_nb_get_perf \
	shmem_bw_nb_put_perf \
	shmem_bibw_nb_put_perf \
	shmem_bw_nb_get_perf \
	shmem_bibw_nb_get_perf \
	shmem_bw_atomics_perf \
	shmem_bibw_atomics_perf

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
AM_LDFLAGS = $(aslr_LDFLAGS) $(LIBTOOL_WRAPPER_LDFLAGS)
LDADD = $(top_builddir)/src/libsma.la

if USE_PMI_SIMPLE
LDADD += $(top_builddir)/pmi-simple/libpmi_simple.la
endif
