# -*- Makefile -*-
#
# Copyright 2011 Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
# retains certain rights in this software.
#
# Copyright (c) 2017 Intel Corporation. All rights reserved.
# This software is available to you under the BSD license.
#
# This file is part of the Sandia OpenSHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.

check_PROGRAMS =

if ENABLE_PROFILING
check_PROGRAMS += \
	pcontrol
endif

if USE_PORTALS4
check_PROGRAMS += \
	shmem_ct
endif

if SHMEMX_TESTS

check_PROGRAMS += \
	perf_counter \
	fadd_nbi \
	atomic_nbi \
	put_signal \
	put_signal_nbi \
	shmemx_wait_until_all \
	shmemx_wait_until_any \
	shmemx_wait_until_some \
	shmemx_test_all \
	shmemx_test_any \
	shmemx_test_some \
	c11_test_shmemx_wait_until \
	c11_test_shmemx_test \
	shmemx_team_split_strided \
	shmemx_team_translate \
	shmemx_team_translate_2 \
	shmemx_team_reuse_teams \
	shmemx_team_sync \
	shmemx_team_broadcast \
	shmemx_team_collect \
	shmemx_team_collect_active_set \
	shmemx_team_alltoall \
	shmemx_team_alltoalls \
	shmemx_team_shared \
	shmemx_team_b2b_collectives \
	c11_shmemx_team_collective_types \
	c11_shmemx_team_reduce \
	shmemx_team_max

if HAVE_PTHREADS
check_PROGRAMS += \
	gettid_register
endif
endif SHMEMX_TESTS

if HAVE_CXX
if SHMEMX_TESTS
check_PROGRAMS += \
	cxx_test_shmem_g \
	cxx_test_shmem_get \
	cxx_test_shmem_p \
	cxx_test_shmem_put \
	cxx_test_shmem_atomic_fetch \
	cxx_test_shmem_atomic_set \
	cxx_test_shmem_atomic_add \
	cxx_test_shmem_atomic_inc \
	cxx_test_shmem_atomic_and \
	cxx_test_shmem_atomic_or \
	cxx_test_shmem_atomic_xor \
	cxx_test_shmem_atomic_swap \
	cxx_test_shmem_atomic_cswap \
	cxx_test_shmem_wait_until \
	cxx_test_shmem_test
endif
endif

TESTS = $(check_PROGRAMS)

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

if EXTERNAL_TESTS
bin_PROGRAMS = $(check_PROGRAMS)
AM_CPPFLAGS = -I$(top_srcdir)/test/include
AM_FCFLAGS =
LDADD =
else
AM_CPPFLAGS = -I$(top_builddir)/mpp -I$(top_srcdir)/mpp -I$(top_srcdir)/test/include
AM_FCFLAGS = -I$(top_builddir)/mpp
LDADD = $(top_builddir)/src/libsma.la
endif

if USE_PMI_SIMPLE
LDADD += $(top_builddir)/pmi-simple/libpmi_simple.la
endif

if SHMEMX_TESTS
gettid_register_LDFLAGS = $(AM_LDFLAGS) $(PTHREAD_LIBS)
gettid_register_CFLAGS = $(PTHREAD_CFLAGS)
gettid_register_LDADD = $(LDADD) $(PTHREAD_CFLAGS)

AM_CPPFLAGS += -DENABLE_SHMEMX_TESTS

# C++ Tests
cxx_test_shmem_g_SOURCES = cxx_test_shmem_g.cpp
cxx_test_shmem_get_SOURCES = cxx_test_shmem_get.cpp
cxx_test_shmem_p_SOURCES = cxx_test_shmem_p.cpp
cxx_test_shmem_put_SOURCES = cxx_test_shmem_put.cpp
cxx_test_shmem_atomic_fetch_SOURCES = cxx_test_shmem_atomic_fetch.cpp
cxx_test_shmem_atomic_set_SOURCES = cxx_test_shmem_atomic_set.cpp
cxx_test_shmem_atomic_add_SOURCES = cxx_test_shmem_atomic_add.cpp
cxx_test_shmem_atomic_inc_SOURCES = cxx_test_shmem_atomic_inc.cpp
cxx_test_shmem_atomic_and_SOURCES = cxx_test_shmem_atomic_and.cpp
cxx_test_shmem_atomic_or_SOURCES = cxx_test_shmem_atomic_or.cpp
cxx_test_shmem_atomic_xor_SOURCES = cxx_test_shmem_atomic_xor.cpp
cxx_test_shmem_atomic_swap_SOURCES = cxx_test_shmem_atomic_swap.cpp
cxx_test_shmem_atomic_cswap_SOURCES = cxx_test_shmem_atomic_cswap.cpp
cxx_test_shmem_wait_until_SOURCES = cxx_test_shmem_wait_until.cpp
cxx_test_shmem_test_SOURCES = cxx_test_shmem_test.cpp

endif
