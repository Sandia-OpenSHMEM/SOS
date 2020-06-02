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

check_PROGRAMS = \
        shmem_ctx_pipelined_reduce

if HAVE_OPENMP
check_PROGRAMS += \
    shmem_ctx
endif

if SHMEMX_TESTS
check_PROGRAMS += \
	shmemx_wait_until_all \
	shmemx_wait_until_any \
	shmemx_wait_until_some \
	shmemx_wait_until_any_vector \
	shmemx_test_any \
	shmemx_test_some \
	shmemx_team_split_strided \
	shmem_team_split_2D \
	shmemx_team_translate \
	shmem_team_context \
	shmemx_team_sync \
	shmemx_team_broadcast \
	shmemx_team_collect \
	shmem_reduce_example \
	shmemx_team_alltoall \
	shmemx_team_alltoalls

endif SHMEMX_TESTS

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
AM_CPPFLAGS += -DENABLE_SHMEMX_TESTS
endif

shmem_ctx_CFLAGS = $(AM_OPENMP_CFLAGS)
