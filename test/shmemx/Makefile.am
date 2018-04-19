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

if HAVE_PTHREADS
if SHMEMX_TESTS
check_PROGRAMS += \
	gettid_register
endif
endif

TESTS = $(check_PROGRAMS)

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

if EXTERNAL_TESTS
bin_PROGRAMS = $(check_PROGRAMS)
AM_CPPFLAGS =
AM_FCFLAGS =
LDADD =
else
AM_CPPFLAGS = -I$(top_builddir)/mpp
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
endif
