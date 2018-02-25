# -*- Makefile -*-
#
# Copyright 2011 Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
# retains certain rights in this software.
#
# Copyright (c) 2016 Los Alamos National Security, LLC. All rights reserved.
#
# Copyright (c) 2017 Intel Corporation. All rights reserved.
# This software is available to you under the BSD license.
#
# This file is part of the Sandia OpenSHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

check_PROGRAMS = \
	binary-search \
	gups

SHORT_TESTS = \
	binary-search \
	gups

if HAVE_PTHREADS
check_PROGRAMS += \
	mandelbrot

SHORT_TESTS += \
	mandelbrot
endif

if ENABLE_LENGTHY_TESTS
TESTS = $(check_PROGRAMS)
else
TESTS = $(SHORT_TESTS)
endif

if EXTERNAL_TESTS
bin_PROGRAMS = $(check_PROGRAMS)
AM_CPPFLAGS =
LDADD =
else
AM_CPPFLAGS = -I$(top_builddir)/mpp
LDADD = $(top_builddir)/src/libsma.la
endif

if USE_PMI_SIMPLE
LDADD += $(top_builddir)/pmi-simple/libpmi_simple.la
endif

mandelbrot_LDFLAGS = $(PTHREAD_LIBS)
mandelbrot_CFLAGS = -I$(top_srcdir)/test/unit $(PTHREAD_CFLAGS)
mandelbrot_LDADD = $(LDADD) $(PTHREAD_CFLAGS)
