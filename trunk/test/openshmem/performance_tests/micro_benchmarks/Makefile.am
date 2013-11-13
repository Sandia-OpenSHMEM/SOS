# -*- Makefile -*-
#
# Copyright 2011 Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
# retains certain rights in this software.
# 
# This file is part of the Portals SHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.
#

all: $(check_PROGRAMS)

TESTS =
check_PROGRAMS = $(TESTS)

if ENABLE_LENGTHY_TESTS
TESTS += \
	put_performance \
	get_performance \
	barrier_performance \
	broadcast32_performance \
	collect32_performance \
	fcollect32_performance
else
check_PROGRAMS += \
	put_performance \
	get_performance \
	barrier_performance \
	broadcast32_performance \
	collect32_performance \
	fcollect32_performance
endif

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_CPPFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

put_performance_SOURCES = put_performance.c
put_performance_LDADD = $(top_builddir)/src/libsma.la

get_performance_SOURCES = get_performance.c
get_performance_LDADD = $(top_builddir)/src/libsma.la

barrier_performance_SOURCES = barrier_performance.c
barrier_performance_LDADD = $(top_builddir)/src/libsma.la

broadcast32_performance_SOURCES = broadcast32_performance.c
broadcast32_performance_LDADD = $(top_builddir)/src/libsma.la

collect32_performance_SOURCES = collect32_performance.c
collect32_performance_LDADD = $(top_builddir)/src/libsma.la

fcollect32_performance_SOURCES = fcollect32_performance.c
fcollect32_performance_LDADD = $(top_builddir)/src/libsma.la
