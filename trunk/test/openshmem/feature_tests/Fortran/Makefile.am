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

DIST_SUBDIRS = accessibility alloc atomic barrier broadcast collect reductions transfer
SUBDIRS = .

all: $(check_PROGRAMS)

check_PROGRAMS = $(TESTS)

if HAVE_FORTRAN

SUBDIRS += accessibility alloc atomic barrier broadcast collect reductions transfer

TESTS = \
	test_shmem_lock \
	test_shmem_synchronization
endif

NPROCS ?= 2
LOG_COMPILER = ${top_srcdir}/test/openshmem/feature_tests/Fortran/test_wrapper.sh $(TEST_RUNNER)

AM_CPPFLAGS = -I$(top_srcdir)/mpp
AM_FCFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

test_shmem_lock_SOURCES = test_shmem_lock.f90
test_shmem_lock_LDADD = $(top_builddir)/src/libsma.la

test_shmem_synchronization_SOURCES = test_shmem_synchronization.f90
test_shmem_synchronization_LDADD = $(top_builddir)/src/libsma.la

