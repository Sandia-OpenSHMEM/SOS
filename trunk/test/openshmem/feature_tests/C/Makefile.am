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

TESTS = \
	test_shmem_accessible \
	test_shmem_atomics \
	test_shmem_barrier \
	test_shmem_broadcast \
	test_shmem_collects \
	test_shmem_get_globals \
	test_shmem_get_shmalloc \
	test_shmem_lock \
	test_shmem_put_globals \
	test_shmem_put_shmalloc \
	test_shmem_reduction \
	test_shmem_synchronization

check_PROGRAMS = \
	$(TESTS)

PROCS ?= 2

TESTS_ENVIRONMENT = yod -np $(PROCS)

AM_CPPFLAGS = -I$(top_srcdir)/mpp

test_shmem_accessible_SOURCES = test_shmem_accessible.c
test_shmem_accessible_LDADD = $(top_builddir)/src/libsma.la

test_shmem_atomics_SOURCES = test_shmem_atomics.c
test_shmem_atomics_LDADD = $(top_builddir)/src/libsma.la

test_shmem_barrier_SOURCES = test_shmem_barrier.c
test_shmem_barrier_LDADD = $(top_builddir)/src/libsma.la

test_shmem_broadcast_SOURCES = test_shmem_broadcast.c
test_shmem_broadcast_LDADD = $(top_builddir)/src/libsma.la

test_shmem_get_globals_SOURCES = test_shmem_get_globals.c
test_shmem_get_globals_LDADD = $(top_builddir)/src/libsma.la

test_shmem_get_shmalloc_SOURCES = test_shmem_get_shmalloc.c
test_shmem_get_shmalloc_LDADD = $(top_builddir)/src/libsma.la

test_shmem_lock_SOURCES = test_shmem_lock.c
test_shmem_lock_LDADD = $(top_builddir)/src/libsma.la

test_shmem_put_globals_SOURCES = test_shmem_put_globals.c
test_shmem_put_globals_LDADD = $(top_builddir)/src/libsma.la

test_shmem_put_shmalloc_SOURCES = test_shmem_put_shmalloc.c
test_shmem_put_shmalloc_LDADD = $(top_builddir)/src/libsma.la

test_shmem_reduction_SOURCES = test_shmem_reduction.c
test_shmem_reduction_LDADD = $(top_builddir)/src/libsma.la

test_shmem_synchronization_SOURCES = test_shmem_synchronization.c
test_shmem_synchronization_LDADD = $(top_builddir)/src/libsma.la
