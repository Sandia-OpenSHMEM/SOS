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

check_PROGRAMS = $(TESTS)

TESTS = \
	test_shmem_barrier_01 \
	test_shmem_barrier_02 \
	test_shmem_barrier_all_01 \
	test_shmem_barrier_all_02

# test_shmem_barrier_all_03 is invalid; it requires a timeout

PROCS ?= 2
RUNCMD ?= yod -np $(PROCS)
TESTS_ENVIRONMENT ?= ${top_srcdir}/test/openshmem/feature_tests/Fortran/test_wrapper.sh $(RUNCMD)

AM_FCFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

test_shmem_barrier_01_SOURCES = test_shmem_barrier_01.f90
test_shmem_barrier_01_LDADD = ${top_builddir}/src/libsma.la

test_shmem_barrier_02_SOURCES = test_shmem_barrier_02.f90
test_shmem_barrier_02_LDADD = ${top_builddir}/src/libsma.la

test_shmem_barrier_all_01_SOURCES = test_shmem_barrier_all_01.f90
test_shmem_barrier_all_01_LDADD = ${top_builddir}/src/libsma.la

test_shmem_barrier_all_02_SOURCES = test_shmem_barrier_all_02.f90
test_shmem_barrier_all_02_LDADD = ${top_builddir}/src/libsma.la

#test_shmem_barrier_all_03_SOURCES = test_shmem_barrier_all_03.f90
#test_shmem_barrier_all_03_LDADD = ${top_builddir}/src/libsma.la
