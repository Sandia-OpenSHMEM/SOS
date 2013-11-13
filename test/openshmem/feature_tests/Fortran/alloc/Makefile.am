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
	test_shmem_shpalloc_01_char \
	test_shmem_shpalloc_01_double \
	test_shmem_shpalloc_01_int4 \
	test_shmem_shpalloc_01_int8 \
	test_shmem_shpalloc_01_logical \
	test_shmem_shpalloc_01_real4 \
	test_shmem_shpalloc_01_real8 \
	test_shmem_shpalloc_02_char \
	test_shmem_shpalloc_02_double \
	test_shmem_shpalloc_02_int4 \
	test_shmem_shpalloc_02_int8 \
	test_shmem_shpalloc_02_logical \
	test_shmem_shpalloc_02_real4 \
	test_shmem_shpalloc_02_real8 \
	test_shmem_shpalloc_03_char \
	test_shmem_shpalloc_03_double \
	test_shmem_shpalloc_03_int4 \
	test_shmem_shpalloc_03_int8 \
	test_shmem_shpalloc_03_logical \
	test_shmem_shpalloc_03_real4 \
	test_shmem_shpalloc_03_real8

NPROCS ?= 2
LOG_COMPILER = ${top_srcdir}/test/openshmem/feature_tests/Fortran/test_wrapper.sh $(TEST_RUNNER)

AM_FCFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

test_shmem_shpalloc_01_char_SOURCES = test_shmem_shpalloc_01_char.f90
test_shmem_shpalloc_01_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_01_double_SOURCES = test_shmem_shpalloc_01_double.f90
test_shmem_shpalloc_01_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_01_int4_SOURCES = test_shmem_shpalloc_01_int4.f90
test_shmem_shpalloc_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_01_int8_SOURCES = test_shmem_shpalloc_01_int8.f90
test_shmem_shpalloc_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_01_logical_SOURCES = test_shmem_shpalloc_01_logical.f90
test_shmem_shpalloc_01_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_01_real4_SOURCES = test_shmem_shpalloc_01_real4.f90
test_shmem_shpalloc_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_01_real8_SOURCES = test_shmem_shpalloc_01_real8.f90
test_shmem_shpalloc_01_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_02_char_SOURCES = test_shmem_shpalloc_02_char.f90
test_shmem_shpalloc_02_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_02_double_SOURCES = test_shmem_shpalloc_02_double.f90
test_shmem_shpalloc_02_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_02_int4_SOURCES = test_shmem_shpalloc_02_int4.f90
test_shmem_shpalloc_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_02_int8_SOURCES = test_shmem_shpalloc_02_int8.f90
test_shmem_shpalloc_02_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_02_logical_SOURCES = test_shmem_shpalloc_02_logical.f90
test_shmem_shpalloc_02_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_02_real4_SOURCES = test_shmem_shpalloc_02_real4.f90
test_shmem_shpalloc_02_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_02_real8_SOURCES = test_shmem_shpalloc_02_real8.f90
test_shmem_shpalloc_02_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_03_char_SOURCES = test_shmem_shpalloc_03_char.f90
test_shmem_shpalloc_03_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_03_double_SOURCES = test_shmem_shpalloc_03_double.f90
test_shmem_shpalloc_03_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_03_int4_SOURCES = test_shmem_shpalloc_03_int4.f90
test_shmem_shpalloc_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_03_int8_SOURCES = test_shmem_shpalloc_03_int8.f90
test_shmem_shpalloc_03_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_03_logical_SOURCES = test_shmem_shpalloc_03_logical.f90
test_shmem_shpalloc_03_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_03_real4_SOURCES = test_shmem_shpalloc_03_real4.f90
test_shmem_shpalloc_03_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_shpalloc_03_real8_SOURCES = test_shmem_shpalloc_03_real8.f90
test_shmem_shpalloc_03_real8_LDADD = ${top_builddir}/src/libsma.la
