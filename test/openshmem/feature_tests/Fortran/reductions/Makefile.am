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
	test_shmem_and_01_int4 \
	test_shmem_and_01_int8 \
	test_shmem_max_01_int4 \
	test_shmem_max_01_int8 \
	test_shmem_max_01_real4 \
	test_shmem_max_01_real8 \
	test_shmem_min_01_int4 \
	test_shmem_min_01_int8 \
	test_shmem_min_01_real4 \
	test_shmem_min_01_real8 \
	test_shmem_or_01_int4 \
	test_shmem_or_01_int8 \
	test_shmem_prod_01_int4 \
	test_shmem_prod_01_int8 \
	test_shmem_prod_01_real4 \
	test_shmem_prod_01_real8 \
	test_shmem_sum_01_int4 \
	test_shmem_sum_01_int8 \
	test_shmem_sum_01_real4 \
	test_shmem_sum_01_real8 \
	test_shmem_xor_01_int4 \
	test_shmem_xor_01_int8

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_FCFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

test_shmem_and_01_int4_SOURCES = test_shmem_and_01_int4.f90
test_shmem_and_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_and_01_int8_SOURCES = test_shmem_and_01_int8.f90
test_shmem_and_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_max_01_int4_SOURCES = test_shmem_max_01_int4.f90
test_shmem_max_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_max_01_int8_SOURCES = test_shmem_max_01_int8.f90
test_shmem_max_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_max_01_real4_SOURCES = test_shmem_max_01_real4.f90
test_shmem_max_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_max_01_real8_SOURCES = test_shmem_max_01_real8.f90
test_shmem_max_01_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_min_01_int4_SOURCES = test_shmem_min_01_int4.f90
test_shmem_min_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_min_01_int8_SOURCES = test_shmem_min_01_int8.f90
test_shmem_min_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_min_01_real4_SOURCES = test_shmem_min_01_real4.f90
test_shmem_min_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_min_01_real8_SOURCES = test_shmem_min_01_real8.f90
test_shmem_min_01_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_or_01_int4_SOURCES = test_shmem_or_01_int4.f90
test_shmem_or_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_or_01_int8_SOURCES = test_shmem_or_01_int8.f90
test_shmem_or_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_prod_01_int4_SOURCES = test_shmem_prod_01_int4.f90
test_shmem_prod_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_prod_01_int8_SOURCES = test_shmem_prod_01_int8.f90
test_shmem_prod_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_prod_01_real4_SOURCES = test_shmem_prod_01_real4.f90
test_shmem_prod_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_prod_01_real8_SOURCES = test_shmem_prod_01_real8.f90
test_shmem_prod_01_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_sum_01_int4_SOURCES = test_shmem_sum_01_int4.f90
test_shmem_sum_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_sum_01_int8_SOURCES = test_shmem_sum_01_int8.f90
test_shmem_sum_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_sum_01_real4_SOURCES = test_shmem_sum_01_real4.f90
test_shmem_sum_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_sum_01_real8_SOURCES = test_shmem_sum_01_real8.f90
test_shmem_sum_01_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_xor_01_int4_SOURCES = test_shmem_xor_01_int4.f90
test_shmem_xor_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_xor_01_int8_SOURCES = test_shmem_xor_01_int8.f90
test_shmem_xor_01_int8_LDADD = ${top_builddir}/src/libsma.la
