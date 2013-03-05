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

# test_shmem_broadcast_*_char are invalid tests; char is not necessarily 4 bytes

TESTS = \
	test_shmem_broadcast_01_double \
	test_shmem_broadcast_01_int4 \
	test_shmem_broadcast_01_int8 \
	test_shmem_broadcast_01_logical \
	test_shmem_broadcast_01_real4 \
	test_shmem_broadcast_01_real8 \
	test_shmem_broadcast_02_double \
	test_shmem_broadcast_02_int4 \
	test_shmem_broadcast_02_int8 \
	test_shmem_broadcast_02_logical \
	test_shmem_broadcast_02_real4 \
	test_shmem_broadcast_02_real8 \
	test_shmem_broadcast_03_double \
	test_shmem_broadcast_03_int4 \
	test_shmem_broadcast_03_int8 \
	test_shmem_broadcast_03_logical \
	test_shmem_broadcast_03_real4 \
	test_shmem_broadcast_03_real8 \
	test_shmem_broadcast_04_double \
	test_shmem_broadcast_04_int4 \
	test_shmem_broadcast_04_int8 \
	test_shmem_broadcast_04_logical \
	test_shmem_broadcast_04_real4 \
	test_shmem_broadcast_04_real8 \
	test_shmem_broadcast_05_double \
	test_shmem_broadcast_05_int4 \
	test_shmem_broadcast_05_int8 \
	test_shmem_broadcast_05_logical \
	test_shmem_broadcast_05_real4 \
	test_shmem_broadcast_05_real8

XFAIL_TESTS = \
	test_shmem_broadcast_04_double \
	test_shmem_broadcast_04_int4 \
	test_shmem_broadcast_04_int8 \
	test_shmem_broadcast_04_logical \
	test_shmem_broadcast_04_real4 \
	test_shmem_broadcast_04_real8

PROCS ?= 2
RUNCMD ?= yod -np $(PROCS)
TESTS_ENVIRONMENT ?= ${top_srcdir}/test/openshmem/feature_tests/Fortran/test_wrapper.sh $(RUNCMD)

AM_FCFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

#test_shmem_broadcast_01_char_SOURCES = test_shmem_broadcast_01_char.f90
#test_shmem_broadcast_01_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_01_double_SOURCES = test_shmem_broadcast_01_double.f90
test_shmem_broadcast_01_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_01_int4_SOURCES = test_shmem_broadcast_01_int4.f90
test_shmem_broadcast_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_01_int8_SOURCES = test_shmem_broadcast_01_int8.f90
test_shmem_broadcast_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_01_logical_SOURCES = test_shmem_broadcast_01_logical.f90
test_shmem_broadcast_01_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_01_real4_SOURCES = test_shmem_broadcast_01_real4.f90
test_shmem_broadcast_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_01_real8_SOURCES = test_shmem_broadcast_01_real8.f90
test_shmem_broadcast_01_real8_LDADD = ${top_builddir}/src/libsma.la

#test_shmem_broadcast_02_char_SOURCES = test_shmem_broadcast_02_char.f90
#test_shmem_broadcast_02_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_02_double_SOURCES = test_shmem_broadcast_02_double.f90
test_shmem_broadcast_02_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_02_int4_SOURCES = test_shmem_broadcast_02_int4.f90
test_shmem_broadcast_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_02_int8_SOURCES = test_shmem_broadcast_02_int8.f90
test_shmem_broadcast_02_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_02_logical_SOURCES = test_shmem_broadcast_02_logical.f90
test_shmem_broadcast_02_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_02_real4_SOURCES = test_shmem_broadcast_02_real4.f90
test_shmem_broadcast_02_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_02_real8_SOURCES = test_shmem_broadcast_02_real8.f90
test_shmem_broadcast_02_real8_LDADD = ${top_builddir}/src/libsma.la

#test_shmem_broadcast_03_char_SOURCES = test_shmem_broadcast_03_char.f90
#test_shmem_broadcast_03_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_03_double_SOURCES = test_shmem_broadcast_03_double.f90
test_shmem_broadcast_03_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_03_int4_SOURCES = test_shmem_broadcast_03_int4.f90
test_shmem_broadcast_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_03_int8_SOURCES = test_shmem_broadcast_03_int8.f90
test_shmem_broadcast_03_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_03_logical_SOURCES = test_shmem_broadcast_03_logical.f90
test_shmem_broadcast_03_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_03_real4_SOURCES = test_shmem_broadcast_03_real4.f90
test_shmem_broadcast_03_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_03_real8_SOURCES = test_shmem_broadcast_03_real8.f90
test_shmem_broadcast_03_real8_LDADD = ${top_builddir}/src/libsma.la

#test_shmem_broadcast_04_char_SOURCES = test_shmem_broadcast_04_char.f90
#test_shmem_broadcast_04_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_04_double_SOURCES = test_shmem_broadcast_04_double.f90
test_shmem_broadcast_04_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_04_int4_SOURCES = test_shmem_broadcast_04_int4.f90
test_shmem_broadcast_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_04_int8_SOURCES = test_shmem_broadcast_04_int8.f90
test_shmem_broadcast_04_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_04_logical_SOURCES = test_shmem_broadcast_04_logical.f90
test_shmem_broadcast_04_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_04_real4_SOURCES = test_shmem_broadcast_04_real4.f90
test_shmem_broadcast_04_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_04_real8_SOURCES = test_shmem_broadcast_04_real8.f90
test_shmem_broadcast_04_real8_LDADD = ${top_builddir}/src/libsma.la

#test_shmem_broadcast_05_char_SOURCES = test_shmem_broadcast_05_char.f90
#test_shmem_broadcast_05_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_05_double_SOURCES = test_shmem_broadcast_05_double.f90
test_shmem_broadcast_05_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_05_int4_SOURCES = test_shmem_broadcast_05_int4.f90
test_shmem_broadcast_05_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_05_int8_SOURCES = test_shmem_broadcast_05_int8.f90
test_shmem_broadcast_05_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_05_logical_SOURCES = test_shmem_broadcast_05_logical.f90
test_shmem_broadcast_05_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_05_real4_SOURCES = test_shmem_broadcast_05_real4.f90
test_shmem_broadcast_05_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_broadcast_05_real8_SOURCES = test_shmem_broadcast_05_real8.f90
test_shmem_broadcast_05_real8_LDADD = ${top_builddir}/src/libsma.la
