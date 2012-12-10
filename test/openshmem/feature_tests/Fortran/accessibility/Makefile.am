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
	test_shmem_acc_01 \
	test_shmem_acc_02 \
	test_shmem_acc_mem_01_char \
	test_shmem_acc_mem_01_double \
	test_shmem_acc_mem_01_int4 \
	test_shmem_acc_mem_01_int8 \
	test_shmem_acc_mem_01_logical \
	test_shmem_acc_mem_01_real4 \
	test_shmem_acc_mem_01_real8 \
	test_shmem_acc_mem_02_char \
	test_shmem_acc_mem_02_double \
	test_shmem_acc_mem_02_int4 \
	test_shmem_acc_mem_02_int8 \
	test_shmem_acc_mem_02_logical \
	test_shmem_acc_mem_02_real4 \
	test_shmem_acc_mem_02_real8 \
	test_shmem_acc_mem_03_char \
	test_shmem_acc_mem_03_double \
	test_shmem_acc_mem_03_int4 \
	test_shmem_acc_mem_03_int8 \
	test_shmem_acc_mem_03_logical \
	test_shmem_acc_mem_03_real4 \
	test_shmem_acc_mem_03_real8 \
	test_shmem_acc_mem_04_char \
	test_shmem_acc_mem_04_double \
	test_shmem_acc_mem_04_int4 \
	test_shmem_acc_mem_04_int8 \
	test_shmem_acc_mem_04_logical \
	test_shmem_acc_mem_04_real4 \
	test_shmem_acc_mem_04_real8 \
	test_shmem_acc_mem_05_char \
	test_shmem_acc_mem_05_double \
	test_shmem_acc_mem_05_int4 \
	test_shmem_acc_mem_05_int8 \
	test_shmem_acc_mem_05_logical \
	test_shmem_acc_mem_05_real4 \
	test_shmem_acc_mem_05_real8

PROCS ?= 2
RUNCMD ?= yod -np $(PROCS)
TESTS_ENVIRONMENT ?= ${top_srcdir}/test/openshmem/feature_tests/Fortran/test_wrapper.sh $(RUNCMD)

AM_FCFLAGS = -I$(top_srcdir)/mpp

test_shmem_acc_01_SOURCES = test_shmem_acc_01.f90
test_shmem_acc_01_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_02_SOURCES = test_shmem_acc_02.f90
test_shmem_acc_02_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_01_char_SOURCES = test_shmem_acc_mem_01_char.f90
test_shmem_acc_mem_01_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_01_double_SOURCES = test_shmem_acc_mem_01_double.f90
test_shmem_acc_mem_01_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_01_int4_SOURCES = test_shmem_acc_mem_01_int4.f90
test_shmem_acc_mem_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_01_int8_SOURCES = test_shmem_acc_mem_01_int8.f90
test_shmem_acc_mem_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_01_logical_SOURCES = test_shmem_acc_mem_01_logical.f90
test_shmem_acc_mem_01_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_01_real4_SOURCES = test_shmem_acc_mem_01_real4.f90
test_shmem_acc_mem_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_01_real8_SOURCES = test_shmem_acc_mem_01_real8.f90
test_shmem_acc_mem_01_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_02_char_SOURCES = test_shmem_acc_mem_02_char.f90
test_shmem_acc_mem_02_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_02_double_SOURCES = test_shmem_acc_mem_02_double.f90
test_shmem_acc_mem_02_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_02_int4_SOURCES = test_shmem_acc_mem_02_int4.f90
test_shmem_acc_mem_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_02_int8_SOURCES = test_shmem_acc_mem_02_int8.f90
test_shmem_acc_mem_02_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_02_logical_SOURCES = test_shmem_acc_mem_02_logical.f90
test_shmem_acc_mem_02_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_02_real4_SOURCES = test_shmem_acc_mem_02_real4.f90
test_shmem_acc_mem_02_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_02_real8_SOURCES = test_shmem_acc_mem_02_real8.f90
test_shmem_acc_mem_02_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_03_char_SOURCES = test_shmem_acc_mem_03_char.f90
test_shmem_acc_mem_03_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_03_double_SOURCES = test_shmem_acc_mem_03_double.f90
test_shmem_acc_mem_03_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_03_int4_SOURCES = test_shmem_acc_mem_03_int4.f90
test_shmem_acc_mem_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_03_int8_SOURCES = test_shmem_acc_mem_03_int8.f90
test_shmem_acc_mem_03_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_03_logical_SOURCES = test_shmem_acc_mem_03_logical.f90
test_shmem_acc_mem_03_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_03_real4_SOURCES = test_shmem_acc_mem_03_real4.f90
test_shmem_acc_mem_03_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_03_real8_SOURCES = test_shmem_acc_mem_03_real8.f90
test_shmem_acc_mem_03_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_04_char_SOURCES = test_shmem_acc_mem_04_char.f90
test_shmem_acc_mem_04_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_04_double_SOURCES = test_shmem_acc_mem_04_double.f90
test_shmem_acc_mem_04_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_04_int4_SOURCES = test_shmem_acc_mem_04_int4.f90
test_shmem_acc_mem_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_04_int8_SOURCES = test_shmem_acc_mem_04_int8.f90
test_shmem_acc_mem_04_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_04_logical_SOURCES = test_shmem_acc_mem_04_logical.f90
test_shmem_acc_mem_04_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_04_real4_SOURCES = test_shmem_acc_mem_04_real4.f90
test_shmem_acc_mem_04_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_04_real8_SOURCES = test_shmem_acc_mem_04_real8.f90
test_shmem_acc_mem_04_real8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_05_char_SOURCES = test_shmem_acc_mem_05_char.f90
test_shmem_acc_mem_05_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_05_double_SOURCES = test_shmem_acc_mem_05_double.f90
test_shmem_acc_mem_05_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_05_int4_SOURCES = test_shmem_acc_mem_05_int4.f90
test_shmem_acc_mem_05_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_05_int8_SOURCES = test_shmem_acc_mem_05_int8.f90
test_shmem_acc_mem_05_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_05_logical_SOURCES = test_shmem_acc_mem_05_logical.f90
test_shmem_acc_mem_05_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_05_real4_SOURCES = test_shmem_acc_mem_05_real4.f90
test_shmem_acc_mem_05_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_acc_mem_05_real8_SOURCES = test_shmem_acc_mem_05_real8.f90
test_shmem_acc_mem_05_real8_LDADD = ${top_builddir}/src/libsma.la

