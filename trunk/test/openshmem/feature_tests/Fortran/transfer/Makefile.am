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
	test_shmem_get_01_char \
	test_shmem_get_01_double \
	test_shmem_get_01_int4 \
	test_shmem_get_01_logical \
	test_shmem_get_01_real4 \
	test_shmem_get_02_char \
	test_shmem_get_02_double \
	test_shmem_get_02_int4 \
	test_shmem_get_02_logical \
	test_shmem_get_02_real4 \
	test_shmem_get_03_char \
	test_shmem_get_03_double \
	test_shmem_get_03_int4 \
	test_shmem_get_03_logical \
	test_shmem_get_03_real4 \
	test_shmem_get_04_char \
	test_shmem_get_04_double \
	test_shmem_get_04_int4 \
	test_shmem_get_04_logical \
	test_shmem_get_04_real4 \
	test_shmem_get_05_char \
	test_shmem_get_05_double \
	test_shmem_get_05_int4 \
	test_shmem_get_05_logical \
	test_shmem_get_05_real4 \
	test_shmem_iget_01_double \
	test_shmem_iget_01_int4 \
	test_shmem_iget_01_logical \
	test_shmem_iget_01_real4 \
	test_shmem_iget_02_double \
	test_shmem_iget_02_int4 \
	test_shmem_iget_02_logical \
	test_shmem_iget_02_real4 \
	test_shmem_iget_03_double \
	test_shmem_iget_03_int4 \
	test_shmem_iget_03_logical \
	test_shmem_iget_03_real4 \
	test_shmem_iget_04_double \
	test_shmem_iget_04_int4 \
	test_shmem_iget_04_logical \
	test_shmem_iget_04_real4 \
	test_shmem_iget_05_double \
	test_shmem_iget_05_int4 \
	test_shmem_iget_05_logical \
	test_shmem_iget_05_real4 \
	test_shmem_iput_01_double \
	test_shmem_iput_01_int4 \
	test_shmem_iput_01_logical \
	test_shmem_iput_01_real4 \
	test_shmem_iput_02_double \
	test_shmem_iput_02_int4 \
	test_shmem_iput_02_logical \
	test_shmem_iput_02_real4 \
	test_shmem_iput_03_double \
	test_shmem_iput_03_int4 \
	test_shmem_iput_03_logical \
	test_shmem_iput_03_real4 \
	test_shmem_iput_04_double \
	test_shmem_iput_04_int4 \
	test_shmem_iput_04_logical \
	test_shmem_iput_04_real4 \
	test_shmem_iput_05_double \
	test_shmem_iput_05_int4 \
	test_shmem_iput_05_logical \
	test_shmem_iput_05_real4 \
	test_shmem_put_01_char \
	test_shmem_put_01_double \
	test_shmem_put_01_int4 \
	test_shmem_put_01_logical \
	test_shmem_put_01_real4 \
	test_shmem_put_02_char \
	test_shmem_put_02_double \
	test_shmem_put_02_int4 \
	test_shmem_put_02_logical \
	test_shmem_put_02_real4 \
	test_shmem_put_03_char \
	test_shmem_put_03_double \
	test_shmem_put_03_int4 \
	test_shmem_put_03_logical \
	test_shmem_put_03_real4 \
	test_shmem_put_04_char \
	test_shmem_put_04_double \
	test_shmem_put_04_int4 \
	test_shmem_put_04_logical \
	test_shmem_put_04_real4 \
	test_shmem_put_05_char \
	test_shmem_put_05_double \
	test_shmem_put_05_int4 \
	test_shmem_put_05_logical \
	test_shmem_put_05_real4

XFAIL_TESTS = \
	test_shmem_put_04_char \
	test_shmem_put_04_double \
	test_shmem_put_04_int4 \
	test_shmem_put_04_logical \
	test_shmem_put_04_real4 \
	test_shmem_put_05_char \
	test_shmem_put_05_double \
	test_shmem_put_05_int4 \
	test_shmem_put_05_logical \
	test_shmem_put_05_real4 \
	test_shmem_iput_04_double \
	test_shmem_iput_04_int4 \
	test_shmem_iput_04_logical \
	test_shmem_iput_04_real4 \
	test_shmem_iput_05_double \
	test_shmem_iput_05_int4 \
	test_shmem_iput_05_logical \
	test_shmem_iput_05_real4 \
	test_shmem_iget_04_double \
	test_shmem_iget_04_int4 \
	test_shmem_iget_04_logical \
	test_shmem_iget_04_real4 \
	test_shmem_iget_05_double \
	test_shmem_iget_05_int4 \
	test_shmem_iget_05_logical \
	test_shmem_iget_05_real4


PROCS ?= 2
RUNCMD ?= yod -np $(PROCS)
TESTS_ENVIRONMENT ?= ${top_srcdir}/test/openshmem/feature_tests/Fortran/test_wrapper.sh $(RUNCMD)

AM_FCFLAGS = -I$(top_srcdir)/mpp

test_shmem_get_01_char_SOURCES = test_shmem_get_01_char.f90
test_shmem_get_01_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_01_double_SOURCES = test_shmem_get_01_double.f90
test_shmem_get_01_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_01_int4_SOURCES = test_shmem_get_01_int4.f90
test_shmem_get_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_01_logical_SOURCES = test_shmem_get_01_logical.f90
test_shmem_get_01_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_01_real4_SOURCES = test_shmem_get_01_real4.f90
test_shmem_get_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_02_char_SOURCES = test_shmem_get_02_char.f90
test_shmem_get_02_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_02_double_SOURCES = test_shmem_get_02_double.f90
test_shmem_get_02_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_02_int4_SOURCES = test_shmem_get_02_int4.f90
test_shmem_get_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_02_logical_SOURCES = test_shmem_get_02_logical.f90
test_shmem_get_02_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_02_real4_SOURCES = test_shmem_get_02_real4.f90
test_shmem_get_02_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_03_char_SOURCES = test_shmem_get_03_char.f90
test_shmem_get_03_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_03_double_SOURCES = test_shmem_get_03_double.f90
test_shmem_get_03_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_03_int4_SOURCES = test_shmem_get_03_int4.f90
test_shmem_get_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_03_logical_SOURCES = test_shmem_get_03_logical.f90
test_shmem_get_03_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_03_real4_SOURCES = test_shmem_get_03_real4.f90
test_shmem_get_03_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_04_char_SOURCES = test_shmem_get_04_char.f90
test_shmem_get_04_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_04_double_SOURCES = test_shmem_get_04_double.f90
test_shmem_get_04_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_04_int4_SOURCES = test_shmem_get_04_int4.f90
test_shmem_get_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_04_logical_SOURCES = test_shmem_get_04_logical.f90
test_shmem_get_04_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_04_real4_SOURCES = test_shmem_get_04_real4.f90
test_shmem_get_04_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_05_char_SOURCES = test_shmem_get_05_char.f90
test_shmem_get_05_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_05_double_SOURCES = test_shmem_get_05_double.f90
test_shmem_get_05_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_05_int4_SOURCES = test_shmem_get_05_int4.f90
test_shmem_get_05_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_05_logical_SOURCES = test_shmem_get_05_logical.f90
test_shmem_get_05_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_get_05_real4_SOURCES = test_shmem_get_05_real4.f90
test_shmem_get_05_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_01_double_SOURCES = test_shmem_iget_01_double.f90
test_shmem_iget_01_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_01_int4_SOURCES = test_shmem_iget_01_int4.f90
test_shmem_iget_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_01_logical_SOURCES = test_shmem_iget_01_logical.f90
test_shmem_iget_01_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_01_real4_SOURCES = test_shmem_iget_01_real4.f90
test_shmem_iget_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_02_double_SOURCES = test_shmem_iget_02_double.f90
test_shmem_iget_02_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_02_int4_SOURCES = test_shmem_iget_02_int4.f90
test_shmem_iget_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_02_logical_SOURCES = test_shmem_iget_02_logical.f90
test_shmem_iget_02_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_02_real4_SOURCES = test_shmem_iget_02_real4.f90
test_shmem_iget_02_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_03_double_SOURCES = test_shmem_iget_03_double.f90
test_shmem_iget_03_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_03_int4_SOURCES = test_shmem_iget_03_int4.f90
test_shmem_iget_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_03_logical_SOURCES = test_shmem_iget_03_logical.f90
test_shmem_iget_03_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_03_real4_SOURCES = test_shmem_iget_03_real4.f90
test_shmem_iget_03_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_04_double_SOURCES = test_shmem_iget_04_double.f90
test_shmem_iget_04_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_04_int4_SOURCES = test_shmem_iget_04_int4.f90
test_shmem_iget_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_04_logical_SOURCES = test_shmem_iget_04_logical.f90
test_shmem_iget_04_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_04_real4_SOURCES = test_shmem_iget_04_real4.f90
test_shmem_iget_04_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_05_double_SOURCES = test_shmem_iget_05_double.f90
test_shmem_iget_05_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_05_int4_SOURCES = test_shmem_iget_05_int4.f90
test_shmem_iget_05_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_05_logical_SOURCES = test_shmem_iget_05_logical.f90
test_shmem_iget_05_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iget_05_real4_SOURCES = test_shmem_iget_05_real4.f90
test_shmem_iget_05_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_01_double_SOURCES = test_shmem_iput_01_double.f90
test_shmem_iput_01_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_01_int4_SOURCES = test_shmem_iput_01_int4.f90
test_shmem_iput_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_01_logical_SOURCES = test_shmem_iput_01_logical.f90
test_shmem_iput_01_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_01_real4_SOURCES = test_shmem_iput_01_real4.f90
test_shmem_iput_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_02_double_SOURCES = test_shmem_iput_02_double.f90
test_shmem_iput_02_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_02_int4_SOURCES = test_shmem_iput_02_int4.f90
test_shmem_iput_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_02_logical_SOURCES = test_shmem_iput_02_logical.f90
test_shmem_iput_02_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_02_real4_SOURCES = test_shmem_iput_02_real4.f90
test_shmem_iput_02_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_03_double_SOURCES = test_shmem_iput_03_double.f90
test_shmem_iput_03_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_03_int4_SOURCES = test_shmem_iput_03_int4.f90
test_shmem_iput_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_03_logical_SOURCES = test_shmem_iput_03_logical.f90
test_shmem_iput_03_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_03_real4_SOURCES = test_shmem_iput_03_real4.f90
test_shmem_iput_03_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_04_double_SOURCES = test_shmem_iput_04_double.f90
test_shmem_iput_04_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_04_int4_SOURCES = test_shmem_iput_04_int4.f90
test_shmem_iput_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_04_logical_SOURCES = test_shmem_iput_04_logical.f90
test_shmem_iput_04_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_04_real4_SOURCES = test_shmem_iput_04_real4.f90
test_shmem_iput_04_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_05_double_SOURCES = test_shmem_iput_05_double.f90
test_shmem_iput_05_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_05_int4_SOURCES = test_shmem_iput_05_int4.f90
test_shmem_iput_05_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_05_logical_SOURCES = test_shmem_iput_05_logical.f90
test_shmem_iput_05_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_iput_05_real4_SOURCES = test_shmem_iput_05_real4.f90
test_shmem_iput_05_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_01_char_SOURCES = test_shmem_put_01_char.f90
test_shmem_put_01_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_01_double_SOURCES = test_shmem_put_01_double.f90
test_shmem_put_01_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_01_int4_SOURCES = test_shmem_put_01_int4.f90
test_shmem_put_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_01_logical_SOURCES = test_shmem_put_01_logical.f90
test_shmem_put_01_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_01_real4_SOURCES = test_shmem_put_01_real4.f90
test_shmem_put_01_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_02_char_SOURCES = test_shmem_put_02_char.f90
test_shmem_put_02_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_02_double_SOURCES = test_shmem_put_02_double.f90
test_shmem_put_02_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_02_int4_SOURCES = test_shmem_put_02_int4.f90
test_shmem_put_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_02_logical_SOURCES = test_shmem_put_02_logical.f90
test_shmem_put_02_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_02_real4_SOURCES = test_shmem_put_02_real4.f90
test_shmem_put_02_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_03_char_SOURCES = test_shmem_put_03_char.f90
test_shmem_put_03_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_03_double_SOURCES = test_shmem_put_03_double.f90
test_shmem_put_03_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_03_int4_SOURCES = test_shmem_put_03_int4.f90
test_shmem_put_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_03_logical_SOURCES = test_shmem_put_03_logical.f90
test_shmem_put_03_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_03_real4_SOURCES = test_shmem_put_03_real4.f90
test_shmem_put_03_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_04_char_SOURCES = test_shmem_put_04_char.f90
test_shmem_put_04_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_04_double_SOURCES = test_shmem_put_04_double.f90
test_shmem_put_04_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_04_int4_SOURCES = test_shmem_put_04_int4.f90
test_shmem_put_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_04_logical_SOURCES = test_shmem_put_04_logical.f90
test_shmem_put_04_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_04_real4_SOURCES = test_shmem_put_04_real4.f90
test_shmem_put_04_real4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_05_char_SOURCES = test_shmem_put_05_char.f90
test_shmem_put_05_char_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_05_double_SOURCES = test_shmem_put_05_double.f90
test_shmem_put_05_double_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_05_int4_SOURCES = test_shmem_put_05_int4.f90
test_shmem_put_05_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_05_logical_SOURCES = test_shmem_put_05_logical.f90
test_shmem_put_05_logical_LDADD = ${top_builddir}/src/libsma.la

test_shmem_put_05_real4_SOURCES = test_shmem_put_05_real4.f90
test_shmem_put_05_real4_LDADD = ${top_builddir}/src/libsma.la
