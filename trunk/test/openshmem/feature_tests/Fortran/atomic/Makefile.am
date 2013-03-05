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
	test_shmem_add_01_int4 \
	test_shmem_add_01_int8 \
	test_shmem_add_02_int4 \
	test_shmem_add_02_int8 \
	test_shmem_add_03_int4 \
	test_shmem_add_03_int8 \
	test_shmem_cswap_01_int4 \
	test_shmem_cswap_01_int8 \
	test_shmem_cswap_02_int4 \
	test_shmem_cswap_02_int8 \
	test_shmem_cswap_03_int4 \
	test_shmem_cswap_03_int8 \
	test_shmem_fadd_01_int4 \
	test_shmem_fadd_01_int8 \
	test_shmem_finc_01_int4 \
	test_shmem_finc_01_int8 \
	test_shmem_finc_02_int4 \
	test_shmem_finc_02_int8 \
	test_shmem_finc_03_int4 \
	test_shmem_finc_03_int8 \
	test_shmem_inc_01_int4 \
	test_shmem_inc_01_int8 \
	test_shmem_inc_02_int4 \
	test_shmem_inc_02_int8 \
	test_shmem_inc_03_int4 \
	test_shmem_inc_03_int8 \
	test_shmem_swap_01_int4 \
	test_shmem_swap_01_int8 \
	test_shmem_swap_02_int4 \
	test_shmem_swap_02_int8 \
	test_shmem_swap_03_int4 \
	test_shmem_swap_03_int8

XFAIL_TESTS =

if ENABLE_ERROR_CHECKING
# a number of tests check for puts outside the symmetric area.  Only
# run those tests if we've enabled error checking
TESTS += \
	test_shmem_add_04_int4 \
	test_shmem_add_04_int8 \
	test_shmem_cswap_04_int4 \
	test_shmem_cswap_04_int8 \
	test_shmem_finc_04_int4 \
	test_shmem_finc_04_int8 \
	test_shmem_finc_05_int4 \
	test_shmem_finc_05_int8 \
	test_shmem_inc_04_int4 \
	test_shmem_inc_04_int8 \
	test_shmem_swap_04_int4 \
	test_shmem_swap_04_int8 \
	test_shmem_inc_05_int4 \
	test_shmem_inc_05_int8 \
	test_shmem_swap_05_int4 \
	test_shmem_swap_05_int8

XFAIL_TESTS += \
	test_shmem_add_04_int4 \
	test_shmem_add_04_int8 \
	test_shmem_cswap_04_int4 \
	test_shmem_cswap_04_int8 \
	test_shmem_finc_04_int4 \
	test_shmem_finc_04_int8 \
	test_shmem_finc_05_int4 \
	test_shmem_finc_05_int8 \
	test_shmem_inc_04_int4 \
	test_shmem_inc_04_int8 \
	test_shmem_swap_04_int4 \
	test_shmem_swap_04_int8 \
	test_shmem_inc_05_int4 \
	test_shmem_inc_05_int8 \
	test_shmem_swap_05_int4 \
	test_shmem_swap_05_int8
endif

PROCS ?= 2
RUNCMD ?= yod -np $(PROCS)
TESTS_ENVIRONMENT ?= ${top_srcdir}/test/openshmem/feature_tests/Fortran/test_wrapper.sh $(RUNCMD)

AM_FCFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

test_shmem_add_01_int4_SOURCES = test_shmem_add_01_int4.f90
test_shmem_add_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_add_01_int8_SOURCES = test_shmem_add_01_int8.f90
test_shmem_add_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_add_02_int4_SOURCES = test_shmem_add_02_int4.f90
test_shmem_add_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_add_02_int8_SOURCES = test_shmem_add_02_int8.f90
test_shmem_add_02_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_add_03_int4_SOURCES = test_shmem_add_03_int4.f90
test_shmem_add_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_add_03_int8_SOURCES = test_shmem_add_03_int8.f90
test_shmem_add_03_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_add_04_int4_SOURCES = test_shmem_add_04_int4.f90
test_shmem_add_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_add_04_int8_SOURCES = test_shmem_add_04_int8.f90
test_shmem_add_04_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_cswap_01_int4_SOURCES = test_shmem_cswap_01_int4.f90
test_shmem_cswap_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_cswap_01_int8_SOURCES = test_shmem_cswap_01_int8.f90
test_shmem_cswap_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_cswap_02_int4_SOURCES = test_shmem_cswap_02_int4.f90
test_shmem_cswap_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_cswap_02_int8_SOURCES = test_shmem_cswap_02_int8.f90
test_shmem_cswap_02_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_cswap_03_int4_SOURCES = test_shmem_cswap_03_int4.f90
test_shmem_cswap_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_cswap_03_int8_SOURCES = test_shmem_cswap_03_int8.f90
test_shmem_cswap_03_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_cswap_04_int4_SOURCES = test_shmem_cswap_04_int4.f90
test_shmem_cswap_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_cswap_04_int8_SOURCES = test_shmem_cswap_04_int8.f90
test_shmem_cswap_04_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_fadd_01_int4_SOURCES = test_shmem_fadd_01_int4.f90
test_shmem_fadd_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_fadd_01_int8_SOURCES = test_shmem_fadd_01_int8.f90
test_shmem_fadd_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_01_int4_SOURCES = test_shmem_finc_01_int4.f90
test_shmem_finc_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_01_int8_SOURCES = test_shmem_finc_01_int8.f90
test_shmem_finc_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_02_int4_SOURCES = test_shmem_finc_02_int4.f90
test_shmem_finc_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_02_int8_SOURCES = test_shmem_finc_02_int8.f90
test_shmem_finc_02_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_03_int4_SOURCES = test_shmem_finc_03_int4.f90
test_shmem_finc_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_03_int8_SOURCES = test_shmem_finc_03_int8.f90
test_shmem_finc_03_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_04_int4_SOURCES = test_shmem_finc_04_int4.f90
test_shmem_finc_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_04_int8_SOURCES = test_shmem_finc_04_int8.f90
test_shmem_finc_04_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_05_int4_SOURCES = test_shmem_finc_05_int4.f90
test_shmem_finc_05_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_finc_05_int8_SOURCES = test_shmem_finc_05_int8.f90
test_shmem_finc_05_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_01_int4_SOURCES = test_shmem_inc_01_int4.f90
test_shmem_inc_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_01_int8_SOURCES = test_shmem_inc_01_int8.f90
test_shmem_inc_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_02_int4_SOURCES = test_shmem_inc_02_int4.f90
test_shmem_inc_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_02_int8_SOURCES = test_shmem_inc_02_int8.f90
test_shmem_inc_02_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_03_int4_SOURCES = test_shmem_inc_03_int4.f90
test_shmem_inc_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_03_int8_SOURCES = test_shmem_inc_03_int8.f90
test_shmem_inc_03_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_04_int4_SOURCES = test_shmem_inc_04_int4.f90
test_shmem_inc_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_04_int8_SOURCES = test_shmem_inc_04_int8.f90
test_shmem_inc_04_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_05_int4_SOURCES = test_shmem_inc_05_int4.f90
test_shmem_inc_05_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_inc_05_int8_SOURCES = test_shmem_inc_05_int8.f90
test_shmem_inc_05_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_01_int4_SOURCES = test_shmem_swap_01_int4.f90
test_shmem_swap_01_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_01_int8_SOURCES = test_shmem_swap_01_int8.f90
test_shmem_swap_01_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_02_int4_SOURCES = test_shmem_swap_02_int4.f90
test_shmem_swap_02_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_02_int8_SOURCES = test_shmem_swap_02_int8.f90
test_shmem_swap_02_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_03_int4_SOURCES = test_shmem_swap_03_int4.f90
test_shmem_swap_03_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_03_int8_SOURCES = test_shmem_swap_03_int8.f90
test_shmem_swap_03_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_04_int4_SOURCES = test_shmem_swap_04_int4.f90
test_shmem_swap_04_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_04_int8_SOURCES = test_shmem_swap_04_int8.f90
test_shmem_swap_04_int8_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_05_int4_SOURCES = test_shmem_swap_05_int4.f90
test_shmem_swap_05_int4_LDADD = ${top_builddir}/src/libsma.la

test_shmem_swap_05_int8_SOURCES = test_shmem_swap_05_int8.f90
test_shmem_swap_05_int8_LDADD = ${top_builddir}/src/libsma.la
