# -*- Makefile -*-
#
# Copyright 2011 Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
# retains certain rights in this software.
#
# Copyright (c) 2016 Intel Corporation. All rights reserved.
# This software is available to you under the BSD license.
#
# This file is part of the Sandia OpenSHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.

check_PROGRAMS = \
	hello \
	pi \
	micro_unit_shmem \
	circular_shift \
	accessible_ping \
	max_reduction \
	big_reduction \
	to_all \
	strided_put \
	barrier \
	bcast \
	put1 \
	get1 \
	swap1 \
	ping \
	pingpong \
	shmalloc \
	shrealloc \
	shmemalign \
	get_g \
	iput32 \
	iput64 \
	iput128 \
	iput_short \
	iput_double \
	iput_float \
	iput_long \
	iput_longdouble \
	iput_longlong \
	sping \
	pingpong-short \
	ipgm \
	iput-iget \
	swapm \
	cswap \
	waituntil \
	atomic_inc \
	set_lock \
	test_lock \
	test_lock_cswap \
	fcollect64 \
	bigput \
	bigget \
	ns \
	bcast_flood \
	lfinc \
	shmem_info \
	global_exit \
	asym_alloc \
	set_fetch \
	alltoall \
	alltoalls \
	c11_test_shmem_add \
	c11_test_shmem_fetch \
	c11_test_shmem_g \
	c11_test_shmem_get \
	c11_test_shmem_inc \
	c11_test_shmem_p \
	c11_test_shmem_put \
	c11_test_shmem_set \
	get_nbi \
	put_nbi \
	rma_coverage \
	collect \
	repeated_barriers \
	broadcast_active_set \
	reduce_active_set \
	collect_active_set \
	pcontrol \
	many-ctx

if ENABLE_PROFILING
check_PROGRAMS += \
	rma_coverage_pshmem
endif

if USE_PORTALS4
check_PROGRAMS += \
	 shmem_ct
endif

if HAVE_CXX
check_PROGRAMS += \
    cxx_test_shmem_add \
    cxx_test_shmem_fetch \
    cxx_test_shmem_g \
    cxx_test_shmem_get \
    cxx_test_shmem_inc \
    cxx_test_shmem_p \
    cxx_test_shmem_put \
    cxx_test_shmem_set \
    cxx_test_shmem_complex
endif

if HAVE_FORTRAN
check_PROGRAMS += \
     hello_f \
     complex_reductions_f \
     shmem_info_f

if !HAVE_LONG_FORTRAN_HEADER
check_PROGRAMS += \
     set_fetch_f
endif
endif

if ENABLE_OPENMP
check_PROGRAMS += \
    ctx
endif

if HAVE_PTHREADS
check_PROGRAMS += \
    mt_a2a \
    threading \
    web
endif

noinst_HEADERS = \
    pthread_barrier.h

TESTS = $(check_PROGRAMS)

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_CPPFLAGS = -I$(top_builddir)/mpp
AM_FCFLAGS = -I$(top_builddir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)
LDADD = $(top_builddir)/src/libsma.la

if USE_PMI_SIMPLE
LDADD += $(top_builddir)/pmi-simple/libpmi_simple.la
endif

threading_LDFLAGS = $(PTHREAD_LIBS)
threading_CFLAGS = $(PTHREAD_CFLAGS)

web_LDFLAGS = $(PTHREAD_LIBS)
web_CFLAGS = $(PTHREAD_CFLAGS)

ctx_CFLAGS = $(AM_OPENMP_CFLAGS)

# C Tests with special flags
rma_coverage_pshmem_SOURCES = rma_coverage.c
rma_coverage_pshmem_CFLAGS = -DTEST_PSHMEM

mt_a2a_SOURCES = mt_a2a.c pthread_barrier.h
mt_a2a_LDFLAGS = $(PTHREAD_LIBS)
mt_a2a_CFLAGS = $(PTHREAD_CFLAGS)

# Fortran Tests (only .c tests use automatic _SOURCES)
hello_f_SOURCES = hello_f.f90
complex_reductions_f_SOURCES = complex_reductions_f.f90
shmem_info_f_SOURCES = shmem_info_f.f90
set_fetch_f_SOURCES = set_fetch_f.f90

# C++ Tests
cxx_test_shmem_add_SOURCES = cxx_test_shmem_add.cpp
cxx_test_shmem_g_SOURCES = cxx_test_shmem_g.cpp
cxx_test_shmem_get_SOURCES = cxx_test_shmem_get.cpp
cxx_test_shmem_inc_SOURCES = cxx_test_shmem_inc.cpp
cxx_test_shmem_p_SOURCES = cxx_test_shmem_p.cpp
cxx_test_shmem_put_SOURCES = cxx_test_shmem_put.cpp
cxx_test_shmem_fetch_SOURCES = cxx_test_shmem_fetch.cpp
cxx_test_shmem_set_SOURCES = cxx_test_shmem_set.cpp
cxx_test_shmem_complex_SOURCES = cxx_test_shmem_complex.cpp
