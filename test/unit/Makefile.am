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

check_PROGRAMS = $(TESTS)

TESTS = \
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
	reduce_active_set

if ENABLE_PROFILING
TESTS += \
	rma_coverage_pshmem
endif

if USE_PORTALS4
TESTS += \
	 shmem_ct
endif

if HAVE_CXX
TESTS += \
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
TESTS += \
     hello_f \
     complex_reductions_f \
     shmem_info_f

if !HAVE_LONG_FORTRAN_HEADER
TESTS += \
     set_fetch_f
endif
endif

if HAVE_PTHREADS
TESTS += \
    mt_a2a
endif

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_CPPFLAGS = -I$(top_builddir)/mpp
AM_FCFLAGS = -I$(top_builddir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)
LDADD = $(top_builddir)/src/libsma.la

if USE_SHMEM_PMI
LDADD += $(top_builddir)/shmem_pmi/libshmem_pmi.la
endif

pi_SOURCES = pi.c

rma_coverage_SOURCES = rma_coverage.c

rma_coverage_pshmem_SOURCES = rma_coverage.c
rma_coverage_pshmem_CFLAGS = -DTEST_PSHMEM

hello_SOURCES = hello.c

hello_f_SOURCES = hello_f.f90

complex_reductions_f_SOURCES = complex_reductions_f.f90

micro_unit_shmem_SOURCES = micro_unit_shmem.c

circular_shift_SOURCES = circular_shift.c

accessible_ping_SOURCES = accessible_ping.c

max_reduction_SOURCES = max_reduction.c

big_reduction_SOURCES = big_reduction.c

strided_put_SOURCES = strided_put.c

barrier_SOURCES = barrier.c

bcast_SOURCES = bcast.c

put1_SOURCES = put1.c

get1_SOURCES = get1.c

swap1_SOURCES = swap1.c

ping_SOURCES = ping.c

pingpong_SOURCES = pingpong.c

iput32_SOURCES = iput32.c

iput64_SOURCES = iput64.c

iput128_SOURCES = iput128.c

iput_short_SOURCES = iput_short.c

iput_double_SOURCES = iput_double.c

iput_float_SOURCES = iput_float.c

iput_long_SOURCES = iput_long.c

iput_longdouble_SOURCES = iput_longdouble.c

iput_longlong_SOURCES = iput_longlong.c

sping_SOURCES = sping.c

pingpong_short_SOURCES = pingpong-short.c

iput_iget_SOURCES = iput-iget.c

ipgm_SOURCES = ipgm.c

shmalloc_SOURCES = shmalloc.c

shrealloc_SOURCES = shrealloc.c

shmemalign_SOURCES = shmemalign.c

get_g_SOURCES = get_g.c

swapm_SOURCES = swapm.c

cswap_SOURCES = cswap.c

waituntil_SOURCES = waituntil.c

atomic_inc_SOURCES = atomic_inc.c

set_lock_SOURCES = set_lock.c

test_lock_SOURCES = test_lock.c

test_lock_cswap_SOURCES = test_lock_cswap.c

fcollect64_SOURCES = fcollect64.c

bigput_SOURCES = bigput.c

bigget_SOURCES = bigget.c

to_all_SOURCES = to_all.c

ns_SOURCES = ns.c

bcast_flood_SOURCES = bcast_flood.c

lfinc_SOURCES = lfinc.c

shmem_ct_SOURCES = shmem_ct.c

mt_a2a_SOURCES = mt_a2a.c pthread_barrier.h
mt_a2a_LDFLAGS = $(PTHREAD_LIBS)
mt_a2a_CFLAGS = $(PTHREAD_CFLAGS)

global_exit_SOURCES = global_exit.c

shmem_info_SOURCES = shmem_info.c

shmem_info_f_SOURCES = shmem_info_f.f90

asym_alloc_SOURCES = asym_alloc.c

set_fetch_SOURCES = set_fetch.c

set_fetch_f_SOURCES = set_fetch_f.f90

alltoall_SOURCES = alltoall.c

alltoalls_SOURCES = alltoalls.c

c11_test_shmem_add_SOURCES = c11_test_shmem_add.c

c11_test_shmem_g_SOURCES = c11_test_shmem_g.c

c11_test_shmem_get_SOURCES = c11_test_shmem_get.c

c11_test_shmem_inc_SOURCES = c11_test_shmem_inc.c

c11_test_shmem_p_SOURCES = c11_test_shmem_p.c

c11_test_shmem_put_SOURCES = c11_test_shmem_put.c

c11_test_shmem_fetch_SOURCES = c11_test_shmem_fetch.c

c11_test_shmem_set_SOURCES = c11_test_shmem_set.c

cxx_test_shmem_add_SOURCES = cxx_test_shmem_add.cpp

cxx_test_shmem_g_SOURCES = cxx_test_shmem_g.cpp

cxx_test_shmem_get_SOURCES = cxx_test_shmem_get.cpp

cxx_test_shmem_inc_SOURCES = cxx_test_shmem_inc.cpp

cxx_test_shmem_p_SOURCES = cxx_test_shmem_p.cpp

cxx_test_shmem_put_SOURCES = cxx_test_shmem_put.cpp

cxx_test_shmem_fetch_SOURCES = cxx_test_shmem_fetch.cpp

cxx_test_shmem_set_SOURCES = cxx_test_shmem_set.cpp

cxx_test_shmem_complex_SOURCES = cxx_test_shmem_complex.cpp

get_nbi_SOURCES = get_nbi.c

put_nbi_SOURCES = put_nbi.c

collect_SOURCES = collect.c

repeated_barriers_SOURCES = repeated_barriers.c

broadcast_active_set_SOURCES = broadcast_active_set.c

reduce_active_set_SOURCES = reduce_active_set.c
