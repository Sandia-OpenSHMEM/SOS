# -*- Makefile -*-
#
# Copyright 2011 Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
# retains certain rights in this software.
#
# Copyright (c) 2017 Intel Corporation. All rights reserved.
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
	shmem_calloc \
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
	query_thread \
	asym_alloc \
	set_fetch \
	alltoall \
	alltoalls \
	c11_test_shmem_g \
	c11_test_shmem_get \
	c11_test_shmem_p \
	c11_test_shmem_put \
	c11_test_shmem_put_signal \
	c11_test_shmem_atomic_fetch \
	c11_test_shmem_atomic_set \
	c11_test_shmem_atomic_add \
	c11_test_shmem_atomic_inc \
	c11_test_shmem_atomic_and \
	c11_test_shmem_atomic_or \
	c11_test_shmem_atomic_xor \
	c11_test_shmem_atomic_swap \
	c11_test_shmem_atomic_cswap \
	c11_test_shmem_wait_until \
	c11_test_shmem_wait_until_vector \
	c11_shmem_wait_until_all_any_some \
	c11_test_shmem_test \
	c11_shmem_test_vector \
	c11_shmem_test_all_any_some \
	c11_shmem_team_collective_types \
	c11_shmem_team_reduce \
	get_nbi \
	put_nbi \
	rma_coverage \
	collect \
	repeated_barriers \
	repeated_syncs \
	broadcast_active_set \
	reduce_active_set \
	reduce_in_place \
	bcast_in_place \
	collect_active_set \
	atomic_bitwise \
	nop_collectives \
	self_collectives \
	zero_comm \
	sync-size \
	many-ctx \
	shmem_test \
	shmem_ptr \
	shmem_team_ptr \
	shmem_malloc_with_hints \
	put_signal \
	put_signal_nbi \
	signal_fetch \
	signal_wait_until \
	shmem_team_b2b_collectives \
	shmem_team_collect_active_set \
	shmem_team_max \
	shmem_team_reuse_teams \
	shmem_team_shared \
	shmem_team_split_2d \
	shmem_team_translate \
	shmem_team_reduce \
	shmem_team_get_config \
	shmem_team_negative_stride \
	shmem_ctx_get_team \
	atomic_nbi \
	fadd_nbi

# Temporarily disabled: Global exit test tends to fail with MPI-PMI
if !USE_PMI_MPI
check_PROGRAMS += \
	global_exit
endif

if ENABLE_PROFILING
check_PROGRAMS += \
	rma_coverage_pshmem \
	pcontrol
endif

if HAVE_CXX
check_PROGRAMS += \
	cxx_test_shmem_complex \
	cxx_test_shmem_g \
	cxx_test_shmem_get \
	cxx_test_shmem_p \
	cxx_test_shmem_put \
	cxx_test_shmem_atomic_fetch \
	cxx_test_shmem_atomic_set \
	cxx_test_shmem_atomic_add \
	cxx_test_shmem_atomic_inc \
	cxx_test_shmem_atomic_and \
	cxx_test_shmem_atomic_or \
	cxx_test_shmem_atomic_xor \
	cxx_test_shmem_atomic_swap \
	cxx_test_shmem_atomic_cswap \
	cxx_test_shmem_wait_until \
	cxx_test_shmem_test \
	cxx_test_shmem_bitwise_reduce \
	cxx_test_shmem_max_min_reduce \
	cxx_test_shmem_sum_prod_reduce \
	cxx_shmem_test_all
endif

if HAVE_FORTRAN
check_PROGRAMS += \
     hello_f

if !HAVE_LONG_FORTRAN_HEADER
check_PROGRAMS += \
     complex_reductions_f \
     set_fetch_f \
     shmem_info_f
endif
endif

if HAVE_PTHREADS
check_PROGRAMS += \
    mt_a2a \
    mt_contention \
    mt_membar \
    mt_lock_test \
    threading \
    web \
    thread_wait \
    query_thread_funneled 
endif

TESTS = $(check_PROGRAMS)

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

if EXTERNAL_TESTS
bin_PROGRAMS = $(check_PROGRAMS)
AM_CPPFLAGS = -I$(top_srcdir)/test/include
AM_FCFLAGS =
LDADD =
else
AM_CPPFLAGS = -I$(top_builddir)/mpp -I$(top_srcdir)/mpp -I$(top_srcdir)/test/include
AM_FCFLAGS = -I$(top_builddir)/mpp
LDADD = $(top_builddir)/src/libsma.la
endif

if USE_PMI_SIMPLE
LDADD += $(top_builddir)/pmi-simple/libpmi_simple.la
endif

if SHMEMX_TESTS
AM_CFLAGS = -DENABLE_SHMEMX_TESTS
AM_CPPFLAGS += -DENABLE_SHMEMX_TESTS
endif

if ENABLE_DEPRECATED_TESTS
AM_CFLAGS = -DENABLE_DEPRECATED_TESTS
AM_CPPFLAGS += -DENABLE_DEPRECATED_TESTS
endif

# C Tests with special flags
rma_coverage_pshmem_SOURCES = rma_coverage.c
rma_coverage_pshmem_CFLAGS = -DTEST_PSHMEM

query_thread_funneled_SOURCES = query_thread.c
query_thread_funneled_CFLAGS = -DENABLE_THREADS

mt_a2a_SOURCES = mt_a2a.c
mt_a2a_LDFLAGS = $(AM_LDFLAGS) $(PTHREAD_LIBS)
mt_a2a_CFLAGS = $(PTHREAD_CFLAGS)
mt_a2a_LDADD = $(LDADD) $(PTHREAD_CFLAGS)

mt_contention_LDFLAGS = $(AM_LDFLAGS) $(PTHREAD_LIBS)
mt_contention_CFLAGS = $(PTHREAD_CFLAGS)
mt_contention_LDADD = $(LDADD) $(PTHREAD_CFLAGS)

mt_lock_test_SOURCES = mt_lock_test.c mt_lock.c mt_lock.h
mt_lock_test_LDFLAGS = $(AM_LDFLAGS) $(PTHREAD_LIBS)
mt_lock_test_CFLAGS = $(PTHREAD_CFLAGS)
mt_lock_test_LDADD = $(LDADD) $(PTHREAD_CFLAGS)

mt_membar_LDFLAGS = $(AM_LDFLAGS) $(PTHREAD_LIBS)
mt_membar_CFLAGS = $(PTHREAD_CFLAGS)
mt_membar_LDADD = $(LDADD) $(PTHREAD_CFLAGS)

threading_LDFLAGS = $(AM_LDFLAGS) $(PTHREAD_LIBS)
threading_CFLAGS = $(PTHREAD_CFLAGS)
threading_LDADD = $(LDADD) $(PTHREAD_CFLAGS)

web_LDFLAGS = $(AM_LDFLAGS) $(PTHREAD_LIBS)
web_CFLAGS = $(PTHREAD_CFLAGS)
web_LDADD = $(LDADD) $(PTHREAD_CFLAGS)

thread_wait_LDFLAGS = $(AM_LDFLAGS) $(PTHREAD_LIBS)
thread_wait_CFLAGS = $(PTHREAD_CFLAGS)
thread_wait_LDADD = $(LDADD) $(PTHREAD_CFLAGS)

# Fortran Tests (only .c tests use automatic _SOURCES)
hello_f_SOURCES = hello_f.f90
complex_reductions_f_SOURCES = complex_reductions_f.f90
shmem_info_f_SOURCES = shmem_info_f.f90
set_fetch_f_SOURCES = set_fetch_f.f90

# C++ Tests
cxx_test_shmem_complex_SOURCES = cxx_test_shmem_complex.cpp
cxx_test_shmem_g_SOURCES = cxx_test_shmem_g.cpp
cxx_test_shmem_get_SOURCES = cxx_test_shmem_get.cpp
cxx_test_shmem_p_SOURCES = cxx_test_shmem_p.cpp
cxx_test_shmem_put_SOURCES = cxx_test_shmem_put.cpp
cxx_test_shmem_atomic_fetch_SOURCES = cxx_test_shmem_atomic_fetch.cpp
cxx_test_shmem_atomic_set_SOURCES = cxx_test_shmem_atomic_set.cpp
cxx_test_shmem_atomic_add_SOURCES = cxx_test_shmem_atomic_add.cpp
cxx_test_shmem_atomic_inc_SOURCES = cxx_test_shmem_atomic_inc.cpp
cxx_test_shmem_atomic_and_SOURCES = cxx_test_shmem_atomic_and.cpp
cxx_test_shmem_atomic_or_SOURCES = cxx_test_shmem_atomic_or.cpp
cxx_test_shmem_atomic_xor_SOURCES = cxx_test_shmem_atomic_xor.cpp
cxx_test_shmem_atomic_swap_SOURCES = cxx_test_shmem_atomic_swap.cpp
cxx_test_shmem_atomic_cswap_SOURCES = cxx_test_shmem_atomic_cswap.cpp
cxx_test_shmem_wait_until_SOURCES = cxx_test_shmem_wait_until.cpp
cxx_test_shmem_test_SOURCES = cxx_test_shmem_test.cpp
cxx_test_shmem_bitwise_reduce_SOURCES = cxx_test_shmem_bitwise_reduce.cpp
cxx_test_shmem_max_min_reduce_SOURCES = cxx_test_shmem_max_min_reduce.cpp
cxx_test_shmem_sum_prod_reduce_SOURCES = cxx_test_shmem_sum_prod_reduce.cpp
