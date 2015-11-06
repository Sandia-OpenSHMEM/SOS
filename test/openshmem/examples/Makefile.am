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
	adjacent_32bit_amo \
	cpi \
	hello \
	ptp \
	shmem_2dheat \
	shmem_daxpy \
	shmem_heat_image \
	shmem_matrix \
	sping

NPROCS ?= 2
LOG_COMPILER = $(TEST_RUNNER)

AM_CPPFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)

adjacent_32bit_amo_SOURCES = adjacent_32bit_amo.c
adjacent_32bit_amo_LDADD = $(top_builddir)/src/libsma.la

cpi_SOURCES = cpi.c
cpi_LDADD = $(top_builddir)/src/libsma.la

hello_SOURCES = hello.c
hello_LDADD = $(top_builddir)/src/libsma.la

ptp_SOURCES = ptp.c
ptp_LDADD = $(top_builddir)/src/libsma.la

shmem_2dheat_SOURCES = shmem_2dheat.c
shmem_2dheat_LDADD = $(top_builddir)/src/libsma.la -lm

shmem_daxpy_SOURCES = shmem_daxpy.c
shmem_daxpy_LDADD = $(top_builddir)/src/libsma.la

shmem_heat_image_SOURCES = shmem_heat_image.c
shmem_heat_image_LDADD = $(top_builddir)/src/libsma.la

shmem_matrix_SOURCES = shmem_matrix.c
shmem_matrix_LDADD = $(top_builddir)/src/libsma.la

sping_SOURCES = sping.c
sping_LDADD = $(top_builddir)/src/libsma.la
