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

TESTS =

if ENABLE_LENGTHY_TESTS
TESTS += \
	NPshmem \
	shmemlatency \
	msgrate
else
check_PROGRAMS += \
	NPshmem \
	shmemlatency \
	msgrate
endif

PROCS ?= 2

TESTS_ENVIRONMENT = yod -np $(PROCS)

AM_CPPFLAGS = -I$(top_srcdir)/mpp
AM_LDFLAGS = $(LIBTOOL_WRAPPER_LDFLAGS)
LDADD = $(top_builddir)/src/libsma.la

NPshmem_SOURCES = netpipe.c netpipe.h shmem.c
NPshmem_CPPFLAGS = $(AM_CPPFLAGS) -DSHMEM

shmemlatency_SOURCES = shmemlatency.c

msgrate_SOURCES = msgrate.c

