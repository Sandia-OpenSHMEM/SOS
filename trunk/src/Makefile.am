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

lib_LTLIBRARIES = libsma.la
libsma_la_SOURCES = \
	atomic.c \
	collectives.c \
	data.c \
	malloc.c \
	memory.c \
	runtime.c \
	runtime.h \
	shmem_internal.h \
	symmetric_heap.c \
	synchronization.c

if WANT_PORTALS_SHMEM_RUNTIME
libsma_la_SOURCES += runtime-shmemrt.c
endif

if WANT_MPI_RUNTIME
libsma_la_SOURCES += runtime-mpi.c
endif

if WANT_PMI_RUNTIME
libsma_la_SOURCES += runtime-pmi.c
endif
