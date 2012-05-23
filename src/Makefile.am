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
	runtime.h \
	runtime-pmi.c \
	shmem_internal.h \
	init.c \
	query.c \
	accessibility.c \
	malloc.c \
	symmetric_heap.c \
	remote_pointer.c \
	data.c \
	atomic.c \
	synchronization.c \
	collectives.c \
	lock.c \
	cache_management.c

if USE_PORTALS4
libsma_la_SOURCES += \
	transport_portals4.h \
	transport_portals4.c
endif

if USE_XPMEM
libsma_la_SOURCES += \
	transport_xpmem.h \
	transport_xpmem.c
endif
