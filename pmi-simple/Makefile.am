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

AM_CPPFLAGS= -I$(top_srcdir)/src

if USE_PMI_SIMPLE
lib_LTLIBRARIES = libpmi_simple.la
libpmi_simple_la_SOURCES = \
	simple_pmi.c \
	simple_pmiutil.c \
	simple_pmiutil.h \
	pmi.h \
	mpl.h \
	mpir_mem.h \
	mpl_sockaddr.c \
	mpl_sockaddr.h
endif
