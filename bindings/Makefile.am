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

EXTRA_DIST = shmem_bind_c.m4   \
             shmem_bind_c11.m4 \
             shmem_bind_cxx.m4 \
             shmem_bind_c11_extras_none.m4 \
             shmem_bind_cxx_extras_none.m4 \
             shmem_bind_f.m4

BUILT_SOURCES = shmem_bind_c11_extras.m4 \
                shmem_bind_cxx_extras.m4

CLEANFILES = $(BUILT_SOURCES)

noinst_PROGRAMS =

if HAVE_C11
noinst_PROGRAMS += generate_bind_c11_extras

generate_bind_c11_extras_SOURCES = generate_bind_c11_extras.c
shmem_bind_c11_extras.m4: generate_bind_c11_extras
	$(AM_V_GEN)./generate_bind_c11_extras > $@

else
shmem_bind_c11_extras.m4:
	$(AM_V_GEN)cp $(top_srcdir)/bindings/shmem_bind_c11_extras_none.m4 $@
endif

if HAVE_CXX
noinst_PROGRAMS += generate_bind_cxx_extras
generate_bind_cxx_extras_SOURCES = generate_bind_cxx_extras.cpp
shmem_bind_cxx_extras.m4: generate_bind_cxx_extras
	$(AM_V_GEN)./generate_bind_cxx_extras > $@

else
shmem_bind_cxx_extras.m4:
	$(AM_V_GEN)cp $(top_srcdir)/bindings/shmem_bind_cxx_extras_none.m4 $@
endif

