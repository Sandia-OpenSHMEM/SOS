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

# shmem.h should appear in BOTH $(includedir)/shmem.h and $(includedir)/mpp/shmem.h!

EXTRA_DIST = shmem.h4 \
             shmem-def.h.in \
             shmemx.h4 \
             pshmem.h4 \
             shmem.fh.in \
             shmemx.fh.in \
             shmem_c_func.h4 \
             shmemx_c_func.h4

mppdir = $(includedir)/mpp

nodist_mpp_HEADERS = shmem.h shmemx.h shmem-def.h
nodist_include_HEADERS = shmem.h shmemx.h shmem-def.h

BUILT_SOURCES = shmem.h shmemx.h shmem-def.h
CLEANFILES += shmem.h shmemx.h shmem-def.h

if ENABLE_PROFILING
nodist_mpp_HEADERS += pshmem.h
nodist_include_HEADERS += pshmem.h

BUILT_SOURCES += pshmem.h
CLEANFILES += pshmem.h
endif

if HAVE_FORTRAN
nodist_mpp_HEADERS += shmem.fh shmemx.fh
nodist_include_HEADERS += shmem.fh shmemx.fh

BUILT_SOURCES += shmem.fh shmemx.fh
CLEANFILES += shmem.fh shmemx.fh
endif

SUFFIXES = .h4

BINDINGS_GEN = \
            $(top_srcdir)/bindings/shmem_bind_c.m4 \
            $(top_srcdir)/bindings/shmem_bind_c11.m4 \
            $(top_srcdir)/bindings/shmem_bind_cxx.m4 \
            $(top_builddir)/bindings/shmem_bind_c11_extras.m4 \
            $(top_builddir)/bindings/shmem_bind_cxx_extras.m4

.h4.h: $(BINDINGS_GEN)
	$(AM_V_GEN) $(M4) -I$(top_srcdir)/bindings -I$(top_builddir)/bindings -I$(top_srcdir)/mpp $< > $@
