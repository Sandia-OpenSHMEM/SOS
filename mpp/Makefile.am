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
mppdir = $(includedir)/mpp

mpp_HEADERS = shmem.h shmemx.h
include_HEADERS = shmem.h shmemx.h

BUILT_SOURCES = shmem.h
CLEANFILES += shmem.h

if ENABLE_PROFILING
mpp_HEADERS += pshmem.h
include_HEADERS += pshmem.h

BUILT_SOURCES += pshmem.h
CLEANFILES += pshmem.h
endif

if HAVE_FORTRAN
mpp_HEADERS += shmem.fh shmemx.fh
include_HEADERS += shmem.fh shmemx.fh
endif

SUFFIXES = .h4

.h4.h: shmem_c_func.m4 shmemx_c_func.m4 ../src/shmem_bind_c.m4
	$(AM_V_GEN) $(M4) $< > $@
