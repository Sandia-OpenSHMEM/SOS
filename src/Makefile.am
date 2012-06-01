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

AM_CPPFLAGS = -I$(top_srcdir)/mpp

lib_LTLIBRARIES = libsma.la
libsma_la_SOURCES = \
	shmem_free_list.h \
	shmem_free_list.c \
	shmem_atomic.h \
	runtime.h \
	runtime-pmi.c \
	shmem_internal.h \
	shmem_comm.h \
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

# add oshfort to bin_SCRIPTS and CLEANFILES once we have a fortran interface...
bin_SCRIPTS = oshcc oshc++
CLEANFILES += oshcc oshc++
if CASE_SENSITIVE_FS
bin_SCRIPTS += oshCC
CLEANFILES += oshCC
endif
EXTRA_DIST = shmem_compiler_script.in

do_subst = sed -e 's,[@]PERL[@],$(PERL),g' \
               -e 's,[@]CC[@],$(CC),g' \
               -e 's,[@]CXX[@],$(CXX),g' \
               -e 's,[@]FC[@],$(FC),g' \
               -e 's,[@]WRAPPER_COMPILER_INCLUDEDIR[@],$(includedir),g' \
               -e 's,[@]WRAPPER_COMPILER_LIBDIR[@],$(libdir),g' \
               -e 's,[@]WRAPPER_COMPILER_EXTRA_LDFLAGS[@],$(WRAPPER_COMPILER_EXTRA_LDFLAGS),g' \
               -e 's,[@]WRAPPER_COMPILER_EXTRA_LIBS[@],$(WRAPPER_COMPILER_EXTRA_LIBS),g'

AM_V_SED = $(am__v_SED_@AM_V@)
am__v_SED_ = $(am__v_SED_@AM_DEFAULT_V@)
am__v_SED_0 = @echo "  SED     " $@;

oshcc: shmem_compiler_script.in Makefile
	$(AM_V_SED)$(do_subst) -e 's,[@]LANG[@],C,g' < $(srcdir)/shmem_compiler_script.in > oshcc
	@chmod +x oshcc

oshc++: shmem_compiler_script.in Makefile
	$(AM_V_SED)$(do_subst) -e 's,[@]LANG[@],C++,g' < $(srcdir)/shmem_compiler_script.in > oshc++
	@chmod +x oshc++

oshCC: shmem_compiler_script.in Makefile
	$(AM_V_SED)@$(do_subst) -e 's,[@]LANG[@],C++,g' < $(srcdir)/shmem_compiler_script.in > oshCC
	@chmod +x oshCC

oshfort: shmem_compiler_script.in Makefile
	$(AM_V_SED)@$(do_subst) -e 's,[@]LANG[@],C++,g' < $(srcdir)/shmem_compiler_script.in > oshfort
	@chmod +x oshfort

