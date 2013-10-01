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
	shmem_collectives.h \
	shmem_synchronization.h \
	shmem_accessibility.h \
	shmem_remote_pointer.h \
	shmem_lock.h \
	malloc.c \
	init.c \
	init_c.c \
	query_c.c \
	accessibility_c.c \
	symmetric_heap_c.c \
	remote_pointer_c.c \
	data_c.c \
	atomic_c.c \
	synchronization_c.c \
	collectives_c.c \
	lock_c.c \
	cache_management_c.c

if HAVE_FORTRAN
libsma_la_SOURCES += \
	init_f.c \
	query_f.c \
	accessibility_f.c \
	symmetric_heap_f.c \
	remote_pointer_f.c \
	data_f.c \
	atomic_f.c \
	synchronization_f.c \
	collectives_f.c \
	lock_f.c \
	cache_management_f.c
endif

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

bin_SCRIPTS = oshcc oshc++
CLEANFILES += oshcc oshc++
if CASE_SENSITIVE_FS
bin_SCRIPTS += oshCC
CLEANFILES += oshCC
endif
if HAVE_FORTRAN
bin_SCRIPTS += oshfort
CLEANFILES += oshfort
endif

EXTRA_DIST = shmem_compiler_script.in

do_subst = sed -e 's|[@]PERL[@]|$(PERL)|g' \
               -e 's|[@]CC[@]|$(CC)|g' \
               -e 's|[@]CXX[@]|$(CXX)|g' \
               -e 's|[@]FC[@]|$(FC)|g' \
               -e 's|[@]WRAPPER_COMPILER_INCLUDEDIR[@]|$(includedir)|g' \
               -e 's|[@]WRAPPER_COMPILER_LIBDIR[@]|$(libdir)|g' \
               -e 's|[@]WRAPPER_COMPILER_EXTRA_LDFLAGS[@]|$(WRAPPER_COMPILER_EXTRA_LDFLAGS)|g' \
               -e 's|[@]WRAPPER_COMPILER_EXTRA_LIBS[@]|$(WRAPPER_COMPILER_EXTRA_LIBS)|g'

AM_V_SED = $(am__v_SED_@AM_V@)
am__v_SED_ = $(am__v_SED_@AM_DEFAULT_V@)
am__v_SED_0 = @echo "  SED     " $@;

oshcc: shmem_compiler_script.in Makefile
	$(AM_V_SED)$(do_subst) -e 's|[@]LANG[@]|C|g' < $(srcdir)/shmem_compiler_script.in > oshcc
	@chmod +x oshcc

oshc++: shmem_compiler_script.in Makefile
	$(AM_V_SED)$(do_subst) -e 's|[@]LANG[@]|C++|g' < $(srcdir)/shmem_compiler_script.in > oshc++
	@chmod +x oshc++

oshCC: shmem_compiler_script.in Makefile
	$(AM_V_SED)$(do_subst) -e 's|[@]LANG[@]|C++|g' < $(srcdir)/shmem_compiler_script.in > oshCC
	@chmod +x oshCC

oshfort: shmem_compiler_script.in Makefile
	$(AM_V_SED)$(do_subst) -e 's|[@]LANG[@]|Fortran|g' < $(srcdir)/shmem_compiler_script.in > oshfort
	@chmod +x oshfort

