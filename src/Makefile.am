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

AM_CPPFLAGS = -I$(top_srcdir)/mpp

GEN_BINDINGS = \
	atomic_c.c \
	collectives_c.c \
	data_c.c \
	synchronization_c.c

if HAVE_FORTRAN
GEN_BINDINGS += \
	collectives_f.c \
	data_f.c
endif

lib_LTLIBRARIES = libsma.la
libsma_la_SOURCES = \
	shmem_free_list.h \
	shmem_free_list.c \
	shmem_atomic.h \
	runtime.h \
	shmem_internal.h \
	shmem_internal_op.h \
	shmem_comm.h \
	shmem_collectives.h \
	shmem_synchronization.h \
	shmem_accessibility.h \
	shmem_remote_pointer.h \
	shmem_lock.h \
	malloc.c \
	init.c \
	collectives.c \
	init_c.c \
	query_c.c \
	accessibility_c.c \
	symmetric_heap_c.c \
	remote_pointer_c.c \
	lock_c.c \
	cache_management_c.c \
	transport.h \
	util.c \
	$(GEN_BINDINGS)

BUILT_SOURCES = $(GEN_BINDINGS)

CLEANFILES += $(GEN_BINDINGS)

if HAVE_FORTRAN
libsma_la_SOURCES += \
	init_f.c \
	query_f.c \
	accessibility_f.c \
	symmetric_heap_f.c \
	remote_pointer_f.c \
	atomic_f.c \
	synchronization_f.c \
	lock_f.c \
	cache_management_f.c
endif

if USE_PORTALS4
libsma_la_SOURCES += \
	transport_portals4.h \
	transport_portals4.c
endif

if USE_OFI
libsma_la_SOURCES += \
	transport_ofi.h \
	transport_ofi.c
endif

if USE_XPMEM
libsma_la_SOURCES += \
	transport_xpmem.h \
	transport_xpmem.c
endif

if USE_CMA
libsma_la_SOURCES += \
	transport_cma.h \
	transport_cma.c
endif

if USE_SHMEM_PMI
AM_CPPFLAGS += -I$(top_srcdir)/shmem_pmi

libsma_la_LIBADD = \
	../shmem_pmi/simple_pmi.lo \
	../shmem_pmi/simple_pmiutil.lo
endif

if USE_PMI2
libsma_la_SOURCES += \
	runtime-pmi2.c
else
libsma_la_SOURCES += \
	runtime-pmi.c
endif


bin_SCRIPTS = oshcc oshrun
CLEANFILES += oshcc oshrun
if CASE_SENSITIVE_FS
bin_SCRIPTS += oshCC
CLEANFILES += oshCC
endif
if HAVE_CXX
bin_SCRIPTS += oshc++
CLEANFILES += oshc++
endif
if HAVE_FORTRAN
bin_SCRIPTS += oshfort
CLEANFILES += oshfort
endif

EXTRA_DIST = shmem_compiler_script.in \
             shmem_launcher_script.in

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

oshrun: shmem_launcher_script.in Makefile
	$(AM_V_SED) cp $(srcdir)/shmem_launcher_script.in oshrun
	@chmod +x oshrun

SUFFIXES = .c4

.c4.c: shmem_bind_c.m4 shmem_bind_f.m4
	$(AM_V_GEN) m4 $< > $@
