ACLOCAL_AMFLAGS = -I config

lib_LTLIBRARIES = libshmem-portals.la
libshmem_portals_la_SOURCES = \
	src/data.c \
	src/runtime.c \
	src/shmem.h \
	src/shmem_internal.h \
	src/symmetric_heap.c \
	src/synchronization.c

mppdir = $(includedir)/mpp
mpp_HEADERS = src/shmem.h
