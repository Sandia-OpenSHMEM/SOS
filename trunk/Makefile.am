ACLOCAL_AMFLAGS = -I config

lib_LTLIBRARIES = libshmem_portals.la
libshmem_portals_la_SOURCES = \
	src/atomic.c \
	src/collectives.c \
	src/data.c \
	src/memory.c \
	src/runtime.c \
	src/shmem.h \
	src/shmem_internal.h \
	src/symmetric_heap.c \
	src/synchronization.c

mppdir = $(includedir)/mpp
mpp_HEADERS = mpp/shmem.h

TESTS = \
	test/put1

check_PROGRAMS = $(TESTS)
PROCS ?= 2
TESTS_ENVIRONMENT = yod -c $(PROCS)

test_put1_SOURCES = test/put1.c
test_put1_LDADD = libshmem_portals.la
