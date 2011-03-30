ACLOCAL_AMFLAGS = -I config

EXTRA_DIST = README

lib_LTLIBRARIES = libshmem_portals.la
libshmem_portals_la_SOURCES = \
	mpp/shmem.h \
	src/atomic.c \
	src/collectives.c \
	src/data.c \
	src/malloc.c \
	src/memory.c \
	src/runtime.c \
	src/shmem_internal.h \
	src/symmetric_heap.c \
	src/synchronization.c

mppdir = $(includedir)/mpp
mpp_HEADERS = mpp/shmem.h

TESTS = \
	test/barrier \
	test/put1 \
	test/get1 \
	test/ping \
	test/pingpong \
	test/iput32 \
	test/iput64 \
	test/iput128 \
	test/iput_short \
	test/iput_double \
	test/iput_float \
	test/iput_long \
	test/iput_longdouble \
	test/iput_longlong \
	test/sping \
	test/pingpong-short \
	test/iput-iget \
	test/shmemlatency

check_PROGRAMS = $(TESTS)
PROCS ?= 2
TESTS_ENVIRONMENT = yod -c $(PROCS)

test_put1_SOURCES = test/put1.c
test_put1_LDADD = libshmem_portals.la

test_get1_SOURCES = test/get1.c
test_get1_LDADD = libshmem_portals.la

test_barrier_SOURCES = test/barrier.c
test_barrier_LDADD = libshmem_portals.la

test_ping_SOURCES = test/ping.c
test_ping_LDADD = libshmem_portals.la

test_pingpong_SOURCES = test/pingpong.c
test_pingpong_LDADD = libshmem_portals.la

test_iput32_SOURCES = test/iput32.c
test_iput32_LDADD = libshmem_portals.la

test_iput64_SOURCES = test/iput64.c
test_iput64_LDADD = libshmem_portals.la

test_iput128_SOURCES = test/iput128.c
test_iput128_LDADD = libshmem_portals.la

test_iput_short_SOURCES = test/iput_short.c
test_iput_short_LDADD = libshmem_portals.la

test_iput_double_SOURCES = test/iput_double.c
test_iput_double_LDADD = libshmem_portals.la

test_iput_float_SOURCES = test/iput_float.c
test_iput_float_LDADD = libshmem_portals.la

test_iput_long_SOURCES = test/iput_long.c
test_iput_long_LDADD = libshmem_portals.la

test_iput_longdouble_SOURCES = test/iput_longdouble.c
test_iput_longdouble_LDADD = libshmem_portals.la

test_iput_longlong_SOURCES = test/iput_longlong.c
test_iput_longlong_LDADD = libshmem_portals.la

test_sping_SOURCES = test/sping.c
test_sping_LDADD = libshmem_portals.la

test_pingpong_short_SOURCES = test/pingpong-short.c
test_pingpong_short_LDADD = libshmem_portals.la

test_iput_iget_SOURCES = test/iput-iget.c
test_iput_iget_LDADD = libshmem_portals.la

test_shmemlatency_SOURCES = test/shmemlatency.c
test_shmemlatency_LDADD = libshmem_portals.la

