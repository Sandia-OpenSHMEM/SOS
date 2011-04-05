ACLOCAL_AMFLAGS = -I config

EXTRA_DIST = README

lib_LTLIBRARIES = libsma.la
libsma_la_SOURCES = \
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
	test/hello \
	test/circular_shift \
	test/accessible_ping \
	test/max_reduction \
	test/strided_put \
	test/barrier \
	test/put1 \
	test/get1 \
	test/ping \
	test/pingpong \
	test/shmalloc \
	test/shrealloc \
	test/shmemalign \
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
	test/iput-iget

check_PROGRAMS = \
	$(TESTS) \
	test/ipgm \
	test/shmemlatency \
	test/msgrate \
	test/NPshmem
PROCS ?= 2
TESTS_ENVIRONMENT = yod -c $(PROCS)

test_hello_SOURCES = test/hello.c
test_hello_LDADD = libsma.la

test_circular_shift_SOURCES = test/circular_shift.c
test_circular_shift_LDADD = libsma.la

test_accessible_ping_SOURCES = test/accessible_ping.c
test_accessible_ping_LDADD = libsma.la

test_max_reduction_SOURCES = test/max_reduction.c
test_max_reduction_LDADD = libsma.la

test_strided_put_SOURCES = test/strided_put.c
test_strided_put_LDADD = libsma.la

test_barrier_SOURCES = test/barrier.c
test_barrier_LDADD = libsma.la

test_put1_SOURCES = test/put1.c
test_put1_LDADD = libsma.la

test_get1_SOURCES = test/get1.c
test_get1_LDADD = libsma.la

test_ping_SOURCES = test/ping.c
test_ping_LDADD = libsma.la

test_pingpong_SOURCES = test/pingpong.c
test_pingpong_LDADD = libsma.la

test_iput32_SOURCES = test/iput32.c
test_iput32_LDADD = libsma.la

test_iput64_SOURCES = test/iput64.c
test_iput64_LDADD = libsma.la

test_iput128_SOURCES = test/iput128.c
test_iput128_LDADD = libsma.la

test_iput_short_SOURCES = test/iput_short.c
test_iput_short_LDADD = libsma.la

test_iput_double_SOURCES = test/iput_double.c
test_iput_double_LDADD = libsma.la

test_iput_float_SOURCES = test/iput_float.c
test_iput_float_LDADD = libsma.la

test_iput_long_SOURCES = test/iput_long.c
test_iput_long_LDADD = libsma.la

test_iput_longdouble_SOURCES = test/iput_longdouble.c
test_iput_longdouble_LDADD = libsma.la

test_iput_longlong_SOURCES = test/iput_longlong.c
test_iput_longlong_LDADD = libsma.la

test_sping_SOURCES = test/sping.c
test_sping_LDADD = libsma.la

test_pingpong_short_SOURCES = test/pingpong-short.c
test_pingpong_short_LDADD = libsma.la

test_iput_iget_SOURCES = test/iput-iget.c
test_iput_iget_LDADD = libsma.la

test_ipgm_SOURCES = test/ipgm.c
test_ipgm_LDADD = libsma.la

test_shmalloc_SOURCES = test/shmalloc.c
test_shmalloc_LDADD = libsma.la

test_shrealloc_SOURCES = test/shrealloc.c
test_shrealloc_LDADD = libsma.la

test_shmemalign_SOURCES = test/shmemalign.c
test_shmemalign_LDADD = libsma.la

test_shmemlatency_SOURCES = test/shmemlatency.c
test_shmemlatency_LDADD = libsma.la

test_msgrate_SOURCES = test/msgrate.c
test_msgrate_LDADD = libsma.la

test_NPshmem_SOURCES = test/netpipe/netpipe.c test/netpipe/netpipe.h test/netpipe/shmem.c
test_NPshmem_CPPFLAGS = -DSHMEM
test_NPshmem_LDADD = libsma.la
