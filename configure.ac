# -*- autoconf -*-
#
# Copyright 2011 Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
# retains certain rights in this software.
#
# Copyright (c) 2017 Intel Corporation. All rights reserved.
# This software is available to you under the BSD license.
#
# This file is part of the Sandia OpenSHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.

dnl Init Autoconf/Automake/Libtool

AC_INIT([Sandia OpenSHMEM], [1.4.0], [https://github.com/Sandia-OpenSHMEM/SOS/issues], [sos], [https://github.com/Sandia-OpenSHMEM/SOS])
AC_PREREQ([2.60])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config])

AM_INIT_AUTOMAKE([foreign check-news color-tests parallel-tests dist-bzip2 no-define subdir-objects tar-ustar 1.11 -Wall -Werror])

m4_ifndef([AS_VAR_COPY],
    [m4_define([AS_VAR_COPY],
       [AS_LITERAL_IF([$1[]$2], [$1=$$2], [eval $1=\$$2])])])

dnl Work around change in how the rules were implemented between
dnl 1.11.2 and 1.11.3.
AM_SILENT_RULES([yes])
AS_IF([test -z "$AM_V"],
  [AM_V='$(V)'
   AM_DEFAULT_V='$(AM_DEFAULT_VERBOSITY)'
   AC_SUBST([AM_V])
   AC_SUBST([AM_DEFAULT_V])])

dnl information on the package
AC_CONFIG_HEADERS([src/config.h])

AC_CANONICAL_HOST

OPAL_CHECK_ATTRIBUTES
AS_IF([test "$opal_cv___attribute__deprecated" = 1],
      [SHMEM_AC_HAVE_ATTRIBUTE_DEPRECATED=1],
      [SHMEM_AC_HAVE_ATTRIBUTE_DEPRECATED=0])
AC_SUBST(SHMEM_AC_HAVE_ATTRIBUTE_DEPRECATED)

OPAL_CHECK_PMIX

AC_ARG_ENABLE([picky],
    [AC_HELP_STRING([--enable-picky],
                    [Enable developer-level compiler pickyness when building (default: disabled)])])

AC_ARG_ENABLE([debug],
    [AC_HELP_STRING([--enable-debug],
                    [Include debugging symbols in the build (default: disabled)])])

AC_ARG_ENABLE([error-checking],
    [AC_HELP_STRING([--enable-error-checking],
                    [Enable error checking for SHMEM calls (default:disabled)])])
AS_IF([test "$enable_error_checking" = "yes"], [AC_DEFINE([ENABLE_ERROR_CHECKING], [1], [Enable argument checking])])
AM_CONDITIONAL([ENABLE_ERROR_CHECKING], [test "$enable_error_checking" = "yes"])

AC_ARG_ENABLE([hard-polling],
    [AC_HELP_STRING([--enable-hard-polling],
                    [Enable hard polling of wait calls (default:automatic)])])
AS_IF([test "$enable_hard_polling" = "yes"], [AC_DEFINE([ENABLE_HARD_POLLING], [1], [Enable hard polling])])

AC_ARG_ENABLE([completion-polling],
    [AC_HELP_STRING([--enable-completion-polling],
                    [Enable polling in quiet, fence, and local completion operations (default:disabled)])])
AS_IF([test "$enable_completion_polling" = "yes"],
      [AC_DEFINE([ENABLE_COMPLETION_POLLING], [1], [Enable polling for local/remote completions])
       AC_DEFINE([DEFAULT_POLL_LIMIT], [-1], [Poll limit for local/remote completions])],
      [AC_DEFINE([DEFAULT_POLL_LIMIT], [0], [Poll limit for local/remote completions])])

AC_ARG_ENABLE([nonblocking-fence],
    [AC_HELP_STRING([--enable-nonblocking-fence],
                    [When total data ordering is not available, make shmem_fence a non-blocking operation.  shmem_fence will return immediately, but the next communication call will block until all previous communications finish. (default:disabled)])])
AS_IF([test "$enable_nb_fence" = "yes"], [AC_DEFINE([ENABLE_NONBLOCKING_FENCE], [1], [Enable non-blocking fence])])


AC_ARG_WITH([total-data-ordering],
    [AC_HELP_STRING([--with-total-data-ordering],
       [Configure handling of total data ordering option.  "no" or "never" to build with the assumption total data ordering will never be available.  "yes" or "always" to build with the assumption total data ordering will always be available (if not, applications will abort in start_pes()).  "check" to build with no assumptions, which may lead to a slight performance decrease on high performance networks.  (default: disabled)])])
AS_CASE([x$with_total_data_ordering],
  [xyes|xalways], [ordering=1; msg="assume always available"],
  [xno|xnever|x], [ordering=0; msg="assume never available"],
  [xcheck], [ordering=2; msg="runtime check"],
  [AC_MSG_ERROR([Unknown ordering requirement $with_total_data_ordering])])
AC_DEFINE_UNQUOTED([WANT_TOTAL_DATA_ORDERING], [$ordering], [0 if never check for total data ordering, 1 if always assume total data ordering, 2 to check])
AC_MSG_CHECKING([For total data ordering behavior])
AC_MSG_RESULT([$msg])

AC_ARG_ENABLE([threads],
    [AC_HELP_STRING([--disable-threads],
                    [Disable threading support (default:enabled)])])

AS_IF([test "$enable_threads" != "no"], [
       OPAL_CONFIG_POSIX_THREADS(HAVE_POSIX_THREADS=1, HAVE_POSIX_THREADS=0)
       AC_MSG_CHECKING([for working POSIX threads package])
       if test "$HAVE_POSIX_THREADS" = "1" ; then
           AC_MSG_RESULT([yes])
           AC_SUBST(PTHREAD_CFLAGS)
           AC_SUBST(PTHREAD_LIBS)
       else
           AC_MSG_RESULT([no])
       fi
       AC_DEFINE([ENABLE_THREADS], [1], [Enable threads])
       AC_CHECK_HEADERS([stdatomic.h])
       CHECK_GETTID(
           [AC_DEFINE([HAVE_SYS_GETTID], [1], [Have support for SYS_gettid syscall])],
           [AC_MSG_WARN([No support for SYS_gettid syscall])])
       AS_IF([test "$enable_thread_completion" = "yes"],
             [AC_DEFINE([USE_CTX_LOCK], [1], [Enable per-context locks])
             AC_DEFINE([USE_THREAD_COMPLETION], [1], [Support SHMEM_THREAD_MULTIPLE using FI_THREAD_COMPLETION instead of FI_THREAD_SAFE])])
       ])
AM_CONDITIONAL([HAVE_PTHREADS], [test "$HAVE_POSIX_THREADS" = "1"])

AS_IF([test "$enable_threads" != "no"], [
       AS_IF([test "$enable_openmp" != "no"], [
             AC_OPENMP
             AC_SUBST(AM_OPENMP_CXXFLAGS,"$OPENMP_CXXFLAGS")
             AC_SUBST(AM_OPENMP_CFLAGS,"$OPENMP_CFLAGS")])
       ])
AM_CONDITIONAL([HAVE_OPENMP], [test "$enable_threads" != "no" -a "$enable_openmp" != "no" -a "$ac_cv_prog_c_openmp" != "unsupported"])

AC_ARG_ENABLE([pthread-mutexes],
    [AC_HELP_STRING([--enable-pthread-mutexes],
                    [Use pthread mutexes instead of spinlocks (used in conjunction with --enable-threads) (default:disabled)])])
AS_IF([test "$enable_pthread_mutexes" = "yes"], [AC_DEFINE([ENABLE_PTHREAD_MUTEX], [1], [Enable pthread mutexes])])

AC_ARG_ENABLE([thread-completion],
    [AC_HELP_STRING([--enable-thread-completion],
        [Support SHMEM_THREAD_MULTIPLE using FI_THREAD_COMPLETION thread safety model instead of FI_THREAD_SAFE (default: disabled)])])

AC_ARG_ENABLE([remote-virtual-addressing],
    [AC_HELP_STRING([--enable-remote-virtual-addressing],
                    [Enable optimizations assuming the symmetric heap and data segments are located at identical addresses at all PEs.  This may cause erroneous behavior if this assumption is violated, but will reduce the instruction count for some operations.  On Linux systems, this optimization requires disabling ASLR.  This optimization also requires that the Portals 4 implementation support BIND_INACCESSIBLE on LEs. (default: disabled)])])
AS_IF([test "$enable_remote_virtual_addressing" = "yes"],
      [AC_DEFINE([ENABLE_REMOTE_VIRTUAL_ADDRESSING], [1], [If defined, the implementation will use one LE/PT to cover all of the symmetric data and heap, setup so that no offset transformation is needed on the target virtual address.])])

AC_ARG_ENABLE([aslr-check],
    [AC_HELP_STRING([--disable-aslr-check],
                    [Disable check for address space layout randomization (ASLR).  This can be useful when ASLR is enabled, but position-independent executable generation is disabled by compiler or linker flags. (default: enabled)])])
AS_IF([test "$enable_aslr_check" = "no"],
      [AC_DEFINE(DISABLE_ASLR_CHECK_AC, [1], [Disable ASLR check])])


AC_ARG_ENABLE([lengthy-tests],
    [AC_HELP_STRING([--enable-lengthy-tests],
                    [Enable long running tests in the test suite (default: disabled)])])
AM_CONDITIONAL([ENABLE_LENGTHY_TESTS], [test "$enable_lengthy_tests" = "yes"])

AC_ARG_ENABLE([fortran],
    [AC_HELP_STRING([--disable-fortran],
                    [Disable building the Fortran bindings (default: enabled)])])

AC_ARG_ENABLE([cxx],
    [AC_HELP_STRING([--disable-cxx],
                    [Disable building the C++ bindings (default: enabled)])])

AC_ARG_ENABLE([libtool-wrapper],
  [AS_HELP_STRING([--disable-libtool-wrapper],
    [Disable use of libtool executable wrappers for tests (default: enabled)])])
AS_IF([test "$enable_libtool_wrapper" = "no"], [LIBTOOL_WRAPPER_LDFLAGS="-no-install"], [LIBTOOL_WRAPPER_LDFLAGS=])
AC_SUBST([LIBTOOL_WRAPPER_LDFLAGS])

AC_ARG_ENABLE([rpath],
  [AS_HELP_STRING([--disable-rpath],
    [Disable use of rpath in compiler wrappers and building tests (default: enabled)])])
AS_IF([test "$enable_rpath" = "no"], [], [enable_wrapper_rpath="yes"])


AC_ARG_ENABLE([profiling],
    [AC_HELP_STRING([--enable-profiling],
                    [Enable profiling (pshmem) interface (default:disabled)])])
AS_IF([test "$enable_profiling" = "yes"],
      [OPAL_C_WEAK_SYMBOLS()
       AS_IF([test "$OPAL_C_HAVE_WEAK_SYMBOLS" = 1],
             [AC_DEFINE([ENABLE_PROFILING], [1], [Enable profiling (pshmem) interface])],
             [AC_MSG_ERROR("Profiling interface requires weak symbols support")])
       ])

AC_ARG_ENABLE([long-fortran-header],
    [AC_HELP_STRING([--enable-long-fortran-header],
                    [Enable long Fortran header, including all function declarations (default:disabled)])])
AS_IF([test "$enable_long_fortran_header" = "yes"], [FORTRAN_LONG_HEADER=" "], [FORTRAN_LONG_HEADER="!"])
AC_SUBST([FORTRAN_LONG_HEADER])
AM_CONDITIONAL([HAVE_LONG_FORTRAN_HEADER], [test "$enable_long_fortran_header" = "yes"])

AC_ARG_ENABLE([mr-scalable],
    [AC_HELP_STRING([--disable-mr-scalable],
                    [Disable the use of scalable memory regions in the OFI transport.  Required by some providers. (default: enabled)])])
AS_IF([test "$enable_mr_scalable" != "no"],
      [AC_DEFINE([ENABLE_MR_SCALABLE], [1], [If defined, the OFI transport will use FI_MR_SCALABLE.  Otherwise, FI_MR_BASIC is used.])])

AC_ARG_ENABLE([mr-rma-event],
    [AC_HELP_STRING([--enable-mr-rma-event],
                    [Enable FI_MR_RMA_EVENT memory registration support in OFI transport.  Required by some providers. (default: disabled)])])
AS_IF([test "$enable_mr_rma_event" = "yes"],
      [AC_DEFINE([ENABLE_MR_RMA_EVENT], [1], [If defined, the OFI transport will enable support for FI_MR_RMA_EVENT.])])

AC_ARG_ENABLE([av-map],
    [AC_HELP_STRING([--enable-av-map],
                    [enable av-map instead of av-table in the OFI transport. (default: disabled)])])
AS_IF([test "$enable_av_map" = "yes"],
      [AC_DEFINE([USE_AV_MAP], [1], [If defined, the OFI transport will use FI_AV_MAP.  Otherwise, FI_AV_TABLE is used.])])

AC_ARG_ENABLE([bounce-buffers],
    [AC_HELP_STRING([--disable-bounce-buffers],
                    [Disable bounce buffers by default (default: enabled)])])
AS_IF([test "$enable_bounce_buffers" = "no"],
      [AC_DEFINE([DEFAULT_BOUNCE_SIZE], [0], [Default bounce buffer threshold])],
      [AC_DEFINE([DEFAULT_BOUNCE_SIZE], [2048], [Default bounce buffer threshold])])

AC_ARG_WITH([oshrun-launcher],
    [AC_HELP_STRING([--with-oshrun-launcher],
                    [Set launcher to be used by oshrun launcher wrapper. (default: auto)])])

AC_ARG_ENABLE([memcpy],
    [AC_HELP_STRING([--enable-memcpy],
                    [Use memcpy to perform local put/get operations (default:disabled)])])

PKG_INSTALLDIR()

dnl check for programs

AC_PROG_CC
AM_PROG_CC_C_O
AC_C_INLINE
AC_PROG_CXX dnl required even with --disable-cxx due to automake conditionals
m4_ifdef([AM_PROG_AR],[AM_PROG_AR])
AC_PATH_PROG([PERL],[perl])
AM_CONDITIONAL([EXTERNAL_TESTS], [false])
AM_CONDITIONAL([SHMEMX_TESTS], [true])

dnl C++ bindings require std::is_same, which is part of C++11
if test "$enable_cxx" != "no" ; then
    AX_CXX_COMPILE_STDCXX([11])
fi

# start gnu11/gnu99 check
ORIG_CFLAGS=$CFLAGS
WRAPPER_COMPILER_CC=$CC
AC_SUBST([WRAPPER_COMPILER_CC])
AC_MSG_CHECKING([if -std=gnu11 works])

AC_LANG_PUSH([C])
CFLAGS="-pedantic-errors -std=gnu11"
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
                   void g(int i){}
                   #define f(X) _Generic((X), default: g)(X)
                   ]], [[
                        f(1);
                        ]]
                   )],
                   [shmem_cv_c11_works="yes"], [shmem_cv_c11_works="no"])
AS_IF([test "$shmem_cv_c11_works" = "no"],
      [AC_MSG_RESULT([no])
      CFLAGS="$ORIG_CFLAGS"
      AC_PROG_CC_C99
      ],
      [AC_MSG_RESULT([yes])
      CFLAGS="-std=gnu11 $ORIG_CFLAGS"]
      )
AC_LANG_POP([C])
AM_CONDITIONAL([HAVE_C11], [test "$shmem_cv_c11_works" = "yes"])
# end gnu11/gnu99 check


AC_PATH_PROG([M4], [m4], [false])
AS_IF([test "$M4" = "false"],
      [AC_MSG_ERROR([Could not find 'm4' macro processor])])

if test "$enable_fortran" != "no" ; then
  AC_PROG_FC
else
  FC=
fi
if test "$FC" != "" ; then
  AC_FC_WRAPPERS
fi

AX_GCC_BUILTIN([__builtin_trap])

if test "$enable_picky" = "yes" -a "$GCC" = "yes" ; then
  CFLAGS="$CFLAGS -Wall -Wno-long-long -Wmissing-prototypes -Wstrict-prototypes -Wcomment -pedantic"
else
  CFLAGS="$CFLAGS -Wall"
fi

if test "$enable_debug" = "yes" ; then
  CFLAGS="$CFLAGS -g"
fi

AC_CACHE_SAVE

AC_SEARCH_LIBS([clock_gettime], [rt],
               [AC_DEFINE([HAVE_CLOCK_GETTIME], [1], [define if clock_gettime is available]) ])

dnl check for libraries

OMPI_CHECK_PORTALS4([portals4],
    [transport_portals4="yes"],
    [transport_portals4="no"])

OPAL_CHECK_OFI([ofi],
    [transport_ofi="yes"],
    [transport_ofi="no"])

# If both OFI and portals4 requested, user needs to choose one:
if test -n "$with_ofi" -a "$with_ofi" != "no" -a -n "$with_portals4" -a "$with_portals4" != "no" ; then
    AC_MSG_ERROR([Cannot choose both OFI and portals4 transports, see --help for details])
# Check which was requested, OFI or portals4:
elif test -n "$with_ofi" -a "$with_ofi" != "no" ; then
    transport="ofi"
    transport_portals4="no"
    AC_DEFINE([USE_OFI], [1], [Define if OFI transport active])
elif test -n "$with_portals4" -a "$with_portals4" != "no" ; then
    transport="portals4"
    transport_ofi="no"
    AC_DEFINE([USE_PORTALS4], [1], [Define if Portals 4 transport active])
# If neither, set transport to "none" and print a warning:
else
    transport="none"
    transport_ofi="no"
    transport_portals4="no"
    AC_MSG_WARN([Neither OFI nor portals4 transport requested])
fi
AM_CONDITIONAL([USE_OFI], [test "$transport_ofi" = "yes"])
AM_CONDITIONAL([USE_PORTALS4], [test "$transport_portals4" = "yes"])

SANDIA_CHECK_XPMEM(
    [transport_xpmem="yes"],
    [transport_xpmem="no"])

CHECK_CMA(
    [transport_cma="yes"],
    [transport_cma="no"])

# If both XPMEM and CMA requested, user needs to choose one:
if test -n "$with_xpmem" -a "$with_xpmem" != "no" -a -n "$with_cma" -a "$with_cma" != "no" ; then
    AC_MSG_ERROR([Cannot choose both XPMEM and CMA transports, see --help for details])
# Check which was requested, XPMEM or CMA:
elif test -n "$with_xpmem" -a "$with_xpmem" != "no" ; then
    transport_cma="no"
    AC_DEFINE([USE_XPMEM], [1], [Define if XPMEM transport is active])
elif test -n "$with_cma" -a "$with_cma" != "no" ; then
    transport_xpmem="no"
    AC_DEFINE([USE_CMA], [1], [Define if Cross Memory Attach transport is active])
    AC_DEFINE([_GNU_SOURCE], [1], [CMA transport header requires global definition of _GNU_SOURCE])
# If neither, disable XPMEM and CMA:
else
    transport_xpmem="no"
    transport_cma="no"
    AC_MSG_RESULT([Neither XPMEM nor CMA transport requested])

fi

if test "$enable_memcpy" = "yes" -a "$transport_xpmem" = "no" -a "$transport_cma" = "no" ; then
    transport_memcpy="yes"
    AC_DEFINE([USE_MEMCPY], [1], [Define to use memcpy for local put/get communication])
elif test "$transport_xpmem" = "yes" -o "$transport_cma" = "yes" ; then
    transport_memcpy="yes"
else
    transport_memcpy="no"
fi

AM_CONDITIONAL([USE_XPMEM], [test "$transport_xpmem" = "yes"])
AM_CONDITIONAL([USE_CMA], [test "$transport_cma" = "yes"])

AS_IF([test "$transport_xpmem" = "yes" -o "$transport_cma" = "yes"],
      [AC_DEFINE([USE_ON_NODE_COMMS], [1], [Define if any on-node comm transport is available])
       AC_DEFINE([ENABLE_HARD_POLLING], [1], [Enable hard polling])
      ])

AC_ARG_ENABLE([pmi-simple], [AC_HELP_STRING([--enable-pmi-simple],
			[build and run shmem-pmi support])])
AS_IF([test "$enable_pmi_simple" = "yes"],
     [TEST_RUNNER='$(abs_top_builddir)/src/oshrun -n $(NPROCS)'],
     [OPAL_CHECK_PMI(pmi_type)])
AS_IF([test "$opal_external_pmix_version_found" = 1],
     [pmi_CPPFLAGS="$opal_external_pmix_CPPFLAGS"
      pmi_LDFLAGS="$opal_external_pmix_LDFLAGS"
      pmi_LIBS="$opal_external_pmix_LIBS"])
AM_CONDITIONAL([USE_PMI_SIMPLE], [test "$enable_pmi_simple" = "yes"])
AM_CONDITIONAL([USE_PMI2], [test "$pmi_type" = "pmi2"])
AM_CONDITIONAL([USE_PMIX], [test "$opal_external_pmix_version_found" = 1])

dnl check for header files
AC_CHECK_HEADERS([fnmatch.h])
AS_IF([test "$enable_pmi_simple" = "yes"],
      [AC_CHECK_HEADERS([assert.h arpa/inet.h sys/types.h unistd.h stdlib.h string.h strings.h])
      AC_DEFINE([USE_PMI_PORT], [1], [Use port])])

dnl check for types

AC_LANG_PUSH([C])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[ ]], [[ _Complex v; ]] )],
                  [],
                  [AC_ERROR([C Compiler does not support _Complex])])
AC_LANG_POP([C])

SHMEM_FIND_INT_TYPE([char], [$transport])
SHMEM_FIND_INT_TYPE([short], [$transport])
SHMEM_FIND_INT_TYPE([int], [$transport])
SHMEM_FIND_INT_TYPE([long], [$transport])
SHMEM_FIND_INT_TYPE([long long], [$transport])
SHMEM_FIND_INT_TYPE([ptrdiff_t], [$transport])

SHMEM_FIND_UINT_TYPE([unsigned int], [$transport])
SHMEM_FIND_UINT_TYPE([unsigned long], [$transport])
SHMEM_FIND_UINT_TYPE([unsigned long long], [$transport])
SHMEM_FIND_UINT_TYPE([size_t], [$transport])

AC_CHECK_SIZEOF([float])
AC_CHECK_SIZEOF([double])
AC_CHECK_SIZEOF([long double])
AC_CHECK_SIZEOF([void*])

dnl Determine psync and pwrk sizes for collectives

C_LOG_MAXPES=32
AC_MSG_CHECKING([size of SHMEM_BCAST_SYNC_SIZE])
    C_BCAST_SYNC_SIZE=1
    AC_SUBST(C_BCAST_SYNC_SIZE)
AC_MSG_RESULT([$C_BCAST_SYNC_SIZE])
AC_MSG_CHECKING([size of SHMEM_REDUCE_SYNC_SIZE])
    C_REDUCE_SYNC_SIZE=`$PERL -e "print $C_BCAST_SYNC_SIZE + 2 + $C_LOG_MAXPES"`
    AC_SUBST(C_REDUCE_SYNC_SIZE)
AC_MSG_RESULT([$C_REDUCE_SYNC_SIZE])
AC_MSG_CHECKING([size of SHMEM_BARRIER_SYNC_SIZE])
    C_BARRIER_SYNC_SIZE=`$PERL -e "print $ac_cv_sizeof_int * 8 * ($ac_cv_sizeof_int / $ac_cv_sizeof_long)"`
    AC_SUBST(C_BARRIER_SYNC_SIZE)
AC_MSG_RESULT([$C_BARRIER_SYNC_SIZE])
AC_MSG_CHECKING([size of SHMEM_COLLECT_SYNC_SIZE])
    linear_sync_size=`$PERL -e "print 2 + $C_BARRIER_SYNC_SIZE"`
    tree_sync_size=`$PERL -e "print $C_BCAST_SYNC_SIZE + 3"`
    recdbl_sync_size=`$PERL -e "print $ac_cv_sizeof_int * 8 * ($ac_cv_sizeof_int / $ac_cv_sizeof_long)"`
    C_COLLECT_SYNC_SIZE=`$PERL -e "use List::Util qw(max); print max($linear_sync_size, $tree_sync_size, $recdbl_sync_size)"`
    AC_SUBST(C_COLLECT_SYNC_SIZE)
AC_MSG_RESULT([$C_COLLECT_SYNC_SIZE])
AC_MSG_CHECKING([size of SHMEM_ALLTOALL_SYNC_SIZE])
    C_ALLTOALL_SYNC_SIZE=$C_BARRIER_SYNC_SIZE
    AC_SUBST(C_ALLTOALL_SYNC_SIZE)
AC_MSG_RESULT([$C_ALLTOALL_SYNC_SIZE])
AC_MSG_CHECKING([size of SHMEM_ALLTOALLS_SYNC_SIZE])
    C_ALLTOALLS_SYNC_SIZE=$C_BARRIER_SYNC_SIZE
    AC_SUBST(C_ALLTOALLS_SYNC_SIZE)
AC_MSG_RESULT([$C_ALLTOALLS_SYNC_SIZE])
AC_MSG_CHECKING([size of SHMEM_SYNC_SIZE])
    C_SYNC_SIZE=`$PERL -e "use List::Util qw (max); print max((       \
                                              $C_BCAST_SYNC_SIZE,     \
                                              $C_REDUCE_SYNC_SIZE,    \
                                              $C_BARRIER_SYNC_SIZE,   \
                                              $C_COLLECT_SYNC_SIZE,   \
                                              $C_ALLTOALL_SYNC_SIZE,  \
                                              $C_ALLTOALLS_SYNC_SIZE));"`
    AC_SUBST(C_SYNC_SIZE)
AC_MSG_RESULT([$C_SYNC_SIZE])
AC_MSG_CHECKING([size of SHMEM_REDUCE_MIN_WRKDATA_SIZE])
    C_REDUCE_MIN_WRKDATA_SIZE=1
    AC_SUBST(C_REDUCE_MIN_WRKDATA_SIZE)
AC_MSG_RESULT([$C_REDUCE_MIN_WRKDATA_SIZE])

AC_MSG_CHECKING([if long can hold lock_t])
AC_TRY_RUN([struct lock_s { int a; int b; }; int main(void) { if (sizeof(long) == sizeof(struct lock_s)) return 0; else return 1; }],
               [AC_MSG_RESULT([yes])], [
                AC_MSG_RESULT([no])
                AC_MSG_ERROR([Lock API requires long can hold 'struct { int a; int b; }']) ])

if test "$FC" != "" ; then
  AC_DEFINE([ENABLE_FORTRAN],[1],[Fortran support enabled])
  AC_MSG_CHECKING([if complex sizes are appropriate])
  AC_TRY_RUN([float _Complex x; double _Complex y; int main(void) { if (sizeof(x)==8 && sizeof(y)==16) return 0; else return 1; }],
                 [AC_MSG_RESULT([yes])], [
                  AC_MSG_RESULT([no])
                  AC_MSG_ERROR([float _Complex and double _Complex must be 4 and 8 bytes, respectively]) ])
  # make sure Fortran supports ISO_C_BINDING (Fortran 2003)
  AC_CACHE_CHECK([if Fortran compiler supports ISO_C_BINDING],
    [shmem_cv_fortran_iso_c_binding],
    [AC_LANG_PUSH([Fortran])
     AC_COMPILE_IFELSE([AC_LANG_PROGRAM(, [[
        use, intrinsic :: ISO_C_BINDING
	integer(c_int) :: i
	i = 5
]])], [shmem_cv_fortran_iso_c_binding="yes"],
      [shmem_cv_fortran_iso_c_binding="no"])
      AC_LANG_POP([Fortran])])
  AS_IF([test "$shmem_cv_fortran_iso_c_binding" = "no"],
    [AC_MSG_ERROR([Fortran compiler does not support ISO_C_BINDING, which is required for Sandia OpenSHMEM.])])

  # Get size of Fortran types
  OMPI_FORTRAN_GET_SIZEOF([], [LOGICAL])
  OMPI_FORTRAN_GET_SIZEOF([], [CHARACTER])
  OMPI_FORTRAN_GET_SIZEOF([], [INTEGER])
  OMPI_FORTRAN_GET_SIZEOF([], [REAL])
  OMPI_FORTRAN_GET_SIZEOF([], [DOUBLE PRECISION])
  OMPI_FORTRAN_GET_SIZEOF([], [COMPLEX])
  OMPI_FORTRAN_GET_SIZEOF([], [DOUBLE COMPLEX])

  # find transport integer type corresponding to INTEGER
  SHMEM_FIND_FORTRAN_INT_TYPE([INTEGER], [$transport])

  # find C type corresponding to given Fortran types
  OMPI_FIND_TYPE([INTEGER], [char,     short, int, long, long long])
  OMPI_FIND_TYPE([DOUBLE PRECISION], [float, double, long double])

  # Find kind values for 4 & 8 byte integer types in Fortran (used in
  # shmem.fh)
  OMPI_FORTRAN_GET_KIND_VALUE([C_INT32_T], [FORTRAN_INT4_KIND])
  OMPI_FORTRAN_GET_KIND_VALUE([C_INT64_T], [FORTRAN_INT8_KIND])
  OMPI_FORTRAN_GET_KIND_VALUE([C_FLOAT], [FORTRAN_REAL4_KIND])
  OMPI_FORTRAN_GET_KIND_VALUE([C_DOUBLE], [FORTRAN_REAL8_KIND])
  OMPI_FORTRAN_GET_KIND_VALUE([C_FLOAT_COMPLEX], [FORTRAN_COMP4_KIND])
  OMPI_FORTRAN_GET_KIND_VALUE([C_DOUBLE_COMPLEX], [FORTRAN_COMP8_KIND])

  # The collectives are all implemented in C and require the psync to
  # hold a variable number of C longs.  Find how many INTEGERs it takes to
  # create a memory region as big as the required C region.
  FORTRAN_BCAST_SYNC_SIZE=`$PERL -e "use POSIX; print ceil($ac_cv_sizeof_long / $ompi_cv_fortran_sizeof_INTEGER * $C_BCAST_SYNC_SIZE)"`
  FORTRAN_REDUCE_SYNC_SIZE=`$PERL -e "use POSIX; print ceil($ac_cv_sizeof_long / $ompi_cv_fortran_sizeof_INTEGER * $C_REDUCE_SYNC_SIZE)"`
  FORTRAN_BARRIER_SYNC_SIZE=`$PERL -e "use POSIX; print ceil($ac_cv_sizeof_long / $ompi_cv_fortran_sizeof_INTEGER * $C_BARRIER_SYNC_SIZE)"`
  FORTRAN_COLLECT_SYNC_SIZE=`$PERL -e "use POSIX; print ceil($ac_cv_sizeof_long / $ompi_cv_fortran_sizeof_INTEGER * $C_COLLECT_SYNC_SIZE)"`
  FORTRAN_ALLTOALL_SYNC_SIZE=`$PERL -e "use POSIX; print ceil($ac_cv_sizeof_long / $ompi_cv_fortran_sizeof_INTEGER * $C_ALLTOALL_SYNC_SIZE)"`
  FORTRAN_ALLTOALLS_SYNC_SIZE=`$PERL -e "use POSIX; print ceil($ac_cv_sizeof_long / $ompi_cv_fortran_sizeof_INTEGER * $C_ALLTOALLS_SYNC_SIZE)"`
  FORTRAN_REDUCE_MIN_WRKDATA_SIZE=`$PERL -e "use POSIX; print ceil($ac_cv_sizeof_long / $ompi_cv_fortran_sizeof_INTEGER * $C_REDUCE_MIN_WRKDATA_SIZE)"`
  FORTRAN_SYNC_SIZE=`$PERL -e "use List::Util qw (max); print max((             \
                                                  $FORTRAN_BCAST_SYNC_SIZE,     \
                                                  $FORTRAN_REDUCE_SYNC_SIZE,    \
                                                  $FORTRAN_BARRIER_SYNC_SIZE,   \
                                                  $FORTRAN_COLLECT_SYNC_SIZE,   \
                                                  $FORTRAN_ALLTOALL_SYNC_SIZE,  \
                                                  $FORTRAN_ALLTOALLS_SYNC_SIZE));"`
else
  FORTRAN_BCAST_SYNC_SIZE=0
  FORTRAN_REDUCE_SYNC_SIZE=0
  FORTRAN_BARRIER_SYNC_SIZE=0
  FORTRAN_COLLECT_SYNC_SIZE=0
  FORTRAN_ALLTOALL_SYNC_SIZE=0
  FORTRAN_ALLTOALLS_SYNC_SIZE=0
  FORTRAN_SYNC_SIZE=0
  FORTRAN_REDUCE_MIN_WRKDATA_SIZE=0
  FORTRAN_INT4_KIND=0
  FORTRAN_INT8_KIND=0
  FORTRAN_REAL4_KIND=0
  FORTRAN_REAL8_KIND=0
  FORTRAN_COMP4_KIND=0
  FORTRAN_COMP8_KIND=0
  AC_DEFINE([DTYPE_FORTRAN_INTEGER],[-1],[Fortran integer C type (disabled)])
fi
if test "$ac_cv_sizeof_voidp" = "4" ; then
  FORTRAN_PTR_KIND=$FORTRAN_INT4_KIND
else
  FORTRAN_PTR_KIND=$FORTRAN_INT8_KIND
fi
AC_SUBST(FORTRAN_BCAST_SYNC_SIZE)
AC_SUBST(FORTRAN_REDUCE_SYNC_SIZE)
AC_SUBST(FORTRAN_BARRIER_SYNC_SIZE)
AC_SUBST(FORTRAN_COLLECT_SYNC_SIZE)
AC_SUBST(FORTRAN_ALLTOALL_SYNC_SIZE)
AC_SUBST(FORTRAN_ALLTOALLS_SYNC_SIZE)
AC_SUBST(FORTRAN_SYNC_SIZE)
AC_SUBST(FORTRAN_REDUCE_MIN_WRKDATA_SIZE)
AC_SUBST(FORTRAN_INT4_KIND)
AC_SUBST(FORTRAN_INT8_KIND)
AC_SUBST(FORTRAN_REAL4_KIND)
AC_SUBST(FORTRAN_REAL8_KIND)
AC_SUBST(FORTRAN_COMP4_KIND)
AC_SUBST(FORTRAN_COMP8_KIND)
AC_SUBST(FORTRAN_PTR_KIND)

AM_CONDITIONAL([HAVE_FORTRAN], [test "$FC" != ""])

AM_CONDITIONAL([HAVE_CXX], [test "$enable_cxx" != "no" ])

AM_CONDITIONAL([ENABLE_PROFILING], [test "$enable_profiling" = "yes" ])

dnl check for structures

dnl check for compiler characteristics

# make all symbols hidden by default
OPAL_CHECK_VISIBILITY
AS_IF([test "$opal_visibility_define" = 1],
      [SHMEM_AC_HAVE_ATTRIBUTE_VISIBILITY=1],
      [SHMEM_AC_HAVE_ATTRIBUTE_VISIBILITY=0])
AC_SUBST(SHMEM_AC_HAVE_ATTRIBUTE_VISIBILITY)
CFLAGS="$CFLAGS $OPAL_VISIBILITY_CFLAGS"

# Disable ASLR at link time by instructing the linker not to generate
# position-independent executable code
aslr_LDFLAGS=
case $host_os in
    darwin*)
           aslr_LDFLAGS="-Wl,-no_pie"
           ;;
esac
AC_SUBST(aslr_LDFLAGS)

dnl check for library functions

AC_MSG_CHECKING([for data segment pointer])
AC_TRY_RUN([
#ifdef __APPLE__
#include <mach-o/getsect.h>
#else
#pragma weak __data_start
#pragma weak _end
extern int __data_start;
extern int _end;
#endif

/* Ensure data segment is not empty.  Anything simpler gets optimized
 * away by the compiler. */
int ensure_nonempty_data(void) {
    static int ncall = 0;
    return ncall++;
}

int main(void) {
    void *base;
    long length;
#ifdef __APPLE__
    base = (void*) get_etext();
    length = get_end() - get_etext();
#else
    if (&__data_start == (int*) 0 || &_end == (int*) 0) return 1;
    base = (void*) &__data_start;
    length = (long) ((char*) &_end - (char*) &__data_start);
#endif
    if (!(base != (void*)0 && length > 0)) return 2;
    return 0;
}],
    AC_MSG_RESULT([found]),
    [ AC_MSG_RESULT([not found])
      AC_MSG_ERROR([Could not locate data segment])
    ])

dnl final output

LT_INIT

OPAL_CASE_SENSITIVE_FS_SETUP

DISTCHECK_CONFIGURE_FLAGS=
if test -n "$with_portals4" -a "$with_portals4" != "no" -a "$with_portals4" != "yes" ; then
  DISTCHECK_CONFIGURE_FLAGS="$DISTCHECK_CONFIGURE_FLAGS --with-portals4=${with_portals4}"
fi
if test -n "$with_ofi" -a "$with_ofi" != "no" -a "$with_ofi" != "yes" ; then
  DISTCHECK_CONFIGURE_FLAGS="$DISTCHECK_CONFIGURE_FLAGS --with-ofi=${with_ofi}"
fi
if test -n "$with_xpmem" -a "$with_xpmem" != "no" -a "$with_xpmem" != "yes" ; then
  DISTCHECK_CONFIGURE_FLAGS="$DISTCHECK_CONFIGURE_FLAGS --with-xpmem=${with_xpmem}"
fi
if test -n "$with_pmi" -a "$with_pmi" != "no" -a "$with_pmi" != "yes" ; then
  DISTCHECK_CONFIGURE_FLAGS="$DISTCHECK_CONFIGURE_FLAGS --with-pmi=${with_pmi}"
fi
if test -n "$FCFLAGS" ; then
  DISTCHECK_CONFIGURE_FLAGS="$DISTCHECK_CONFIGURE_FLAGS FCFLAGS=\"$FCFLAGS\""
fi
AC_SUBST(DISTCHECK_CONFIGURE_FLAGS)

dnl Cause Automake to delete emacs turds
CLEANFILES="*~"
AC_SUBST(CLEANFILES)

# configuration for --with-oshrun-launcher
WRAPPER_SEARCH_LAUNCHERS="yod mpirun mpiexec srun"
if test -n "$with_oshrun_launcher" ; then
    WRAPPER_SEARCH_LAUNCHERS="$with_oshrun_launcher"
fi
AC_SUBST(WRAPPER_SEARCH_LAUNCHERS)

# last minute updates of the flags so that tests don't fail oddly if
# portals libdir isn't in LD_LIBRARY_PATH...
if test "$transport_portals4" = "yes" ; then
    CPPFLAGS="$CPPFLAGS $portals4_CPPFLAGS"
    LDFLAGS="$LDFLAGS $portals4_LDFLAGS"
    LIBS="$LIBS $portals4_LIBS"
    WRAPPER_COMPILER_EXTRA_LDFLAGS="$portals4_LDFLAGS"
    WRAPPER_COMPILER_EXTRA_LIBS="$portals4_LIBS"
fi

if test "$transport_ofi" = "yes" ; then
    CPPFLAGS="$CPPFLAGS $ofi_CPPFLAGS"
    LDFLAGS="$LDFLAGS $ofi_LDFLAGS"
    LIBS="$LIBS $ofi_LIBS"
    WRAPPER_COMPILER_EXTRA_LDFLAGS="$ofi_LDFLAGS"
    WRAPPER_COMPILER_EXTRA_LIBS="$ofi_LIBS"
fi

if test "$transport_xpmem" = "yes" ; then
    CPPFLAGS="$CPPFLAGS $XPMEM_CPPFLAGS"
    LDFLAGS="$LDFLAGS $XPMEM_LDFLAGS"
    LIBS="$LIBS $XPMEM_LIBS"
    WRAPPER_COMPILER_EXTRA_LDFLAGS="$XPMEM_LDFLAGS"
    WRAPPER_COMPILER_EXTRA_LIBS="$XPMEM_LIBS"
fi

CPPFLAGS="$CPPFLAGS $pmi_CPPFLAGS"
LDFLAGS="$LDFLAGS $pmi_LDFLAGS $aslr_LDFLAGS"
LIBS="$LIBS $pmi_LIBS"
WRAPPER_COMPILER_EXTRA_LDFLAGS="$WRAPPER_COMPILER_EXTRA_LDFLAGS $pmi_LDFLAGS $aslr_LDFLAGS"
WRAPPER_COMPILER_EXTRA_LIBS="$WRAPPER_COMPILER_LIBS $pmi_LIBS"

# Need the libtool binary before setting up rpath
LT_OUTPUT

OPAL_TOP_BUILDDIR="`pwd`"
OPAL_SETUP_RPATH

AS_IF([test "$enable_pmi_simple" = "yes"],
     [WRAPPER_COMPILER_EXTRA_LIBS="$WRAPPER_COMPILER_EXTRA_LIBS -lpmi_simple"])

AC_MSG_CHECKING([for OPAL LDFLAGS])
RPATHIFY_LDFLAGS([WRAPPER_COMPILER_EXTRA_LDFLAGS])
WRAPPER_COMPILER_EXTRA_LDFLAGS=`echo "$WRAPPER_COMPILER_EXTRA_LDFLAGS" | sed -e 's/@{libdir}/\${libdir}/g'`
AC_MSG_RESULT([$WRAPPER_COMPILER_EXTRA_LDFLAGS])
AC_SUBST(WRAPPER_COMPILER_EXTRA_LDFLAGS)
AC_SUBST(WRAPPER_COMPILER_EXTRA_LIBS)

AC_CONFIG_FILES([Makefile
  sandia-openshmem.spec
  man/Makefile
  bindings/Makefile
  pmi-simple/Makefile
  mpp/Makefile
  mpp/shmem.fh
  mpp/shmemx.fh
  mpp/shmem.h4
  mpp/shmem_c_func.h4
  mpp/shmemx_c_func.h4
  mpp/shmemx.h4
  src/Makefile
  src/sos.pc
  test/Makefile
  test/unit/Makefile
  test/performance/Makefile
  test/performance/shmem_perf_suite/Makefile
  test/performance/tests/Makefile
  test/apps/Makefile])

if test "$enable_profiling" = "yes"; then
    AC_CONFIG_FILES([mpp/pshmem.h4])
fi

AC_OUTPUT

AS_IF([test "$enable_pmi_simple" != "yes" -a "$opal_external_pmix_version_found" != 1 -a "$ompi_check_pmi_happy" != "yes" -a -z "$pmi_type"],
         [AC_MSG_WARN([No PMI found, if build fails consider --enable-pmi-simple or --with-pmi])])

AS_IF([test "$transport_portals4" != "yes" -a "$transport_xpmem" != "yes" -a "$transport_cma" != "yes" -a "$transport_ofi" != "yes"],
      [AC_MSG_WARN([No transport found])])

AS_IF([test "$shmem_cv_c11_works" != "yes"],
      [AC_MSG_WARN([C compiler does not support _Generic, unabled to verify and test C11 bindings])])

AS_IF([test "$enable_cxx" == "no"],
      [AC_MSG_WARN([No C++ compiler or C++ disabled, unabled to verify and test C++ bindings])])

AS_IF([test "$enable_fortran" == "no"],
      [AC_MSG_WARN([No Fortran compiler or Fortran disabled, unabled to test Fortran bindings])])

AS_IF([test "$enable_fortran" != "no"],
      [AC_MSG_WARN([Fortran bindings have been deprecated as of OpenSHMEM v1.4])])

#Note: The --with-pmix flag is not currently marked as experimental in
#      --help, because the AC_HELP string is set in upstream OMPI.
AS_IF([test "$opal_external_pmix_happy" = "yes"],
      [AC_MSG_WARN([PMIx is an experimental feature in Sandia OpenSHMEM])])

FORT="$FC"
if test "$FORT" = "" ; then
  FORT="none"
fi

if test "$enable_cxx" == "no"; then
  CXX="none"
fi

echo ""
echo "Compilers:"
echo "  C:              $CC"
echo "  C++:            $CXX"
echo "  Fortran:        $FORT"
echo ""
echo "Transports:"
echo "  Portals 4:      $transport_portals4"
echo "  OFI:            $transport_ofi"
echo "  XPMEM:          $transport_xpmem"
echo "  CMA:            $transport_cma"
echo "  memcpy:         $transport_memcpy"
echo ""
