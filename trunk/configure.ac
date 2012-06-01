# -*- autoconf -*-
#
# Copyright 2011 Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
# retains certain rights in this software.
# 
# This file is part of the Portals SHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.
#

dnl Init Autoconf/Automake/Libtool

AC_INIT([Portals SHMEM], [1.0a6], [bwbarre@sandia.gov])
AC_PREREQ([2.59])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config])

AM_INIT_AUTOMAKE([foreign check-news color-tests dist-bzip2 no-define subdir-objects tar-ustar 1.9.6 -Wall -Werror])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

dnl information on the package
AC_CONFIG_HEADERS([src/config.h])

AC_ARG_ENABLE([picky],
    [AC_HELP_STRING([--enable-picky],
                    [Enable developer-level compiler pickyness when building (default: disabled)])])
if test -z "$enable_picky" && test -d .svn -o -d .hg ; then
    echo "--> developer override: enable picky compiler by default"
    enable_picky=yes
fi

AC_ARG_ENABLE([error-checking],
    [AC_HELP_STRING([--enable-error-checking],
                    [Enable error checking for SHMEM calls (default:disabled)])])
AS_IF([test "$enable_error_checking" = "yes"], [AC_DEFINE([ENABLE_ERROR_CHECKING], [1], [Enable argument checking])])

AC_ARG_ENABLE([hard-polling],
    [AC_HELP_STRING([--enable-hard-polling],
                    [Enable hard polling of wait calls (default:disabled)])])
AS_IF([test "$enable_hard_polling" = "yes"], [AC_DEFINE([ENABLE_HARD_POLLING], [1], [Enable hard polling])])

AC_ARG_WITH([total-data-ordering],
    [AC_HELP_STRING([--with-total-data-ordering],
       [Configure handling of total data ordering option.  "no" or "never" to build with the assumption total data ordering will never be available.  "yes" or "always" to build with the assumption total data ordering will always be available (if not, applications will abort in start_pes()).  "check" to build with no assumptions, which may lead to a very slight performance decrease on high performance networks.  Default: check.])])
AS_CASE([x$with_total_data_ordering],
  [xyes|xalways], [ordering=1; msg="assume always available"],
  [xno|xnever], [ordering=0; msg="assume never available"],
  [xcheck|x], [ordering=2; msg="runtime check"],
  [AC_MSG_ERROR([Unknown ordering requirement $with_total_data_ordering])])
AC_DEFINE_UNQUOTED([WANT_TOTAL_DATA_ORDERING_CHECK], [$ordering], [0 if never check for total data ordering, 1 if always assume total data ordering, 2 to check])
AC_MSG_CHECKING([For total data ordering behavior])
AC_MSG_RESULT([$msg])

dnl check for programs

AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CC_C99
AC_C_INLINE
AM_PROG_AR
AC_PATH_PROG([PERL],[perl])
AC_PROG_CXX
AC_PROG_FC

if test "$enable_picky" = "yes" -a "$GCC" = "yes" ; then
  CFLAGS="$CFLAGS -Wall -Wno-long-long -Wmissing-prototypes -Wstrict-prototypes -Wcomment -pedantic"
fi
if test "$enable_picky" = "yes" -a "$GXX" = "yes" ; then
  CXXFLAGS="$CXXFLAGS -Wall -Wno-long-long"
fi

AC_CACHE_SAVE

dnl check for libraries

OMPI_CHECK_PORTALS4([portals4],
    [AC_DEFINE([USE_PORTALS4], [1], [Define if Portals 4 transport active])
     transport_portals4="yes"],
    [transport_portals4="no"])
AM_CONDITIONAL([USE_PORTALS4], [test "$transport_portals4" = "yes"])

SANDIA_CHECK_XPMEM(
    [AC_DEFINE([USE_XPMEM], [1], [Define if XPMEM transport is active])
     transport_xpmem="yes"],
    [transport_xpmem="no"])
AM_CONDITIONAL([USE_XPMEM], [test "$transport_xpmem" = "yes"])
AS_IF([test "$transport_xpmem" = "yes"],
      [AC_DEFINE([USE_ON_NODE_COMMS], [1], [Define if any on-node comm transport is available])])

ORTE_CHECK_PMI([pmi])

dnl check for header files

dnl check for types
SHMEM_FIND_INT_TYPE(short)
SHMEM_FIND_INT_TYPE(int)
SHMEM_FIND_INT_TYPE(long)
SHMEM_FIND_INT_TYPE(long long)

dnl check for structures

dnl check for compiler characteristics

dnl check for library functions

dnl final output

LT_INIT

OPAL_CASE_SENSITIVE_FS_SETUP

if test -n "$with_portals4" ; then
  DISTCHECK_CONFIGURE_FLAGS="--with-portals4=${with_portals4}"
fi
if test -n "$with_xpmem" ; then
  DISTCHECK_CONFIGURE_FLAGS="--with-xpmem=${with_xpmem}"
fi
if test -n "$with_pmi" ; then
  DISTCHECK_CONFIGURE_FLAGS="--with-pmi=${with_pmi}"
fi
AC_SUBST(DISTCHECK_CONFIGURE_FLAGS)

dnl Cause Automake to delete emacs turds
CLEANFILES="*~"
AC_SUBST(CLEANFILES)

# last minute updates of the flags so that tests don't fail oddly if
# portals libdir isn't in LD_LIBRARY_PATH...
CPPFLAGS="$CPPFLAGS $portals4_CPPFLAGS $XPMEM_CPPFLAGS $pmi_CPPFLAGS"
LDFLAGS="$LDFLAGS $portals4_LDFLAGS $XPMEM_LDFLAGS $pmi_LDFLAGS"
LIBS="$LIBS $portals4_LIBS $XPMEM_LIBS $pmi_LIBS"
CPPFLAGS="$CPPFLAGS"' -I$(top_srcdir)'

# wrapper compiler gorp
WRAPPER_COMPILER_EXTRA_LDFLAGS="$portlas4_LDFLAGS $XPMEM_LDFLAGS $pmi_LDFLAGS"
WRAPPER_COMPILER_EXTRA_LIBS="$portals4_LIBS $XPMEM_LIBS $pmi_LIBS"
AC_SUBST(WRAPPER_COMPILER_EXTRA_LDFLAGS)
AC_SUBST(WRAPPER_COMPILER_EXTRA_LIBS)

AC_CONFIG_FILES([Makefile mpp/Makefile src/Makefile test/Makefile])
AC_OUTPUT

echo ""
echo "Transports:"
echo "  Portals 4:      $transport_portals4"
echo "  XPMEM:          $transport_xpmem"
echo ""
