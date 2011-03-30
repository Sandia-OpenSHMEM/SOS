# -*- autoconf -*-
#
# Copyright (c) 2011 Sandia National Laboratories. All rights reserved.
#

dnl Init Autoconf/Automake/Libtool

AC_INIT([shmem-portals], [0.1a], [bwbarre@sandia.gov])
AC_PREREQ([2.59])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config])

AM_INIT_AUTOMAKE([1.9.6 foreign dist-bzip2 subdir-objects no-define tar-ustar -Wall -Werror])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

LT_INIT

dnl information on the package
AC_CONFIG_HEADERS([src/config.h])

AC_ARG_ENABLE(picky, 
    AC_HELP_STRING([--enable-picky],
                   [enable developer-level compiler pickyness when building (default: disabled)]))
if test -z "$enable_picky" && test -d .svn -o -d .hg ; then
    echo "--> developer override: enable picky compiler by default"
    enable_picky=yes
fi

dnl check for programs

AC_PROG_CC
AM_PROG_CC_C_O
AC_PROG_CC_C99
AC_C_INLINE

if test "$enable_picky" = "yes" -a "$GCC" = "yes" ; then
  CFLAGS="$CFLAGS -Wall -Wno-long-long -Wmissing-prototypes -Wstrict-prototypes -Wcomment -pedantic"
fi
if test "$enable_picky" = "yes" -a "$GXX" = "yes" ; then
  CXXFLAGS="$CXXFLAGS -Wall -Wno-long-long"
fi

AC_CACHE_SAVE

dnl check for libraries

OMPI_CHECK_PORTALS4([portals4], [], [AC_ERROR([Could not find Portals 4 library])])
CPPFLAGS="$CPPFLAGS $portals4_CPPFLAGS"
LDFLAGS="$LDFLAGS $portals4_LDFLAGS"
LIBS="$LIBS $portals4_LIBS"

dnl check for header files

dnl check for types

dnl check for structures

dnl check for compiler characteristics

dnl check for library functions

dnl final output

if test -n "$with_portals4" ; then
  DISTCHECK_CONFIGURE_FLAGS="--with-portals4=${with_portals4}"
fi
AC_SUBST(DISTCHECK_CONFIGURE_FLAGS)

dnl Cause Automake to delete emacs turds
CLEANFILES="*~"
AC_SUBST(CLEANFILES)

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
