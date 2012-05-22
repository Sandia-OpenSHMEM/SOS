# -*- Autoconf -*-
#
# Copyright (c)      2011  Sandia Corporation
#

# SANDIA_CHECK_XPMEM([action-if-found], [action-if-not-found])
# ------------------------------------------------------------------------------
AC_DEFUN([SANDIA_CHECK_XPMEM], [
  AC_ARG_WITH([xpmem],
    [AS_HELP_STRING([--with-xpmem=[path]],
       [Location of XPMEM library])])

  AS_IF([test "$with_xpmem" = "no"], [happy=no], [happy=yes])

  XPMEM_CPPFLAGS=
  XPMEM_LDFLAGS=
  XPMEM_LIBS="-lxpmem"

  saved_CPPFLAGS="$CPPFLAGS"
  saved_LDFLAGS="$LDFLAGS"
  saved_LIBS="$LIBS"
  AS_IF([test ! -z "$with_xpmem" -a "$with_xpmem" != "yes"],
    [CPPFLAGS="$CPPFLAGS -I$with_xpmem/include"
     LDFLAGS="$LDFLAGS -L$with_xpmem/lib"
     XPMEM_CPPFLAGS="-I$with_xpmem/include"
     XPMEM_LDFLAGS="-L$with_xpmem/lib"])

  AS_IF([test "$happy" = "yes"], 
    [AC_CHECK_HEADERS([xpmem.h], [], [happy=no])])
  AS_IF([test "$happy" = "yes"],
    [AC_CHECK_LIB([xpmem], [xpmem_init], [], [happy=no])])

  CPPFLAGS="$saved_CPPFLAGS"
  LDFLAGS="$saved_LDFLAGS"
  LIBS="$saved_LIBS"

  AC_SUBST(XPMEM_CPPFLAGS)
  AC_SUBST(XPMEM_LDFLAGS)
  AC_SUBST(XPMEM_LIBS) 

  AS_IF([test "$happy" = "yes"], [$1], [$2])
])
