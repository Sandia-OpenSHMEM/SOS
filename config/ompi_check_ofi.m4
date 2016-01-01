# -*- shell-script -*-
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2006 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2006      QLogic Corp. All rights reserved.
# Copyright (c) 2009      Cisco Systems, Inc.  All rights reserved.
# Copyright (c) 2015      Intel Corp.  All rights reserved.
#
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# OMPI_CHECK_OFI(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if OFI support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found
AC_DEFUN([OMPI_CHECK_OFI],[
    AC_ARG_WITH([ofi],
        [AC_HELP_STRING([--with-ofi(=DIR)],
             [Build OFI support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OMPI_CHECK_WITHDIR([ofi], [$with_ofi], [include/rdma/fabric.h])
    AC_ARG_WITH([ofi-libdir],
        [AC_HELP_STRING([--with-ofi-libdir=DIR],
             [Search for OFI libraries in DIR])])
    OMPI_CHECK_WITHDIR([ofi-libdir], [$with_ofi_libdir], [libfabric.*])

    ompi_check_ofi_$1_save_CPPFLAGS="$CPPFLAGS"
    ompi_check_ofi_$1_save_LDFLAGS="$LDFLAGS"
    ompi_check_ofi_$1_save_LIBS="$LIBS"

    AS_IF([test "$with_ofi" != "no"],
          [AS_IF([test ! -z "$with_ofi" -a "$with_ofi" != "yes"],
                 [ompi_check_ofi_dir="$with_ofi"])
           AS_IF([test ! -z "$with_ofi_libdir" -a "$with_ofi_libdir" != "yes"],
                 [ompi_check_ofi_libdir="$with_ofi_libdir"])

           OMPI_CHECK_PACKAGE([$1],
                              [rdma/fabric.h],
                              [fabric],
                              [fi_getinfo],
			      [],
                              [$ompi_check_ofi_dir],
                              [$ompi_check_ofi_libdir],
                              [ompi_check_ofi_happy="yes"],
                              [ompi_check_ofi_happy="no"])],
          [ompi_check_ofi_happy="no"])

    LDFLAGS="$LDFLAGS $ofi_LDFLAGS"
    LIBS="$LIBS $ofi_LIBS"

    CPPFLAGS="$ompi_check_ofi_$1_save_CPPFLAGS"
    LDFLAGS="$ompi_check_ofi_$1_save_LDFLAGS"
    LIBS="$ompi_check_ofi_$1_save_LIBS"

    AS_IF([test "$ompi_check_ofi_happy" = "yes"],
          [AS_IF([test "$enable_remote_virtual_addressing" != "yes" -o "$enable_mr_scalable" == "no"],
                 [AC_MSG_WARN([OFI is configured without remote virtual addressing and/or scalable memory registration.  Performance may be affected.])])])

    AS_IF([test "$ompi_check_ofi_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_ofi" -a "$with_ofi" != "no"],
                 [AC_MSG_ERROR([OFI support requested but not found.  Aborting])])
           $3])
])

