dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# OMPI_FIND_TYPE(type, [list of c types])
# -----------------------------------------------------------
AC_DEFUN([OMPI_FIND_TYPE],[
    AS_VAR_PUSHDEF([type_var], [ompi_cv_find_type_$1])
    AS_VAR_PUSHDEF([size_var],
       m4_translit([[ompi_cv_fortran_sizeof_$1]], [*], [p]))

    AC_CACHE_CHECK([for C type corresponding to $1], type_var,
        [ # Loop over all the types handed to us
         oft_target_size=AS_VAR_GET(size_var)
         oft_real_type=
         AS_IF([test "$oft_target_size" != ""],
             [m4_foreach(oft_type, [$2], 
                  [if test -z "$oft_real_type"; then
                       if test "[$ac_cv_sizeof_]m4_bpatsubst(oft_type, [[^a-zA-Z0-9_]], [_])" = "$oft_target_size" ; then
                           oft_real_type="oft_type"
                       fi
                   fi
])])
         AS_IF([test -z "$oft_real_type"],
               [AS_VAR_SET(type_var, "void")],
               [AS_VAR_SET(type_var, "$oft_real_type")])])

    AS_VAR_IF(type_var, ["void"],
          [AC_MSG_WARN([*** Did not find corresponding C type])])

    AC_DEFINE_UNQUOTED([fortran_]m4_translit(m4_bpatsubst(m4_bpatsubst([$1], [*], []), [[^a-zA-Z0-9_]], [_]), [A-Z], [a-z])[_t],
                       [AS_VAR_GET(type_var)], 
                       [C type corresponding to Fortran $1])

    unset oft_real_type

    AS_VAR_POPDEF([size_var])dnl
    AS_VAR_POPDEF([type_var])dnl
])dnl
