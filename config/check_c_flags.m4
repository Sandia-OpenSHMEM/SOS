dnl @synopsis C_FLAGS_CHECK [compiler flags], [var name]
dnl @summary check whether compiler supports given C flags
AC_DEFUN([C_FLAGS_CHECK],
[dnl
  AC_MSG_CHECKING([if $CC supports $1])
  AC_LANG_PUSH([C])
  ac_saved_cflags="$CFLAGS"
  CFLAGS="-Werror $1"
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([])],
    [AC_MSG_RESULT([yes])
     ax_cv_c_flags_$2=yes],
    [AC_MSG_RESULT([no])
     ax_cv_c_flags_$2=no])
  CFLAGS="$ac_saved_cflags"
  AC_LANG_POP([C])
])
