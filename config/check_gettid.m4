#CHECK_GETTID([action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if SYS_gettid is available.
AC_DEFUN([CHECK_GETTID], [
    AC_MSG_CHECKING([SYS_gettid syscall definitions])
    AC_LANG_PUSH([C])
    AC_COMPILE_IFELSE([
       AC_LANG_SOURCE([[
#ifndef __APPLE__
#define _GNU_SOURCE
#include <sys/syscall.h>
long tid = SYS_gettid;
#endif
]])],
       [gettid_happy="yes"],
       [gettid_happy="no"])
    AC_LANG_POP([C])
    AC_MSG_RESULT([$gettid_happy])
    AS_IF([test "$gettid_happy" = "yes"], [$1], [$2])
])
