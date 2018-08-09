#CHECK_SCHED_GETAFFINITY([action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if sched_getaffinity is available.
AC_DEFUN([CHECK_SCHED_GETAFFINITY], [
    AC_MSG_CHECKING([sched_getaffinity definition])
    AC_LANG_PUSH([C])
    AC_COMPILE_IFELSE([
       AC_LANG_SOURCE([[
#define _GNU_SOURCE
#include <sched.h>
cpu_set_t my_set;
int main() {CPU_ZERO(&my_set); sched_getaffinity(0, sizeof(my_set), &my_set);}
]])],
       [sched_getaffinity_happy="yes"],
       [sched_getaffinity_happy="no"])
    AC_LANG_POP([C])
    AC_MSG_RESULT([$sched_getaffinity_happy])
    AS_IF([test "$sched_getaffinity_happy" = "yes"], [$1], [$2])
])
