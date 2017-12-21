#CHECK_CMA([action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if cma support is wanted.
AC_DEFUN([CHECK_CMA], [
    AC_ARG_WITH([cma],
       [AS_HELP_STRING([--with-cma],
         [Use Cross Memory Attach syscalls for on-node comms, invalid with XPMEM (default: no)])])

    if test "$with_cma" = "yes" ; then
        AC_CHECK_FUNC([process_vm_writev],
            [AC_DEFINE([HAVE_LIBC_CMA], [1], ["LibC has process_vm_writev"])
             cma_happy="yes" ],
            [AC_MSG_CHECKING([CMA syscall definitions])
             AC_LANG_PUSH([C])
             AC_COMPILE_IFELSE([
                AC_LANG_SOURCE([[#include <sys/syscall.h>]],
                               [[long cma=__NR_process_vm_readv;]])],
                [cma_happy="yes"],
                [cma_happy="no"])
             AC_LANG_POP([C])
             AC_MSG_RESULT([$cma_happy])])
    fi
    AS_IF([test "$cma_happy" = "yes"], [$1], [$2])
])
