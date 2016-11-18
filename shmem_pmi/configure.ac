AC_INIT([shmem_pmi], [1.0], [https://github.com/Sandia-OpenSHMEM/SOS])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config])
AC_SUBST(shrext_cmds)
AM_INIT_AUTOMAKE([foreign])
AM_SILENT_RULES([yes])
AC_CONFIG_HEADERS([mpichconf.h])
AC_CHECK_HEADERS([assert.h arpa/inet.h sys/types.h unistd.h stdlib.h \
                  string.h strings.h])
AC_DEFINE([USE_PMI_PORT], [1], [Use port])

AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
                   #include <stdlib.h>
                   static void nop(void) __attribute__ ((unused));
                   static void nop(void) { exit(1); }
                   ], [])],
                   [sos_pmi_have_attribute="yes"],
                   [sos_pmi_have_attribute="no"])

AS_IF([test "$sos_pmi_have_attribute" = "no"],
      [AC_MSG_ERROR([C compiler does not suppot __attribute__])])

AC_PROG_CC
m4_ifdef([AM_PROG_AR],[AM_PROG_AR])
LT_INIT
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
