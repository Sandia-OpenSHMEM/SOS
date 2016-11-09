AC_INIT([shmem_pmi], [1.0], [kayla.seager@intel.com])
AC_CONFIG_AUX_DIR([config])
AC_CONFIG_MACRO_DIR([config])
AC_SUBST(shrext_cmds)
AM_INIT_AUTOMAKE([foreign])
AM_SILENT_RULES([yes])
AC_CONFIG_HEADERS([mpichconf.h])
AC_CHECK_HEADERS([assert.h arpa/inet.h sys/types.h unistd.h stdlib.h \
                  string.h strings.h])
AC_DEFINE([USE_PMI_PORT], [1], [Use port])
AC_PROG_CC
m4_ifdef([AM_PROG_AR],[AM_PROG_AR])
LT_INIT
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
