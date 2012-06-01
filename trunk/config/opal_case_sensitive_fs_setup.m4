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
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OPAL_CASE_SENSITIVE_FS_SETUP],[
have_cs_fs=1

AC_MSG_CHECKING([if build filesystem is case sensitive])
cat > conf_fs_test.$$ <<EOF
lowercase
EOF

cat > CONF_FS_TEST.$$ <<EOF
uppercase
EOF

if test "`cat conf_fs_test.$$`" = "lowercase"; then
    have_cs_fs=1
    AC_MSG_RESULT([yes])
else
    have_cs_fs=0
    AC_MSG_RESULT([no])
fi

rm -f conf_fs_test.$$ CONF_FS_TEST.$$

AM_CONDITIONAL(CASE_SENSITIVE_FS, test "$have_cs_fs" = "1")

# Clean up
unset have_cs_fs])dnl
