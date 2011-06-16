# -*- shell-script -*-
#
# Copyright 2011 Sandia Corporation. Under the terms of Contract
# DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
# retains certain rights in this software.
# 
# This file is part of the Portals SHMEM software package. For license
# information, see the LICENSE file in the top level directory of the
# distribution.
# 
# $HEADER$
#

# SHMEM_FIND_TYPE(type)
# --------------------------------------------------------
AC_DEFUN([SHMEM_FIND_INT_TYPE],[
        AC_CHECK_SIZEOF([$1])
        case "$AS_TR_SH([ac_cv_sizeof_$1])" in
            1)
                type=PTL_INT8_T
                ;;
            2)
                type=PTL_INT16_T
                ;;
            4)
                type=PTL_INT32_T
                ;;
            8)
                type=PTL_INT64_T
                ;;
            *)
                AC_MSG_ERROR([Can not find matching Portals type for $1])
        esac
        AC_DEFINE_UNQUOTED(AS_TR_CPP([DTYPE_$1]), [$type], [Portals datatype corresponding to $1])
])
