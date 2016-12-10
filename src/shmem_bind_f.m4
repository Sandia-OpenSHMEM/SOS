dnl Copyright (c) 2016 Intel Corporation. All rights reserved.
dnl This software is available to you under the BSD license.
dnl
dnl This file is part of the Sandia OpenSHMEM software package. For license
dnl information, see the LICENSE file in the top level directory of the
dnl distribution.
dnl
dnl This file contains m4 macros used to generate the SOS Fortran bindings.
dnl
define(`SHMEM_BIND_F_RMA', dnl args: macro_name
`$1(character, `SIZEOF_FORTRAN_CHARACTER')
$1(complex, `SIZEOF_FORTRAN_COMPLEX')
$1(double,  `SIZEOF_FORTRAN_DOUBLE_PRECISION')
$1(integer, `SIZEOF_FORTRAN_INTEGER')
$1(logical, `SIZEOF_FORTRAN_LOGICAL')
$1(real,    `SIZEOF_FORTRAN_REAL')')dnl
dnl
define(`SHMEM_BIND_F_SIZES', dnl args: macro_name
`$1(mem,     1)
$1(4,       4)
$1(8,       8)
$1(32,      4)
$1(64,      8)
$1(128,     16)')dnl
dnl
define(`SHMEM_BIND_F_COLL_SIZES', dnl args: macro_name
`$1(4,       4)
$1(8,       8)
$1(32,      4)
$1(64,      8)')dnl
dnl
define(`SHMEM_BIND_F_COLL_INTS', dnl args: macro_name, op_name, op_const
`$1(int4,    int32_t,     `SHM_INTERNAL_INT32', 4, `$2', `$3')
$1(int8,    int64_t,     `SHM_INTERNAL_INT64', 8, `$2', `$3')')dnl
dnl
define(`SHMEM_BIND_F_COLL_FLOATS', dnl args: macro_name, op_name, op_const
`#if SIZEOF_LONG_DOUBLE == 16
$1(real4,   float,       `SHM_INTERNAL_FLOAT', 4, `$2', `$3')
$1(real8,   double,      `SHM_INTERNAL_DOUBLE', 8, `$2', `$3')
$1(real16,  long double, `SHM_INTERNAL_LONG_DOUBLE', 16, `$2', `$3')
#else
$1(real4,   float,       `SHM_INTERNAL_FLOAT', 4, `$2', `$3')
$1(real8,   double,      `SHM_INTERNAL_DOUBLE', 8, `$2', `$3')
#endif')dnl
dnl
define(`SH_UPCASE', `translit(`$*', `abcdefghijklmnopqrstuvwxyz', `ABCDEFGHIJKLMNOPQRSTUVWXYZ')')dnl
define(`SH_DOWNCASE', `translit(`$*', `ABCDEFGHIJKLMNOPQRSTUVWXYZ', `abcdefghijklmnopqrstuvwxyz')')dnl
dnl
