dnl Copyright 2011 Sandia Corporation. Under the terms of Contract
dnl DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
dnl retains certain rights in this software.
dnl
dnl This file is part of the Sandia OpenSHMEM software package. For license
dnl information, see the LICENSE file in the top level directory of the
dnl distribution.
dnl
dnl This file contains the m4 macros used to generate the SOS C bindings.
dnl
define(`SHMEM_BIND_C_RMA',
`$1(float,      float)`$2'
$1(double,     double)`$2'
$1(longdouble, long double)`$2'
$1(char,       char)`$2'
$1(short,      short)`$2'
$1(int,        int)`$2'
$1(long,       long)`$2'
$1(longlong,   long long)')dnl
dnl
define(`SHMEM_BIND_C_SIZES',
`$1(8,          1)`$2'
$1(16,         2)`$2'
$1(32,         4)`$2'
$1(64,         8)`$2'
$1(128,        16)')dnl
dnl
define(`SHMEM_BIND_C_AMO',
`$1(int,        int,        `SHM_INTERNAL_INT')`$2'
$1(long,       long,       `SHM_INTERNAL_LONG')`$2'
$1(longlong,   long long,  `SHM_INTERNAL_LONG_LONG')')dnl
dnl
define(`SHMEM_BIND_C_EXTENDED_AMO',
`SHMEM_BIND_C_AMO(`$1',`$2')`$2'
$1(float,      float,      `SHM_INTERNAL_FLOAT')`$2'
$1(double,     double,     `SHM_INTERNAL_DOUBLE')')dnl
dnl
define(`SHMEM_BIND_C_WAIT',
`$1(short,      short)
$1(int,        int)
$1(long,       long)
$1(longlong,   long long)')dnl
dnl
define(`SHMEM_BIND_C_COLL_INTS',
`$1(short,    short,         `SHM_INTERNAL_SHORT',         `$2', `$3')
$1(int,      int,           `SHM_INTERNAL_INT',           `$2', `$3')
$1(long,     long,          `SHM_INTERNAL_LONG',          `$2', `$3')
$1(longlong, long long,     `SHM_INTERNAL_LONG_LONG',     `$2', `$3')')dnl
dnl
define(`SHMEM_BIND_C_COLL_FLOATS',
`$1(float,    float,         `SHM_INTERNAL_FLOAT',         `$2', `$3')
$1(double,   double,        `SHM_INTERNAL_DOUBLE',        `$2', `$3')
$1(longdouble, long double, `SHM_INTERNAL_LONG_DOUBLE',   `$2', `$3')')dnl
dnl
define(`SHMEM_BIND_C_COLL_CMPLX',
`$1(complexf, float complex, `SHM_INTERNAL_FLOAT_COMPLEX', `$2', `$3')
$1(complexd, double complex,`SHM_INTERNAL_DOUBLE_COMPLEX',`$2', `$3')')dnl
dnl
define(`SHMEM_DECLARE_FOR_RMA', `SHMEM_BIND_C_RMA(`$1',`;');')dnl
define(`SHMEM_DECLARE_FOR_AMO', `SHMEM_BIND_C_AMO(`$1',`;');')dnl
define(`SHMEM_DECLARE_FOR_SIZES', `SHMEM_BIND_C_SIZES(`$1',`;');')dnl
define(`SHMEM_DECLARE_FOR_EXTENDED_AMO', `SHMEM_BIND_C_EXTENDED_AMO(`$1',`;');')dnl
dnl
define(`SHMEM_DEFINE_FOR_RMA', `SHMEM_BIND_C_RMA(`$1')')dnl
define(`SHMEM_DEFINE_FOR_AMO', `SHMEM_BIND_C_AMO(`$1')')dnl
define(`SHMEM_DEFINE_FOR_SIZES', `SHMEM_BIND_C_SIZES(`$1')')dnl
define(`SHMEM_DEFINE_FOR_EXTENDED_AMO', `SHMEM_BIND_C_EXTENDED_AMO(`$1')')dnl
dnl
define(`SH_PAD',
`substr(`                    ', 1, eval(len(`$1')+len(`$2')))')dnl
