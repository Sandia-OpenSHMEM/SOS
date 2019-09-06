dnl Copyright (c) 2016 Intel Corporation. All rights reserved.
dnl This software is available to you under the BSD license.
dnl
dnl This file is part of the Sandia OpenSHMEM software package. For license
dnl information, see the LICENSE file in the top level directory of the
dnl distribution.
dnl
dnl This file contains the m4 macros used to generate the SOS C bindings.
dnl
define(`SHMEM_BIND_C_RMA', dnl args: macro_name, end
`$1(float,      float)$2
$1(double,     double)$2
$1(longdouble, long double)$2
$1(char,       char)$2
$1(schar,      signed char)$2
$1(short,      short)$2
$1(int,        int)$2
$1(long,       long)$2
$1(longlong,   long long)$2
$1(uchar,      unsigned char)$2
$1(ushort,     unsigned short)$2
$1(uint,       unsigned int)$2
$1(ulong,      unsigned long)$2
$1(ulonglong,  unsigned long long)$2
$1(int8,       int8_t)$2
$1(int16,      int16_t)$2
$1(int32,      int32_t)$2
$1(int64,      int64_t)$2
$1(uint8,      uint8_t)$2
$1(uint16,     uint16_t)$2
$1(uint32,     uint32_t)$2
$1(uint64,     uint64_t)$2
$1(size,       size_t)$2
$1(ptrdiff,    ptrdiff_t)')dnl
dnl
define(`SHMEM_BIND_C_COLLECTIVES', dnl args: macro_name, end
`$1(float,      float)$2
$1(double,     double)$2
$1(longdouble, long double)$2
$1(char,       char)$2
$1(schar,      signed char)$2
$1(short,      short)$2
$1(int,        int)$2
$1(long,       long)$2
$1(longlong,   long long)$2
$1(uchar,      unsigned char)$2
$1(ushort,     unsigned short)$2
$1(uint,       unsigned int)$2
$1(ulong,      unsigned long)$2
$1(ulonglong,  unsigned long long)$2
$1(int8,       int8_t)$2
$1(int16,      int16_t)$2
$1(int32,      int32_t)$2
$1(int64,      int64_t)$2
$1(uint8,      uint8_t)$2
$1(uint16,     uint16_t)$2
$1(uint32,     uint32_t)$2
$1(uint64,     uint64_t)$2
$1(size,       size_t)$2
$1(ptrdiff,    ptrdiff_t)')dnl
dnl
define(`SHMEM_BIND_C_SIZES', dnl args: macro_name, end
`$1(8,          1)$2
$1(16,         2)$2
$1(32,         4)$2
$1(64,         8)$2
$1(128,        16)')dnl
dnl
define(`SHMEM_BIND_C_AMO', dnl args: macro_name, end
`$1(int,        int,                `SHM_INTERNAL_INT')$2
$1(long,       long,               `SHM_INTERNAL_LONG')$2
$1(longlong,   long long,          `SHM_INTERNAL_LONG_LONG')$2
$1(uint,       unsigned int,       `SHM_INTERNAL_UINT')$2
$1(ulong,      unsigned long,      `SHM_INTERNAL_ULONG')$2
$1(ulonglong,  unsigned long long, `SHM_INTERNAL_ULONG_LONG')$2
$1(int32,      int32_t,            `SHM_INTERNAL_INT32')$2
$1(int64,      int64_t,            `SHM_INTERNAL_INT64')$2
$1(uint32,     uint32_t,           `SHM_INTERNAL_UINT32')$2
$1(uint64,     uint64_t,           `SHM_INTERNAL_UINT64')$2
$1(size,       size_t,             `SHM_INTERNAL_SIZE_T')$2
$1(ptrdiff,    ptrdiff_t,          `SHM_INTERNAL_PTRDIFF_T')')dnl
dnl
define(`SHMEM_BIND_C_EXTENDED_AMO', dnl args: macro_name, end
`SHMEM_BIND_C_AMO(`$1',`$2')$2
$1(float,      float,      `SHM_INTERNAL_FLOAT')$2
$1(double,     double,     `SHM_INTERNAL_DOUBLE')')dnl
dnl
define(`SHMEM_BIND_C_BITWISE_AMO', dnl args: macro_name, end
`$1(uint,       unsigned int,       `SHM_INTERNAL_UINT')$2
$1(ulong,      unsigned long,      `SHM_INTERNAL_ULONG')$2
$1(ulonglong,  unsigned long long, `SHM_INTERNAL_ULONG_LONG')$2
$1(int32,      int32_t,            `SHM_INTERNAL_INT32')$2
$1(int64,      int64_t,            `SHM_INTERNAL_INT64')$2
$1(uint32,     uint32_t,           `SHM_INTERNAL_UINT32')$2
$1(uint64,     uint64_t,           `SHM_INTERNAL_UINT64')')dnl
dnl
define(`SHMEM_BIND_C_WAIT', dnl args: macro_name
`$1(short,      short)
$1(int,        int)
$1(long,       long)
$1(longlong,   long long)')dnl
dnl
define(`SHMEM_BIND_C_SYNC', dnl args: macro_name
`$1(short,      short)
$1(int,        int)
$1(long,       long)
$1(longlong,   long long)
$1(ushort,     unsigned short)
$1(uint,       unsigned int)
$1(ulong,      unsigned long)
$1(ulonglong,  unsigned long long)
$1(int32,      int32_t)
$1(int64,      int64_t)
$1(uint32,     uint32_t)
$1(uint64,     uint64_t)
$1(size,       size_t)
$1(ptrdiff,    ptrdiff_t)')dnl
dnl
define(`SHMEM_BIND_C_COLL_INTS', dnl args: macro_name, op_name, op_const
`$1(short,    short,         `SHM_INTERNAL_SHORT',         `$2', `$3')
$1(int,      int,           `SHM_INTERNAL_INT',           `$2', `$3')
$1(long,     long,          `SHM_INTERNAL_LONG',          `$2', `$3')
$1(longlong, long long,     `SHM_INTERNAL_LONG_LONG',     `$2', `$3')')dnl
dnl
define(`SHMEM_BIND_C_COLL_AND_OR_XOR', dnl args: macro_name, op_name, op_const
`$1(uchar,    unsigned char,      `SHM_INTERNAL_UCHAR',      `$2', `$3')
$1(short,     short,              `SHM_INTERNAL_SHORT',      `$2', `$3')
$1(ushort,    unsigned short,     `SHM_INTERNAL_USHORT',     `$2', `$3')
$1(int,       int,                `SHM_INTERNAL_INT',        `$2', `$3')
$1(uint,      unsigned int,       `SHM_INTERNAL_UINT',       `$2', `$3')
$1(long,      long,               `SHM_INTERNAL_LONG',       `$2', `$3')
$1(ulong,     unsigned long,      `SHM_INTERNAL_ULONG',      `$2', `$3')
$1(longlong,  long long,          `SHM_INTERNAL_LONG_LONG',  `$2', `$3')
$1(ulonglong, unsigned long long, `SHM_INTERNAL_ULONG_LONG', `$2', `$3')')dnl
dnl
define(`SHMEM_BIND_C_COLL_MIN_MAX', dnl args: macro_name, op_name, op_const
`$1(short,     short,              `SHM_INTERNAL_SHORT',       `$2', `$3')
$1(ushort,     unsigned short,     `SHM_INTERNAL_USHORT',      `$2', `$3')
$1(int,        int,                `SHM_INTERNAL_INT',         `$2', `$3')
$1(uint,       unsigned int,       `SHM_INTERNAL_UINT',        `$2', `$3')
$1(long,       long,               `SHM_INTERNAL_LONG',        `$2', `$3')
$1(ulong,      unsigned long,      `SHM_INTERNAL_ULONG',       `$2', `$3')
$1(longlong,   long long,          `SHM_INTERNAL_LONG_LONG',   `$2', `$3')
$1(ulonglong,  unsigned long long, `SHM_INTERNAL_ULONG_LONG',  `$2', `$3')
$1(float,      float,              `SHM_INTERNAL_FLOAT',       `$2', `$3')
$1(double,     double,             `SHM_INTERNAL_DOUBLE',      `$2', `$3')
$1(longdouble, long double,        `SHM_INTERNAL_LONG_DOUBLE', `$2', `$3')')dnl
dnl
define(`SHMEM_BIND_C_COLL_SUM_PROD', dnl args: macro_name, op_name, op_const
`$1(short,     short,              `SHM_INTERNAL_SHORT',          `$2', `$3')
$1(ushort,     unsigned short,     `SHM_INTERNAL_USHORT',         `$2', `$3')
$1(int,        int,                `SHM_INTERNAL_INT',            `$2', `$3')
$1(uint,       unsigned int,       `SHM_INTERNAL_UINT',           `$2', `$3')
$1(long,       long,               `SHM_INTERNAL_LONG',           `$2', `$3')
$1(ulong,      unsigned long,      `SHM_INTERNAL_ULONG',          `$2', `$3')
$1(longlong,   long long,          `SHM_INTERNAL_LONG_LONG',      `$2', `$3')
$1(ulonglong,  unsigned long long, `SHM_INTERNAL_ULONG_LONG',     `$2', `$3')
$1(float,      float,              `SHM_INTERNAL_FLOAT',          `$2', `$3')
$1(double,     double,             `SHM_INTERNAL_DOUBLE',         `$2', `$3')
$1(longdouble, long double,        `SHM_INTERNAL_LONG_DOUBLE',    `$2', `$3')
$1(complexd,   double _Complex,    `SHM_INTERNAL_DOUBLE_COMPLEX', `$2', `$3')
$1(complexf,   float _Complex,     `SHM_INTERNAL_DOUBLE_COMPLEX', `$2', `$3')')dnl
dnl
define(`SHMEM_BIND_C_COLL_FLOATS', dnl args: macro_name, op_name, op_const
`$1(float,    float,         `SHM_INTERNAL_FLOAT',         `$2', `$3')
$1(double,   double,        `SHM_INTERNAL_DOUBLE',        `$2', `$3')
$1(longdouble, long double, `SHM_INTERNAL_LONG_DOUBLE',   `$2', `$3')')dnl
dnl
define(`SHMEM_BIND_C_COLL_CMPLX', dnl args: macro_name, op_name, op_const
`$1(complexf, float _Complex, `SHM_INTERNAL_FLOAT_COMPLEX', `$2', `$3')
$1(complexd, double _Complex,`SHM_INTERNAL_DOUBLE_COMPLEX',`$2', `$3')')dnl
dnl
define(`SHMEM_DECLARE_FOR_RMA', `SHMEM_BIND_C_RMA(`$1',`;');')dnl
define(`SHMEM_DECLARE_FOR_AMO', `SHMEM_BIND_C_AMO(`$1',`;');')dnl
define(`SHMEM_DECLARE_FOR_SIZES', `SHMEM_BIND_C_SIZES(`$1',`;');')dnl
define(`SHMEM_DECLARE_FOR_EXTENDED_AMO', `SHMEM_BIND_C_EXTENDED_AMO(`$1',`;');')dnl
define(`SHMEM_DECLARE_FOR_BITWISE_AMO', `SHMEM_BIND_C_BITWISE_AMO(`$1',`;');')dnl
define(`SHMEM_DECLARE_FOR_COLLECTIVES', `SHMEM_BIND_C_COLLECTIVES(`$1',`;');')dnl
dnl
define(`SHMEM_DEFINE_FOR_RMA', `SHMEM_BIND_C_RMA(`$1')')dnl
define(`SHMEM_DEFINE_FOR_AMO', `SHMEM_BIND_C_AMO(`$1')')dnl
define(`SHMEM_DEFINE_FOR_SIZES', `SHMEM_BIND_C_SIZES(`$1')')dnl
define(`SHMEM_DEFINE_FOR_EXTENDED_AMO', `SHMEM_BIND_C_EXTENDED_AMO(`$1')')dnl
define(`SHMEM_DEFINE_FOR_BITWISE_AMO', `SHMEM_BIND_C_BITWISE_AMO(`$1')')dnl
define(`SHMEM_DEFINE_FOR_COLLECTIVES', `SHMEM_BIND_C_COLLECTIVES(`$1')')dnl
dnl
define(`SH_PAD',
`substr(`                    ', 1, eval(len(`$1')+len(`$2')))')dnl
