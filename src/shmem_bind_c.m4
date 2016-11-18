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
define(`SHMEM_EVAL_MACRO_FOR_RMA',
`$1(float,float)$2
$1(double,double)$2
$1(longdouble, long double)$2
$1(char,       char)$2
$1(short,      short)$2
$1(int,        int)$2
$1(long,       long)$2
$1(longlong,   long long)')dnl
dnl
define(`SHMEM_EVAL_MACRO_FOR_AMO',
`$1(int,        int)$2
$1(long,       long)$2
$1(longlong,   long long)')dnl
dnl
define(`SHMEM_EVAL_MACRO_FOR_EXTENDED_AMO',
`SHMEM_EVAL_MACRO_FOR_AMO(`$1',`$2')$2
$1(float,  float)$2
$1(double, double)')dnl
dnl
define(`SHMEM_EVAL_MACRO_FOR_INTS',
`$1(short,    short)$2
$1(int,      int)$2
$1(long,     long)$2
$1(longlong, long long)')dnl
dnl
define(`SHMEM_EVAL_MACRO_FOR_FLOATS',
`$1(float,     float)$2
$1(double,     double)$2
$1(longdouble, long double)')dnl
dnl
define(`SHMEM_EVAL_MACRO_FOR_CMPLX',
`$1(complexf, float complex)$2
$1(complexd, double complex)')dnl
dnl
define(`SHMEM_EVAL_MACRO_FOR_SIZES',
`$1(8,    1*sizeof(uint8_t))$2
$1(16,   2*sizeof(uint8_t))$2
$1(32,   4*sizeof(uint8_t))$2
$1(64,   8*sizeof(uint8_t))$2
$1(128, 16*sizeof(uint8_t))')dnl
dnl
define(`SHMEM_DECLARE_FOR_RMA', `SHMEM_EVAL_MACRO_FOR_RMA(`$1',;)')dnl
define(`SHMEM_DECLARE_FOR_AMO', `SHMEM_EVAL_MACRO_FOR_AMO(`$1',;)')dnl
define(`SHMEM_DECLARE_FOR_EXTENDED_AMO', `SHMEM_EVAL_MACRO_FOR_EXTENDED_AMO(`$1',;)')dnl
define(`SHMEM_DECLARE_FOR_INTS', `SHMEM_EVAL_MACRO_FOR_INTS(`$1',;)')dnl
define(`SHMEM_DECLARE_FOR_FLOATS', `SHMEM_EVAL_MACRO_FOR_FLOATS(`$1',;)')dnl
define(`SHMEM_DECLARE_FOR_CMPLX', `SHMEM_EVAL_MACRO_FOR_CMPLX(`$1',;)')dnl
define(`SHMEM_DECLARE_FOR_SIZES', `SHMEM_EVAL_MACRO_FOR_SIZES(`$1',;)')dnl
dnl
define(`SHMEM_DEFINE_FOR_RMA', `SHMEM_EVAL_MACRO_FOR_RMA(`$1')')dnl
define(`SHMEM_DEFINE_FOR_AMO', `SHMEM_EVAL_MACRO_FOR_AMO(`$1')')dnl
define(`SHMEM_DEFINE_FOR_EXTENDED_AMO', `SHMEM_EVAL_MACRO_FOR_EXTENDED_AMO(`$1')')dnl
define(`SHMEM_DEFINE_FOR_INTS', `SHMEM_EVAL_MACRO_FOR_INTS(`$1')')dnl
define(`SHMEM_DEFINE_FOR_FLOATS', `SHMEM_EVAL_MACRO_FOR_FLOATS(`$1')')dnl
define(`SHMEM_DEFINE_FOR_CMPLX', `SHMEM_EVAL_MACRO_FOR_CMPLX(`$1')')dnl
define(`SHMEM_DEFINE_FOR_SIZES', `SHMEM_EVAL_MACRO_FOR_SIZES(`$1')')dnl
dnl
define(`SHMEM_BIND_C_AMO', dnl args: decl
`$1(int,      int,           SHM_INTERNAL_INT)
$1(long,     long,          SHM_INTERNAL_LONG)
$1(longlong, long long,     SHM_INTERNAL_LONG_LONG)')dnl
dnl
define(`SHMEM_BIND_C_EXTENDED_AMO', dnl args: decl
`SHMEM_BIND_C_AMO(`$1')
$1(float,    float,         SHM_INTERNAL_FLOAT)
$1(double,   double,        SHM_INTERNAL_DOUBLE)')dnl
dnl
define(`SHMEM_BIND_C_INTS_OP', dnl args: decl, s_op, op
`$1(short,    short,         SHM_INTERNAL_SHORT,         $2, $3)
$1(int,      int,           SHM_INTERNAL_INT,           $2, $3)
$1(long,     long,          SHM_INTERNAL_LONG,          $2, $3)
$1(longlong, long long,     SHM_INTERNAL_LONG_LONG,     $2, $3)')dnl
dnl
define(`SHMEM_BIND_C_FLOATS_OP', dnl args: decl, s_op, op
`$1(float,    float,         SHM_INTERNAL_FLOAT,         $2, $3)
$1(double,   double,        SHM_INTERNAL_DOUBLE,        $2, $3)
$1(longdouble, long double, SHM_INTERNAL_LONG_DOUBLE,   $2, $3)')dnl
dnl
define(`SHMEM_BIND_C_CMPLX_OP', dnl args: decl, s_op, op
`$1(complexf, float complex, SHM_INTERNAL_FLOAT_COMPLEX, $2, $3)
$1(complexd, double complex,SHM_INTERNAL_DOUBLE_COMPLEX,$2, $3)')dnl
dnl
define(`SHMEM_BIND_F_RMA', dnl args: decl
`$1(character, CHARACTER,    SIZEOF_FORTRAN_CHARACTER)
$1(complex,  COMPLEX,       SIZEOF_FORTRAN_COMPLEX)
$1(double,   DOUBLE,        SIZEOF_FORTRAN_DOUBLE_PRECISION)
$1(integer,  INTEGER,       SIZEOF_FORTRAN_INTEGER)
$1(logical,  LOGICAL,       SIZEOF_FORTRAN_LOGICAL)
$1(real,     REAL,          SIZEOF_FORTRAN_REAL)')dnl
dnl
define(`SHMEM_BIND_F_SIZES', dnl args: decl
`$1(mem,     MEM,    1)
$1(4,       4,      4)
$1(8,       8,      8)
$1(32,      32,     4)
$1(64,      64,     8)
$1(128,     128,    16)')dnl
dnl
define(`SHMEM_BIND_F_COLL_SIZES', dnl args: decl
`$1(4,       4)
$1(8,       8)
$1(32,      4)
$1(64,      8)')dnl
dnl
define(`SHMEM_BIND_F_INTS_OP', dnl args: decl, s_op_l, s_op_u, op
`$1(int4,    INT4,   int32_t,     SHM_INTERNAL_INT32,  $2, $3, $4, 4)
$1(int8,    INT8,   int64_t,     SHM_INTERNAL_INT64,  $2, $3, $4, 8)')dnl
dnl
define(`SHMEM_BIND_F_FLOATS_OP', dnl args: decl, s_op_l, s_op_u, op
`#if SIZEOF_LONG_DOUBLE == 16
$1(real4,   REAL4,  float,       SHM_INTERNAL_FLOAT,  $2, $3, $4, 4)
$1(real8,   REAL8,  double,      SHM_INTERNAL_DOUBLE, $2, $3, $4, 8)
$1(real16,  REAL16, long double, SHM_INTERNAL_LONG_DOUBLE, $2, $3, $4, 16)
#else
$1(real4,   REAL4,  float,       SHM_INTERNAL_FLOAT,  $2, $3, $4, 4)
$1(real8,   REAL8,  double,      SHM_INTERNAL_DOUBLE, $2, $3, $4, 8)
#endif')dnl
dnl
define(`SH_PAD',
`substr(`                    ', 1, eval(len(`$1')+len(`$2')))')dnl
