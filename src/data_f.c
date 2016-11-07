/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define SHMEM_INTERNAL_INCLUDE
#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"


#define FC_SHMEM_CHARACTER_PUT FC_FUNC_(shmem_character_put, SHMEM_CHARACTER_PUT)
void FC_SHMEM_CHARACTER_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_CHARACTER_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, SIZEOF_FORTRAN_CHARACTER * *len, *pe,
                          &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_COMPLEX_PUT FC_FUNC_(shmem_complex_put, SHMEM_COMPLEX_PUT)
void FC_SHMEM_COMPLEX_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_COMPLEX_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, SIZEOF_FORTRAN_COMPLEX * *len, *pe,
                          &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_DOUBLE_PUT FC_FUNC_(shmem_double_put, SHMEM_DOUBLE_PUT)
void FC_SHMEM_DOUBLE_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_DOUBLE_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, SIZEOF_FORTRAN_DOUBLE_PRECISION *
                          *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_INTEGER_PUT FC_FUNC_(shmem_integer_put, SHMEM_INTEGER_PUT)
void FC_SHMEM_INTEGER_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_INTEGER_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, SIZEOF_FORTRAN_INTEGER * *len, *pe,
                          &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_LOGICAL_PUT FC_FUNC_(shmem_logical_put, SHMEM_LOGICAL_PUT)
void FC_SHMEM_LOGICAL_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_LOGICAL_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, SIZEOF_FORTRAN_LOGICAL * *len, *pe,
                          &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_REAL_PUT FC_FUNC_(shmem_real_put, SHMEM_REAL_PUT)
void FC_SHMEM_REAL_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_REAL_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, SIZEOF_FORTRAN_REAL * *len, *pe,
                          &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT FC_FUNC_(shmem_put, SHMEM_PUT)
void FC_SHMEM_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT4 FC_FUNC_(shmem_put4, SHMEM_PUT4)
void FC_SHMEM_PUT4(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT4(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, 4 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT8 FC_FUNC_(shmem_put8, SHMEM_PUT8)
void FC_SHMEM_PUT8(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT8(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, 8 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT32 FC_FUNC_(shmem_put32, SHMEM_PUT32)
void FC_SHMEM_PUT32(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT32(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, 4 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT64 FC_FUNC_(shmem_put64, SHMEM_PUT64)
void FC_SHMEM_PUT64(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT64(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, 8 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT128 FC_FUNC_(shmem_put128, SHMEM_PUT128)
void FC_SHMEM_PUT128(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT128(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, 16 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUTMEM FC_FUNC_(shmem_putmem, SHMEM_PUTMEM)
void FC_SHMEM_PUTMEM(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUTMEM(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(target);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_put_nb(target, source, *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_CHARACTER_PUT_NBI FC_FUNC_(shmem_character_put_nbi, SHMEM_CHARACTER_PUT_NBI)
void FC_SHMEM_CHARACTER_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_CHARACTER_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, SIZEOF_FORTRAN_CHARACTER * *nelems, *pe, NULL);
}


#define FC_SHMEM_COMPLEX_PUT_NBI FC_FUNC_(shmem_complex_put_nbi, SHMEM_COMPLEX_PUT_NBI)
void FC_SHMEM_COMPLEX_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_COMPLEX_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, SIZEOF_FORTRAN_COMPLEX * *nelems, *pe, NULL);
}


#define FC_SHMEM_DOUBLE_PUT_NBI FC_FUNC_(shmem_double_put_nbi, SHMEM_DOUBLE_PUT_NBI)
void FC_SHMEM_DOUBLE_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_DOUBLE_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, SIZEOF_FORTRAN_DOUBLE_PRECISION * *nelems, *pe, NULL);
}


#define FC_SHMEM_INTEGER_PUT_NBI FC_FUNC_(shmem_integer_put_nbi, SHMEM_INTEGER_PUT_NBI)
void FC_SHMEM_INTEGER_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_INTEGER_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, SIZEOF_FORTRAN_INTEGER * *nelems, *pe, NULL);
}


#define FC_SHMEM_LOGICAL_PUT_NBI FC_FUNC_(shmem_logical_put_nbi, SHMEM_LOGICAL_PUT_NBI)
void FC_SHMEM_LOGICAL_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_LOGICAL_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, SIZEOF_FORTRAN_LOGICAL * *nelems, *pe, NULL);
}


#define FC_SHMEM_PUT4_NBI FC_FUNC_(shmem_put4_nbi, SHMEM_PUT4_NBI)
void FC_SHMEM_PUT4_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_PUT4_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, 4 * *nelems, *pe, NULL);
}


#define FC_SHMEM_PUT8_NBI FC_FUNC_(shmem_put8_nbi, SHMEM_PUT8_NBI)
void FC_SHMEM_PUT8_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_PUT8_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, 8 * *nelems, *pe, NULL);
}


#define FC_SHMEM_PUT32_NBI FC_FUNC_(shmem_put32_nbi, SHMEM_PUT32_NBI)
void FC_SHMEM_PUT32_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_PUT32_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, 4 * *nelems, *pe, NULL);
}


#define FC_SHMEM_PUT64_NBI FC_FUNC_(shmem_put64_nbi, SHMEM_PUT64_NBI)
void FC_SHMEM_PUT64_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_PUT64_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, 8 * *nelems, *pe, NULL);
}


#define FC_SHMEM_PUT128_NBI FC_FUNC_(shmem_put128_nbi, SHMEM_PUT128_NBI)
void FC_SHMEM_PUT128_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_PUT128_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, 16 * *nelems, *pe, NULL);
}


#define FC_SHMEM_PUTMEM_NBI FC_FUNC_(shmem_putmem_nbi, SHMEM_PUTMEM_NBI)
void FC_SHMEM_PUTMEM_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_PUTMEM_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, *nelems, *pe, NULL);
}


#define FC_SHMEM_REAL_PUT_NBI FC_FUNC_(shmem_real_put_nbi, SHMEM_REAL_PUT_NBI)
void FC_SHMEM_REAL_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_REAL_PUT_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(dest);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_put_nb(dest, source, SIZEOF_FORTRAN_REAL * *nelems, *pe, NULL);
}


#define FC_SHMEM_COMPLEX_IPUT FC_FUNC_(shmem_complex_iput, SHMEM_COMPLEX_IPUT)
void FC_SHMEM_COMPLEX_IPUT(void *targetp, void *sourcep,
			   fortran_integer_t *tst, fortran_integer_t *sst,
			   fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_COMPLEX_IPUT(void *targetp, void *sourcep,
		      fortran_integer_t *tst, fortran_integer_t *sst,
		      fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, SIZEOF_FORTRAN_COMPLEX, *pe);
	target += (*tst * SIZEOF_FORTRAN_COMPLEX);
	source += (*sst * SIZEOF_FORTRAN_COMPLEX);
    }
}


#define FC_SHMEM_DOUBLE_IPUT FC_FUNC_(shmem_double_iput, SHMEM_DOUBLE_IPUT)
void FC_SHMEM_DOUBLE_IPUT(void *targetp, void *sourcep,
			   fortran_integer_t *tst, fortran_integer_t *sst,
			   fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_DOUBLE_IPUT(void *targetp, void *sourcep,
		      fortran_integer_t *tst, fortran_integer_t *sst,
		      fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, SIZEOF_FORTRAN_DOUBLE_PRECISION, *pe);
	target += (*tst * SIZEOF_FORTRAN_DOUBLE_PRECISION);
	source += (*sst * SIZEOF_FORTRAN_DOUBLE_PRECISION);
    }
}



#define FC_SHMEM_INTEGER_IPUT FC_FUNC_(shmem_integer_iput, SHMEM_INTEGER_IPUT)
void FC_SHMEM_INTEGER_IPUT(void *targetp, void *sourcep,
			   fortran_integer_t *tst, fortran_integer_t *sst,
			   fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_INTEGER_IPUT(void *targetp, void *sourcep,
		      fortran_integer_t *tst, fortran_integer_t *sst,
		      fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, SIZEOF_FORTRAN_INTEGER, *pe);
	target += (*tst * SIZEOF_FORTRAN_INTEGER);
	source += (*sst * SIZEOF_FORTRAN_INTEGER);
    }
}


#define FC_SHMEM_LOGICAL_IPUT FC_FUNC_(shmem_logical_iput, SHMEM_LOGICAL_IPUT)
void FC_SHMEM_LOGICAL_IPUT(void *targetp, void *sourcep,
			   fortran_integer_t *tst, fortran_integer_t *sst,
			   fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_LOGICAL_IPUT(void *targetp, void *sourcep,
		      fortran_integer_t *tst, fortran_integer_t *sst,
		      fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, SIZEOF_FORTRAN_LOGICAL, *pe);
	target += (*tst * SIZEOF_FORTRAN_LOGICAL);
	source += (*sst * SIZEOF_FORTRAN_LOGICAL);
    }
}


#define FC_SHMEM_REAL_IPUT FC_FUNC_(shmem_real_iput, SHMEM_REAL_IPUT)
void FC_SHMEM_REAL_IPUT(void *targetp, void *sourcep,
			fortran_integer_t *tst, fortran_integer_t *sst,
			fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_REAL_IPUT(void *targetp, void *sourcep,
		   fortran_integer_t *tst, fortran_integer_t *sst,
		   fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, SIZEOF_FORTRAN_REAL, *pe);
	target += (*tst * SIZEOF_FORTRAN_REAL);
	source += (*sst * SIZEOF_FORTRAN_REAL);
    }
}


#define FC_SHMEM_IPUT4 FC_FUNC_(shmem_iput4, SHMEM_IPUT4)
void FC_SHMEM_IPUT4(void *targetp, void *sourcep,
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IPUT4(void *targetp, void *sourcep,
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, 4, *pe);
	target += (*tst * 4);
	source += (*sst * 4);
    }
}


#define FC_SHMEM_IPUT8 FC_FUNC_(shmem_iput8, SHMEM_IPUT8)
void FC_SHMEM_IPUT8(void *targetp, void *sourcep,
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IPUT8(void *targetp, void *sourcep,
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, 8, *pe);
	target += (*tst * 8);
	source += (*sst * 8);
    }
}



#define FC_SHMEM_IPUT32 FC_FUNC_(shmem_iput32, SHMEM_IPUT32)
void FC_SHMEM_IPUT32(void *targetp, void *sourcep,
		     fortran_integer_t *tst, fortran_integer_t *sst,
		     fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IPUT32(void *targetp, void *sourcep,
		fortran_integer_t *tst, fortran_integer_t *sst,
		fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, 4, *pe);
	target += (*tst * 4);
	source += (*sst * 4);
    }
}


#define FC_SHMEM_IPUT64 FC_FUNC_(shmem_iput64, SHMEM_IPUT64)
void FC_SHMEM_IPUT64(void *targetp, void *sourcep,
		     fortran_integer_t *tst, fortran_integer_t *sst,
		     fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IPUT64(void *targetp, void *sourcep,
		fortran_integer_t *tst, fortran_integer_t *sst,
		fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, 8, *pe);
	target += (*tst * 8);
	source += (*sst * 8);
    }
}


#define FC_SHMEM_IPUT128 FC_FUNC_(shmem_iput128, SHMEM_IPUT128)
void FC_SHMEM_IPUT128(void *targetp, void *sourcep,
		      fortran_integer_t *tst, fortran_integer_t *sst,
		      fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IPUT128(void *targetp, void *sourcep,
		 fortran_integer_t *tst, fortran_integer_t *sst,
		 fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(targetp);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len) {
        shmem_internal_put_small(target, source, 16, *pe);
	target += (*tst * 16);
	source += (*sst * 16);
    }
}


#define FC_SHMEM_CHARACTER_GET FC_FUNC_(shmem_character_get, SHMEM_CHARACTER_GET)
void FC_SHMEM_CHARACTER_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_CHARACTER_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, SIZEOF_FORTRAN_CHARACTER * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_COMPLEX_GET FC_FUNC_(shmem_complex_get, SHMEM_COMPLEX_GET)
void FC_SHMEM_COMPLEX_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_COMPLEX_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, SIZEOF_FORTRAN_COMPLEX * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_DOUBLE_GET FC_FUNC_(shmem_double_get, SHMEM_DOUBLE_GET)
void FC_SHMEM_DOUBLE_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_DOUBLE_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, SIZEOF_FORTRAN_DOUBLE_PRECISION * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_INTEGER_GET FC_FUNC_(shmem_integer_get, SHMEM_INTEGER_GET)
void FC_SHMEM_INTEGER_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_INTEGER_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, SIZEOF_FORTRAN_INTEGER * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET4 FC_FUNC_(shmem_get4, SHMEM_GET4)
void FC_SHMEM_GET4(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET4(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, 4 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET8 FC_FUNC_(shmem_get8, SHMEM_GET8)
void FC_SHMEM_GET8(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET8(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, 8 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET32 FC_FUNC_(shmem_get32, SHMEM_GET32)
void FC_SHMEM_GET32(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET32(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, 4 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET64 FC_FUNC_(shmem_get64, SHMEM_GET64)
void FC_SHMEM_GET64(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET64(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, 8 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET128 FC_FUNC_(shmem_get128, SHMEM_GET128)
void FC_SHMEM_GET128(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET128(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, 16 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GETMEM FC_FUNC_(shmem_getmem, SHMEM_GETMEM)
void FC_SHMEM_GETMEM(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GETMEM(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_LOGICAL_GET FC_FUNC_(shmem_logical_get, SHMEM_LOGICAL_GET)
void FC_SHMEM_LOGICAL_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_LOGICAL_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, SIZEOF_FORTRAN_LOGICAL * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_REAL_GET FC_FUNC_(shmem_real_get, SHMEM_REAL_GET)
void FC_SHMEM_REAL_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_REAL_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*len);

    shmem_internal_get(target, source, SIZEOF_FORTRAN_REAL * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_CHARACTER_GET_NBI FC_FUNC_(shmem_character_get_nbi, SHMEM_CHARACTER_GET_NBI)
void FC_SHMEM_CHARACTER_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_CHARACTER_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, SIZEOF_FORTRAN_CHARACTER * *nelems, *pe);
}


#define FC_SHMEM_COMPLEX_GET_NBI FC_FUNC_(shmem_complex_get_nbi, SHMEM_COMPLEX_GET_NBI)
void FC_SHMEM_COMPLEX_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_COMPLEX_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, SIZEOF_FORTRAN_COMPLEX * *nelems, *pe);
}


#define FC_SHMEM_DOUBLE_GET_NBI FC_FUNC_(shmem_double_get_nbi, SHMEM_DOUBLE_GET_NBI)
void FC_SHMEM_DOUBLE_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_DOUBLE_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, SIZEOF_FORTRAN_DOUBLE_PRECISION * *nelems, *pe);
}


#define FC_SHMEM_GET4_NBI FC_FUNC_(shmem_get4_nbi, SHMEM_GET4_NBI)
void FC_SHMEM_GET4_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_GET4_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, 4 * *nelems, *pe);
}


#define FC_SHMEM_GET8_NBI FC_FUNC_(shmem_get8_nbi, SHMEM_GET8_NBI)
void FC_SHMEM_GET8_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_GET8_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, 8 * *nelems, *pe);
}


#define FC_SHMEM_GET32_NBI FC_FUNC_(shmem_get32_nbi, SHMEM_GET32_NBI)
void FC_SHMEM_GET32_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_GET32_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, 4 * *nelems, *pe);
}


#define FC_SHMEM_GET64_NBI FC_FUNC_(shmem_get64_nbi, SHMEM_GET64_NBI)
void FC_SHMEM_GET64_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_GET64_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, 8 * *nelems, *pe);
}


#define FC_SHMEM_GET128_NBI FC_FUNC_(shmem_get128_nbi, SHMEM_GET128_NBI)
void FC_SHMEM_GET128_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_GET128_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, 16 * *nelems, *pe);
}


#define FC_SHMEM_GETMEM_NBI FC_FUNC_(shmem_getmem_nbi, SHMEM_GETMEM_NBI)
void FC_SHMEM_GETMEM_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_GETMEM_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, *nelems, *pe);
}


#define FC_SHMEM_INTEGER_GET_NBI FC_FUNC_(shmem_integer_get_nbi, SHMEM_INTEGER_GET_NBI)
void FC_SHMEM_INTEGER_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_INTEGER_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, SIZEOF_FORTRAN_INTEGER * *nelems, *pe);
}


#define FC_SHMEM_LOGICAL_GET_NBI FC_FUNC_(shmem_logical_get_nbi, SHMEM_LOGICAL_GET_NBI)
void FC_SHMEM_LOGICAL_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_LOGICAL_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, SIZEOF_FORTRAN_LOGICAL * *nelems, *pe);
}


#define FC_SHMEM_REAL_GET_NBI FC_FUNC_(shmem_real_get_nbi, SHMEM_REAL_GET_NBI)
void FC_SHMEM_REAL_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe);
void
FC_SHMEM_REAL_GET_NBI(void *dest, void *source, fortran_integer_t *nelems, fortran_integer_t *pe)
{
    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(source);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*nelems);

    shmem_internal_get(dest, source, SIZEOF_FORTRAN_REAL * *nelems, *pe);
}


#define FC_SHMEM_IGET4 FC_FUNC_(shmem_iget4, SHMEM_IGET4)
void FC_SHMEM_IGET4(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IGET4(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, 4, *pe);
	target += (*tst * 4);
	source += (*sst * 4);
    }
    shmem_internal_get_wait();
}


#define FC_SHMEM_IGET8 FC_FUNC_(shmem_iget8, SHMEM_IGET8)
void FC_SHMEM_IGET8(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IGET8(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, 8, *pe);
	target += (*tst * 8);
	source += (*sst * 8);
    }
    shmem_internal_get_wait();
}


#define FC_SHMEM_IGET32 FC_FUNC_(shmem_iget32, SHMEM_IGET32)
void FC_SHMEM_IGET32(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IGET32(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, 4, *pe);
	target += (*tst * 4);
	source += (*sst * 4);
    }
    shmem_internal_get_wait();
}


#define FC_SHMEM_IGET64 FC_FUNC_(shmem_iget64, SHMEM_IGET64)
void FC_SHMEM_IGET64(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IGET64(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, 8, *pe);
	target += (*tst * 8);
	source += (*sst * 8);
    }
    shmem_internal_get_wait();
}


#define FC_SHMEM_IGET128 FC_FUNC_(shmem_iget128, SHMEM_IGET128)
void FC_SHMEM_IGET128(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_IGET128(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, 16, *pe);
	target += (*tst * 16);
	source += (*sst * 16);
    }
    shmem_internal_get_wait();
}


#define FC_SHMEM_COMPLEX_IGET FC_FUNC_(shmem_complex_iget, SHMEM_COMPLEX_IGET)
void FC_SHMEM_COMPLEX_IGET(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_COMPLEX_IGET(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, SIZEOF_FORTRAN_COMPLEX, *pe);
	target += (*tst * SIZEOF_FORTRAN_COMPLEX);
	source += (*sst * SIZEOF_FORTRAN_COMPLEX);
    }
    shmem_internal_get_wait();
}


#define FC_SHMEM_DOUBLE_IGET FC_FUNC_(shmem_double_iget, SHMEM_DOUBLE_IGET)
void FC_SHMEM_DOUBLE_IGET(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_DOUBLE_IGET(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, SIZEOF_FORTRAN_DOUBLE_PRECISION, *pe);
	target += (*tst * SIZEOF_FORTRAN_DOUBLE_PRECISION);
	source += (*sst * SIZEOF_FORTRAN_DOUBLE_PRECISION);
    }
    shmem_internal_get_wait();
}


#define FC_SHMEM_INTEGER_IGET FC_FUNC_(shmem_integer_iget, SHMEM_INTEGER_IGET)
void FC_SHMEM_INTEGER_IGET(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_INTEGER_IGET(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, SIZEOF_FORTRAN_INTEGER, *pe);
	target += (*tst * SIZEOF_FORTRAN_INTEGER);
	source += (*sst * SIZEOF_FORTRAN_INTEGER);
    }
    shmem_internal_get_wait();
}


#define FC_SHMEM_LOGICAL_IGET FC_FUNC_(shmem_logical_iget, SHMEM_LOGICAL_IGET)
void FC_SHMEM_LOGICAL_IGET(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_LOGICAL_IGET(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, SIZEOF_FORTRAN_LOGICAL, *pe);
	target += (*tst * SIZEOF_FORTRAN_LOGICAL);
	source += (*sst * SIZEOF_FORTRAN_LOGICAL);
    }
    shmem_internal_get_wait();
}


#define FC_SHMEM_REAL_IGET FC_FUNC_(shmem_real_iget, SHMEM_REAL_IGET)
void FC_SHMEM_REAL_IGET(void *targetp, void *sourcep, 
		    fortran_integer_t *tst, fortran_integer_t *sst,
		    fortran_integer_t *lenp, fortran_integer_t *pe);
void
FC_SHMEM_REAL_IGET(void *targetp, void *sourcep, 
	       fortran_integer_t *tst, fortran_integer_t *sst,
	       fortran_integer_t *lenp, fortran_integer_t *pe)
{
    fortran_integer_t len = *lenp;
    char *target = (char*) targetp;
    char *source = (char*) sourcep;

    SHMEM_ERR_CHECK_INITIALIZED();
    SHMEM_ERR_CHECK_PE(*pe);
    SHMEM_ERR_CHECK_SYMMETRIC(sourcep);
    SHMEM_ERR_CHECK_POSITIVE(*tst);
    SHMEM_ERR_CHECK_POSITIVE(*sst);
    SHMEM_ERR_CHECK_NON_NEGATIVE(*lenp);

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, SIZEOF_FORTRAN_REAL, *pe);
	target += (*tst * SIZEOF_FORTRAN_REAL);
	source += (*sst * SIZEOF_FORTRAN_REAL);
    }
    shmem_internal_get_wait();
}









