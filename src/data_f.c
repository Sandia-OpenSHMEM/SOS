/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * This file is part of the Portals SHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#include "config.h"

#include <portals4.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "shmem.h"
#include "shmem_internal.h"
#include "shmem_comm.h"


#define FC_SHMEM_CHARACTER_PUT FC_FUNC_(shmem_character_put, SHMEM_CHARACTER_PUT)
void FC_SHMEM_CHARACTER_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_CHARACTER_PUT(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT4 FC_FUNC_(shmem_put4, SHMEM_PUT4)
void FC_SHMEM_PUT4(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT4(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, 4 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT8 FC_FUNC_(shmem_put8, SHMEM_PUT8)
void FC_SHMEM_PUT8(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT8(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, 8 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT32 FC_FUNC_(shmem_put32, SHMEM_PUT32)
void FC_SHMEM_PUT32(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT32(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, 4 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT64 FC_FUNC_(shmem_put64, SHMEM_PUT64)
void FC_SHMEM_PUT64(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT64(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, 8 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUT128 FC_FUNC_(shmem_put128, SHMEM_PUT128)
void FC_SHMEM_PUT128(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUT128(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, 16 * *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
}


#define FC_SHMEM_PUTMEM FC_FUNC_(shmem_putmem, SHMEM_PUTMEM)
void FC_SHMEM_PUTMEM(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_PUTMEM(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
    long completion = 0;

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_put_nb(target, source, *len, *pe, &completion);
    shmem_internal_put_wait(&completion);
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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, SIZEOF_FORTRAN_CHARACTER * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_COMPLEX_GET FC_FUNC_(shmem_complex_get, SHMEM_COMPLEX_GET)
void FC_SHMEM_COMPLEX_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_COMPLEX_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, SIZEOF_FORTRAN_COMPLEX * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_DOUBLE_GET FC_FUNC_(shmem_double_get, SHMEM_DOUBLE_GET)
void FC_SHMEM_DOUBLE_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_DOUBLE_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, SIZEOF_FORTRAN_DOUBLE_PRECISION * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_INTEGER_GET FC_FUNC_(shmem_integer_get, SHMEM_INTEGER_GET)
void FC_SHMEM_INTEGER_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_INTEGER_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, SIZEOF_FORTRAN_INTEGER * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET4 FC_FUNC_(shmem_get4, SHMEM_GET4)
void FC_SHMEM_GET4(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET4(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, 4 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET8 FC_FUNC_(shmem_get8, SHMEM_GET8)
void FC_SHMEM_GET8(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET8(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, 8 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET32 FC_FUNC_(shmem_get32, SHMEM_GET32)
void FC_SHMEM_GET32(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET32(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, 4 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET64 FC_FUNC_(shmem_get64, SHMEM_GET64)
void FC_SHMEM_GET64(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET64(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, 8 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GET128 FC_FUNC_(shmem_get128, SHMEM_GET128)
void FC_SHMEM_GET128(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GET128(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, 16 * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_GETMEM FC_FUNC_(shmem_getmem, SHMEM_GETMEM)
void FC_SHMEM_GETMEM(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_GETMEM(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_LOGICAL_GET FC_FUNC_(shmem_logical_get, SHMEM_LOGICAL_GET)
void FC_SHMEM_LOGICAL_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_LOGICAL_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, SIZEOF_FORTRAN_LOGICAL * *len, *pe);
    shmem_internal_get_wait();
}


#define FC_SHMEM_REAL_GET FC_FUNC_(shmem_real_get, SHMEM_REAL_GET)
void FC_SHMEM_REAL_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe);
void
FC_SHMEM_REAL_GET(void *target, void *source, fortran_integer_t *len, fortran_integer_t *pe)
{
#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    shmem_internal_get(target, source, SIZEOF_FORTRAN_REAL * *len, *pe);
    shmem_internal_get_wait();
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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

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

#ifdef ENABLE_ERROR_CHECKING
    if (!shmem_internal_initialized) {
        RAISE_ERROR_STR("library not initialized");
    }
#endif

    for ( ; len > 0 ; --len ) {
        shmem_internal_get(target, source, SIZEOF_FORTRAN_REAL, *pe);
	target += (*tst * SIZEOF_FORTRAN_REAL);
	source += (*sst * SIZEOF_FORTRAN_REAL);
    }
    shmem_internal_get_wait();
}









