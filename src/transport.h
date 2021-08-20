/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2017 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef TRANSPORT_H
#define TRANSPORT_H

/* Datatypes */
enum shm_internal_datatype_t {
    SHM_INTERNAL_SIGNED_BYTE,
    SHM_INTERNAL_CHAR,
    SHM_INTERNAL_SHORT,
    SHM_INTERNAL_INT,
    SHM_INTERNAL_LONG,
    SHM_INTERNAL_LONG_LONG,
    SHM_INTERNAL_FORTRAN_INTEGER,
    SHM_INTERNAL_INT8,
    SHM_INTERNAL_INT16,
    SHM_INTERNAL_INT32,
    SHM_INTERNAL_INT64,
    SHM_INTERNAL_PTRDIFF_T,
    SHM_INTERNAL_UCHAR,
    SHM_INTERNAL_USHORT,
    SHM_INTERNAL_UINT,
    SHM_INTERNAL_ULONG,
    SHM_INTERNAL_ULONG_LONG,
    SHM_INTERNAL_UINT8,
    SHM_INTERNAL_UINT16,
    SHM_INTERNAL_UINT32,
    SHM_INTERNAL_UINT64,
    SHM_INTERNAL_SIZE_T,
    SHM_INTERNAL_FLOAT,
    SHM_INTERNAL_DOUBLE,
    SHM_INTERNAL_LONG_DOUBLE,
    SHM_INTERNAL_FLOAT_COMPLEX,
    SHM_INTERNAL_DOUBLE_COMPLEX
};

typedef enum shm_internal_datatype_t shm_internal_datatype_t;

#if defined (USE_PORTALS4)
#include "transport_portals4.h"

#elif defined (USE_OFI)
#include "transport_ofi.h"

#elif defined (USE_UCX)
#include "transport_ucx.h"

#else /* No transport */
#include "transport_none.h"

#endif /* Transport selection */

#endif /* TRANSPORT_H */
