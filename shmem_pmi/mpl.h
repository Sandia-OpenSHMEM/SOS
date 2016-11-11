/* -*- C -*-
 *
 * Copyright 2016 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
 * Copyright (c) 2016 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef MPL_H
#define MPL_H

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define ATTRIBUTE __attribute__
#define MPI_MAX_PORT_NAME 256

#define MPL_snprintf(...)       \
    ({ snprintf(__VA_ARGS__);   \
       0;                       \
    })

#define MPL_strncpy(...)        \
    ({ strncpy(__VA_ARGS__);    \
       0;                       \
    })

#define MPL_exit exit

#define MPL_strnapp(...)        \
    ({ strncat(__VA_ARGS__);    \
       0;                       \
    })

#define MPL_internal_error_printf(...) \
    do { fprintf(stderr, __VA_ARGS__); } while(0)

#endif /* MPL_H */
