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

#ifndef SHMEM_DECL_H
#define SHMEM_DECL_H

#ifdef OPAL_HAVE_ATTRIBUTE_NORETURN
#define SHMEM_ATTRIBUTE_NORETURN __attribute__ ((noreturn))
#else
#define SHMEM_ATTRIBUTE_NORETURN
#endif

#ifndef SHMEM_ATTRIBUTE_VISIBILITY
#   if defined(OPAL_C_HAVE_VISIBILITY) && (OPAL_C_HAVE_VISIBILITY == 1)
#       define SHMEM_ATTRIBUTE_VISIBILITY __attribute__((visibility("default")))
#   else
#       define SHMEM_ATTRIBUTE_VISIBILITY
#   endif
#endif

#ifndef SHMEM_FUNCTION_ATTRIBUTES
#    define SHMEM_FUNCTION_ATTRIBUTES SHMEM_ATTRIBUTE_VISIBILITY
#endif

#endif /* SHMEM_DECL_H */
