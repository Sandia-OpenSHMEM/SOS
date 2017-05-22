/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
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

#ifndef SHMEM_DECL_H
#define SHMEM_DECL_H

#ifdef HAVE_FUNC_ATTRIBUTE_NORETURN
#define __shmem_attribute_noreturn__ __attribute__ ((noreturn))
#else
#define __shmem_attribute_noreturn__
#endif

#ifndef __shmem_attribute_visibility__
#   if defined(OPAL_C_HAVE_VISIBILITY) && (OPAL_C_HAVE_VISIBILITY == 1)
#       define __shmem_attribute_visibility__ __attribute__((visibility("default")))
#   else
#       define __shmem_attribute_visibility__
#   endif
#endif

#ifndef __shmem_function_attributes__
#    define __shmem_function_attributes__ __shmem_attribute_visibility__
#endif

#endif /* SHMEM_DECL_H */
