/* -*- C -*-
 *
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 * 
 * Copyright (c) 2015 Intel Corporation. All rights reserved.
 * This software is available to you under the BSD license.
 *
 * This file is part of the Sandia OpenSHMEM software package. For license
 * information, see the LICENSE file in the top level directory of the
 * distribution.
 *
 */

#ifndef TRANSPORT_H
#define TRANSPORT_H

#if defined (USE_PORTALS4)
#include "transport_portals4.h"

#elif defined (USE_OFI)
#include "transport_ofi.h"

#else /* No transport */

#include "transport_none.h"

#endif /* Transport selection */

#endif /* TRANSPORT_H */
