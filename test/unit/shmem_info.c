/*
 *  Copyright (c) 2017 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 *      Redistribution and use in source and binary forms, with or
 *      without modification, are permitted provided that the following
 *      conditions are met:
 *
 *      - Redistributions of source code must retain the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer.
 *
 *      - Redistributions in binary form must reproduce the above
 *        copyright notice, this list of conditions and the following
 *        disclaimer in the documentation and/or other materials
 *        provided with the distribution.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <shmem.h>

int
main(int argc, char* argv[])
{
    int major_ver, minor_ver;
    char name[SHMEM_MAX_NAME_LEN];

    shmem_init();

    shmem_info_get_version(&major_ver, &minor_ver);
    shmem_info_get_name(name);

    assert(strlen(name) <= SHMEM_MAX_NAME_LEN);
    assert(major_ver == SHMEM_MAJOR_VERSION);
    assert(minor_ver == SHMEM_MINOR_VERSION);
    assert(major_ver >= 1);
    assert(minor_ver >= 0);
    assert(strcmp(name, SHMEM_VENDOR_STRING) == 0);

    printf("%d: OpenSHMEM %d.%d -- \"%s\"\n", shmem_my_pe(), major_ver,
           minor_ver, name);

    shmem_finalize();
    return 0;
}
