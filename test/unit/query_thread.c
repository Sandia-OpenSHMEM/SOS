/*
 *  Copyright (c) 2018 Intel Corporation. All rights reserved.
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
#include <shmem.h>

int
main(int argc, char* argv[])
{
    int provided;

#if defined(ENABLE_THREADS)
    int tl, ret;
    ret = shmem_init_thread(SHMEM_THREAD_FUNNELED, &tl);

    if (tl != SHMEM_THREAD_FUNNELED || ret != 0) {
        printf("Init failed (requested thread level %d, got %d, ret %d)\n",
               SHMEM_THREAD_FUNNELED, tl, ret);
        if (ret == 0) {
            shmem_global_exit(1);
        } else {
            return ret;
        }
    }
#else
    shmem_init();
#endif

    shmem_query_thread(&provided);
    printf("%d: Query result for thread level %d\n", shmem_my_pe(), provided);

#if defined(ENABLE_THREADS)
    if (provided != SHMEM_THREAD_FUNNELED) 
        shmem_global_exit(1);
#else
    if (provided != SHMEM_THREAD_SINGLE && provided != SHMEM_THREAD_FUNNELED && 
        provided != SHMEM_THREAD_SERIALIZED && provided != SHMEM_THREAD_MULTIPLE) 
        shmem_global_exit(1);
#endif

    shmem_finalize();
    return 0;
}
