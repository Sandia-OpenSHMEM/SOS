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


/* Synopsis: Use atomic fetch to spin on var until it changes, then release
 * neighbor process.
 */

#include <stdio.h>
#include <shmem.h>

int       var_int      = -1;
long      var_long     = -1;
long long var_longlong = -1;
float     var_float    = -1;
double    var_double   = -1;

#define SPIN_TEST(typename, fmt)                                        \
do {                                                                    \
    printf("%d: Entering %s test\n", me, #typename);                    \
    if (me == 0)                                                        \
        shmem_##typename##_atomic_set(&var_##typename, nproc-1, me);    \
                                                                        \
    while (0 > shmem_##typename##_atomic_fetch(&var_##typename, me)) ;  \
                                                                        \
    shmem_##typename##_atomic_set(&var_##typename, me, (me+1) % nproc); \
                                                                        \
    if (var_##typename - ((me + (nproc-1)) % nproc) > 0.01) {           \
        printf("[%d] Type '%s' expected %d, got " fmt "\n", me,         \
               #typename, me + (nproc-1) % nproc, var_##typename);      \
        shmem_global_exit(1);                                           \
    }                                                                   \
    printf("%d: Finished %s test\n", me, #typename);\
} while (0)

int main(int argc, char **argv) {
    int me, nproc;

    shmem_init();

    me = shmem_my_pe();
    nproc = shmem_n_pes();

    SPIN_TEST(int, "%d");
    SPIN_TEST(long, "%ld");
    SPIN_TEST(longlong, "%lld");
    SPIN_TEST(float, "%f");
    SPIN_TEST(double, "%lf");

    shmem_finalize();

    return 0;
}
