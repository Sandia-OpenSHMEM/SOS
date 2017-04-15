/*
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
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

#include <shmem.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <time.h>
#include <math.h>

#define NUM_POINTS 10000


long long inside = 0, total = 0;

int
main(int argc, char* argv[], char *envp[])
{
    int me, myshmem_n_pes;
    /*
    ** Starts/Initializes SHMEM/OpenSHMEM
    */
    shmem_init();
    /*
    ** Fetch the number or processes
    ** Some implementations use num_pes();
    */
    myshmem_n_pes = shmem_n_pes();
    /*
    ** Assign my process ID to me
    */
    me = shmem_my_pe();

    srand(1+me);

    for(total = 0; total < NUM_POINTS; ++total) {
        double x,y;
        x = rand()/(double)RAND_MAX;
        y = rand()/(double)RAND_MAX;

        if(x*x + y*y < 1) {
            ++inside;
        }
    }

    shmem_barrier_all();

    int errors = 0;

    if(me == 0) {
        for(int i = 1; i < myshmem_n_pes; ++i) {
            long long remoteInside,remoteTotal;
            shmem_longlong_get(&remoteInside,&inside,1,i);
            shmem_longlong_get(&remoteTotal,&total,1,i);
            total += remoteTotal;
            inside += remoteInside;
        }

        double approx_pi = 4.0*inside/(double)total;

        if(fabs(M_PI-approx_pi) > 0.1) {
            ++errors;
        }

        if (NULL == getenv("MAKELEVEL")) {
            printf("Pi from %llu points on %d PEs: %lf\n",total,myshmem_n_pes,approx_pi);
        }
    }

    shmem_finalize();

    return errors;
}

