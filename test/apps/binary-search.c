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
#include <shmem.h>

#define N_PER_PE 10

/* N_PER_PE*N_PES sorted, distributed shared array */
int keys[N_PER_PE];

static int binary_search(int key) {
    int low, mid, high;

    low = 0;
    high = shmem_n_pes()*N_PER_PE;

    while(low < high) {
        int val;

        mid = low + (high-low)/2;
        val = shmem_int_g(&keys[mid%N_PER_PE], mid/N_PER_PE);

        if(val == key) {
            return mid;
        } else if(val < key) {
            low = mid;
        } else {
            high = mid;
        }
    }

    return -1;
}

int main(int argc, char **argv) {
    int i, errors = 0;

    shmem_init();

    for (i = 0; i < N_PER_PE; i++)
        keys[i] = N_PER_PE * shmem_my_pe() + i;

    shmem_barrier_all();

    for (i = 0; i < N_PER_PE * shmem_n_pes(); i++) {
        int j = binary_search(i);
        if (j != i) {
            printf("%2d: Error searching for %d.  Found at index %d, expected %d\n",
                   shmem_my_pe(), i, j, i);
            errors++;
        }
    }

    shmem_finalize();

    return errors;
}
