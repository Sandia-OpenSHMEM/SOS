/*
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 * *	Redistribution and use in source and binary forms, with or
 *	without modification, are permitted provided that the following
 *	conditions are met:
 *
 *	- Redistributions of source code must retain the above
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

#include <pthread.h>

#include <string.h>
#include <stdio.h>

#include <shmem.h>

#define N_THREADS 8
#define N_ELEMS   10

static long source[N_THREADS*N_ELEMS];
static long target[N_THREADS*N_ELEMS];

void* roundrobin(void* tparam) {
    int tid = (int)tparam;
    int offset = tid*N_ELEMS;
    fprintf(stderr,"Starting thread %d with offset %d\n",tid,offset);

    int nextpe = (shmem_my_pe()+1)%shmem_n_pes();
    int prevpe = (shmem_my_pe()-1 + shmem_n_pes())%shmem_n_pes();
    shmem_long_put(target+offset, source+offset, N_ELEMS, nextpe);

    fprintf(stderr,"Thread %d done first put\n",tid);
    shmem_barrier_all();  /* sync sender and receiver */

    shmem_long_get(source+offset, target+offset, N_ELEMS, prevpe);

    fprintf(stderr,"Thread %d done first get\n",tid);
    shmem_barrier_all();  /* sync sender and receiver */

    shmem_long_get(target+offset, source+offset, N_ELEMS, nextpe);

    fprintf(stderr,"Thread %d done second get\n",tid);
    shmem_barrier_all();  /* sync sender and receiver */
    fprintf(stderr,"Done thread %d\n",tid);

    return 0;
}

int
main(int argc, char* argv[])
{
    int i;
    for(i = 0; i < N_THREADS*N_ELEMS; ++i) {
        source[i] = i+1;
    }

    shmem_init();

    if (shmem_n_pes() == 1) {
        printf("%s: Requires number of PEs > 1\n", argv[0]);
        shmem_finalize();
        return 0;
    }

    pthread_t threads[N_THREADS];

    for(i = 0; i < N_THREADS; ++i) {
        pthread_create(&threads[i],NULL,&roundrobin,(void*)i);
    }

    shmem_barrier_all(); /* put 1 */
    shmem_barrier_all(); /* get 1 */
    shmem_barrier_all(); /* get 2, threads end */

    for(i = 0; i < N_THREADS; ++i) {
        pthread_join(threads[i],NULL);
    }

    if (0 != memcmp(source, target, sizeof(long) * N_THREADS*N_ELEMS)) {
        fprintf(stderr,"[%d] Src & Target mismatch?\n",shmem_my_pe());
        for (i = 0 ; i < 10 ; ++i) {
            printf("%ld,%ld ", source[i], target[i]);
        }
        printf("\n");
        shmem_global_exit(1);
    }

    shmem_finalize();

    return 0;
}

