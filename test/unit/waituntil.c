/*
 * Copyright 2011 Sandia Corporation. Under the terms of Contract
 * DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
 * retains certain rights in this software.
 *
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

/*
 * exercise shmem_short_wait() and shmem_short_wait_until() functions.
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include <shmem.h>

#define DataType long

#define SHM_PUT   shmem_long_put
#define SHM_PUTP  shmem_long_p
#define SHM_GETP  shmem_long_g

#define SHM_WAITU shmem_long_wait_until
#define PF "%ld"

#define Vprintf if (Verbose) printf

int
main(int argc, char* argv[])
{
    DataType source[10] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    static DataType target[10];
    static DataType pong=666;
    DataType *t2=NULL;
    int me, num_pes, pe, Verbose=0;

    if (argc > 1 && (strcmp(argv[1],"-v") == 0)) {
        Verbose++;
    }

    shmem_init();
    me = shmem_my_pe();
    num_pes = shmem_n_pes();

    if (num_pes == 1) {
        printf("%s: Requires number of PEs > 1\n", argv[0]);
        shmem_finalize();
        return 0;
    }

    t2 = shmem_malloc(10*sizeof(DataType));
    if (!t2) {
        if (me==0) printf("shmem_malloc() failed?\n");
        shmem_global_exit(1);
    }
    t2[9] = target[9] = 0xFF;

    shmem_barrier_all();

    if (me == 0) {
        memset(target, 0, sizeof(target));
        for(pe=1; pe < num_pes; pe++)
            SHM_PUT(target, target, 10, pe);

        for(pe=1; pe < num_pes; pe++) /* put 10 elements into target on PE 1 */
            SHM_PUT(target, source, 10, pe);

        SHM_WAITU( &pong, SHMEM_CMP_GT, 666 );
        Vprintf("PE[%d] pong now "PF"\n",me,pong);

        for(pe=1; pe < num_pes; pe++) /* put 1 element into t2 on PE 1 */
            SHM_PUTP(&t2[9], 0xDD, pe);
    }
    else {
        /* wait for 10th element write of 'target' */
        SHM_WAITU( &target[9], SHMEM_CMP_NE, 0xFF );
        Vprintf("PE[%d] target[9] was 255 now "PF", success.\n",me,target[9]);

        SHM_WAITU( &target[9], SHMEM_CMP_EQ, 10 );
        Vprintf("PE[%d] expected target[9] == 10 now "PF"\n",me,target[9]);

        if (me == 1) {
            if (Verbose) {
                DataType tmp = SHM_GETP( &pong, 0);
                printf("PE[%d] @ PE[0] pong == "PF", setting to 999\n",me,tmp);
            }
            SHM_PUTP( &pong, 999, 0);
        }

        SHM_WAITU( &t2[9], SHMEM_CMP_NE, 0xFF );
    }

    //shmem_barrier_all();  /* sync sender and receiver */

    if (me != 0) {
        if (memcmp(source, target, sizeof(DataType) * 10) != 0) {
            int i;
            fprintf(stderr,"[%d] Src & Target mismatch?\n",me);
            for (i = 0 ; i < 10 ; ++i) {
                printf(PF","PF" ", source[i], target[i]);
            }
            printf("\n");
            shmem_global_exit(1);
        }
    }
    shmem_free(t2);

    if (Verbose)
        fprintf(stderr,"[%d] exit\n",shmem_my_pe());

    shmem_finalize();
    return 0;
}
