/*
 *
 * Copyright (c) 2011, 2012 
 *   University of Houston System and Oak Ridge National Laboratory.
 * 
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 
 * o Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * 
 * o Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * 
 * o Neither the name of the University of Houston System, Oak Ridge
 *   National Laboratory nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 * HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 * TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */


/* Passive Target Progress test */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/time.h>
#include <getopt.h>
#include <string.h>
#include <assert.h>

#include <shmem.h>

#ifdef __GNUC__
int pe_escape = 1;
#define mb() __sync_synchronize()
#else
/* this isn't entirely right, may need to fill in your compiler or
   platform's memory barrier */
volatile int pe_escape = 1;
#endif

int A = 0;

int
main(int argc, char **argv)
{
     int me, npes;
     struct timeval now;
     long t_start, t_end;

     start_pes(0);
     me = _my_pe();
     npes = _num_pes();

     if (npes < 4) {
          if (me==0)
               fprintf(stderr,"ERR: test requires 4 or more PEs\n");
          return 77;
     }
     shmem_barrier_all();

     gettimeofday(&now, NULL);
     t_start = (now.tv_sec * 1000000.0) + now.tv_usec;

     switch (me) {
     case 0:
          while (pe_escape) {
               double pi, pi2, pi3;
               int j;

               for (j=1; j <= 5000; j++) {
                    pi = (22.0 / 7.0) + (double) j;
                    pi2 = pi * (double) j;
                    pi3 = (pi2 * pi) / 1.2;
               }
               mb();
          }
          gettimeofday(&now, NULL);
          t_end = ((now.tv_sec * 1000000.0) + now.tv_usec) - t_start;
          break;

     case 1:
          shmem_int_inc(&A, 0);
          gettimeofday(&now, NULL);
          t_end = ((now.tv_sec * 1000000.0) + now.tv_usec) - t_start;
          break;

     case 2:
          while (1 != shmem_int_g(&A, 0)) { ; }
          shmem_int_inc(&A, 0);
          gettimeofday(&now, NULL);
          t_end = ((now.tv_sec * 1000000.0) + now.tv_usec) - t_start;
          break;

     case 3:
          while (2 != shmem_int_g(&A, 0)) { ; }
          shmem_int_p((int*) &pe_escape, 0, 0);  // release PE0.
          if (npes > 4) {
               int i;

               for(i=4; i < npes; i++)
                    shmem_int_p((int*)&pe_escape, 0, i);  // release PE0.
          }
          gettimeofday(&now, NULL);
          t_end = ((now.tv_sec * 1000000.0) + now.tv_usec) - t_start;
          break;

     default:
          /* spin until released, A will never == 99, generate PE-0 traffic */
          while (99 != shmem_int_g(&A, 0) && pe_escape) {
               mb();
          }
          gettimeofday(&now, NULL);
          t_end = ((now.tv_sec * 1000000.0) + now.tv_usec) - t_start;
          break;
     }

     if (me < 4)
          fprintf(stderr,"[%d] elapsed usecs %ld A %d\n",me,t_end,A);

     shmem_barrier_all();

     return 0;
}
