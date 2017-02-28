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

/* NOTE: Anywhere a sched_yield() is called, previously there was a busy
 * polling wait on the byte or flag, which caused horrible performance on the
 * machine I tested on (helix).  sched_yield() seemed to fix this issue. 
 */

#include <sched.h>

double *pTime;
int    *pNrepeat;

void Init(ArgStruct *p, int* pargc, char*** pargv)
{
   shmem_init();
}

void Setup(ArgStruct *p)
{
   int npes;

   if((npes=shmem_n_pes())!=2) {

      printf("Error Message: Run with npes set to 2\n");
      exit(1);
   }

   p->prot.flag=(int *) shmem_malloc(sizeof(int));
   pTime = (double *) shmem_malloc(sizeof(double));
   pNrepeat = (int *) shmem_malloc(sizeof(int));

   p->tr = p->rcv = 0;

   if((p->prot.ipe=shmem_my_pe()) == 0) {
      p->tr=1;
      p->prot.nbor=1;
      *p->prot.flag=1;

   } else {

      p->rcv=1;
      p->prot.nbor=0;
      *p->prot.flag=0;
   }
}

void Sync(ArgStruct *p)
{
   shmem_barrier_all();
}

void PrepareToReceive(ArgStruct *p) { }

void SendData(ArgStruct *p)
{
   if(p->bufflen%8==0)
      shmem_put64(p->s_ptr,p->s_ptr,p->bufflen/8,p->prot.nbor);
   else
      shmem_putmem(p->s_ptr,p->s_ptr,p->bufflen,p->prot.nbor);
}

void RecvData(ArgStruct *p)
{
   while(p->r_ptr[p->bufflen-1] != 'a' + (p->cache ? 1 - p->tr : 1) ) {
     sched_yield();
  }

   p->r_ptr[p->bufflen-1] = 'a' + (p->cache ? p->tr : 0);
}

void SendTime(ArgStruct *p, double *t)
{
   *pTime=*t;

   shmem_double_put(pTime,pTime,1,p->prot.nbor);
   shmem_int_put((int*) p->prot.flag, (int*) p->prot.flag,1,p->prot.nbor);
}

void RecvTime(ArgStruct *p, double *t)
{
   while(*p->prot.flag!=p->prot.ipe)
   {
     sched_yield();
   }
   *t=*pTime; 
   *p->prot.flag=p->prot.nbor;
}

void SendRepeat(ArgStruct *p, int rpt)
{
   *pNrepeat= rpt;

   shmem_int_put(pNrepeat,pNrepeat,1,p->prot.nbor);
   shmem_int_put((int*) p->prot.flag,(int*) p->prot.flag,1,p->prot.nbor);
}

void RecvRepeat(ArgStruct *p, int *rpt)
{
   while(*p->prot.flag!=p->prot.ipe)
   {
     sched_yield();

   }
   *rpt=*pNrepeat;
   *p->prot.flag=p->prot.nbor;
}

void  CleanUp(ArgStruct *p)
{
    shmem_finalize();
}


void Reset(ArgStruct *p)
{

}

void AfterAlignmentInit(ArgStruct *p)
{

}

void MyMalloc(ArgStruct *p, int bufflen, int soffset, int roffset)
{
   void* buff1;
   void* buff2;

   if((buff1=(char *)shmem_malloc(bufflen+MAX(soffset,roffset)))==(char *)NULL)
   {
      fprintf(stderr,"couldn't allocate memory\n");
      exit(-1);
   }

   if(!p->cache)

     if((buff2=(char *)shmem_malloc(bufflen+soffset))==(char *)NULL)
       {
         fprintf(stderr,"Couldn't allocate memory\n");
         exit(-1);
       }

   if(p->cache) {
     p->r_buff = buff1;
   } else { /* Flip-flop buffers so send <--> recv between nodes */
     p->r_buff = p->tr ? buff1 : buff2;
     p->s_buff = p->tr ? buff2 : buff1;
   }

}
void FreeBuff(char *buff1, char* buff2)
{
  if(buff1 != NULL)
    shmem_free(buff1);

  if(buff2 != NULL)
    shmem_free(buff2);
}
