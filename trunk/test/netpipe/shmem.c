/* NOTE: Anywhere a sched_yield() is called, previously there was a busy
 * polling wait on the byte or flag, which caused horrible performance on the
 * machine I tested on (helix).  sched_yield() seemed to fix this issue. 
 */

#include  "netpipe.h"

double *pTime;
int    *pNrepeat;

void Init(ArgStruct *p, int* pargc, char*** pargv)
{

}

void Setup(ArgStruct *p)
{
   int npes;

   start_pes(2);

   if((npes=shmem_n_pes())!=2) {

      printf("Error Message: Run with npes set to 2\n");
      exit(1);
   }

   p->prot.flag=(int *) shmalloc(sizeof(int));
   pTime = (double *) shmalloc(sizeof(double));
   pNrepeat = (int *) shmalloc(sizeof(int));

   p->tr = p->rcv = 0;

   if((p->prot.ipe=_my_pe()) == 0) {
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
   int i=0;

   while(p->r_ptr[p->bufflen-1] != 'a' + (p->cache ? 1 - p->tr : 1) ) {
     sched_yield();
  }

   p->r_ptr[p->bufflen-1] = 'a' + (p->cache ? p->tr : 0);
}

void SendTime(ArgStruct *p, double *t)
{
   *pTime=*t;

   shmem_double_put(pTime,pTime,1,p->prot.nbor);
   shmem_int_put(p->prot.flag,p->prot.flag,1,p->prot.nbor);
}

void RecvTime(ArgStruct *p, double *t)
{
   int i=0;

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
   shmem_int_put(p->prot.flag,p->prot.flag,1,p->prot.nbor);
}

void RecvRepeat(ArgStruct *p, int *rpt)
{
   int i=0;

   while(*p->prot.flag!=p->prot.ipe)
   {
     sched_yield();

   }
   *rpt=*pNrepeat;
   *p->prot.flag=p->prot.nbor;
}

void  CleanUp(ArgStruct *p)
{
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

   if((buff1=(char *)shmalloc(bufflen+MAX(soffset,roffset)))==(char *)NULL)
   {
      fprintf(stderr,"couldn't allocate memory\n");
      exit(-1);
   }

   if(!p->cache)

     if((buff2=(char *)shmalloc(bufflen+soffset))==(char *)NULL)
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
    shfree(buff1);

  if(buff2 != NULL)
    shfree(buff2);
}
