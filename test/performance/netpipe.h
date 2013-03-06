#define FINAL
#undef FINAL
/*****************************************************************************/
/* "NetPIPE" -- Network Protocol Independent Performance Evaluator.          */
/* Copyright 1997, 1998 Iowa State University Research Foundation, Inc.      */
/*                                                                           */
/* This program is free software; you can redistribute it and/or modify      */
/* it under the terms of the GNU General Public License as published by      */
/* the Free Software Foundation.  You should have received a copy of the     */
/* GNU General Public License along with this program; if not, write to the  */
/* Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.   */
/*                                                                           */
/*     * netpipe.h          ---- General include file                        */
/*****************************************************************************/
#include <ctype.h>
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>       /* struct timeval */
#include <sys/resource.h>   /* getrusage() */
#include <stdlib.h>         /* malloc(3) */
#include <unistd.h>         /* getopt, read, write, ... */

/* Handle the case of building on MacOS X */
#if defined(__APPLE__)
#include <stdint.h>
#endif 

#ifdef INFINIBAND
#include <ib_defs.h> /* ib_mtu_t */
#endif

#ifdef OPENIB
#include <infiniband/verbs.h>	/* enum ibv_mtu */
#endif

#ifdef FINAL
  #define  TRIALS             7
  #define  RUNTM              0.25
#else
  #define  TRIALS             3
  #define  RUNTM              0.10
#endif

#define  MEMSIZE            10000000 
#define  DEFPORT            5002
#define  NSAMP              8000
#define  DEFPERT            3
#define  LONGTIME           1e99
#define  CHARSIZE           8
#define  STOPTM             1.0
#define  MAXINT             10 000 000
/*#define  MAXINT             1048576*/

#define     ABS(x)     (((x) < 0)?(-(x)):(x))
#define     MIN(x,y)   (((x) < (y))?(x):(y))
#define     MAX(x,y)   (((x) > (y))?(x):(y))

/* Need to include the protocol structure header file.                       */
/* Change this to reflect the protocol                                       */

#if defined(TCP)
  #include <netdb.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netinet/tcp.h>
  #include <arpa/inet.h>
  
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct
  {
      struct sockaddr_in      sin1,   /* socket structure #1              */
                              sin2;   /* socket structure #2              */
      int                     nodelay;  /* Flag for TCP nodelay           */
      struct hostent          *addr;    /* Address of host                */
      int                     sndbufsz, /* Size of TCP send buffer        */
                              rcvbufsz; /* Size of TCP receive buffer     */
#if defined(INFINIBAND)
      IB_mtu_t                ib_mtu;   /* MTU Size for Infiniband HCA    */
      int                     commtype; /* Communications type            */
      int                     comptype; /* Completion type                */
#endif
#if defined(OPENIB)
      enum ibv_mtu            ib_mtu;   /* MTU Size for Infiniband HCA    */
      int                     commtype; /* Communications type            */
      int                     comptype; /* Completion type                */
      char                    *device_and_port; /* Local port specification */
#endif
  };

#if defined(INFINIBAND) || defined(OPENIB)
enum completion_types {
   NP_COMP_LOCALPOLL,  /* Poll locally on last byte of data     */
   NP_COMP_VAPIPOLL,   /* Poll using vapi function              */
   NP_COMP_EVENT       /* Don't poll, use vapi event completion */
};
enum communication_types {
   NP_COMM_SENDRECV,           /* Communication with send/receive            */
   NP_COMM_SENDRECV_WITH_IMM,  /* Communication with send/receive & imm data */
   NP_COMM_RDMAWRITE,          /* Communication with rdma write              */
   NP_COMM_RDMAWRITE_WITH_IMM, /* Communication with rdma write & imm data   */
};
#endif

#elif defined(SCTP6)
  #include <netdb.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netinet/sctp.h>
  #include <arpa/inet.h>
  
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct
  {
      struct sockaddr_in6     sin1;   /* socket structure #1              */
      struct sockaddr_in6     sin2;   /* socket structure #2              */
      int                     nodelay;  /* Flag for TCP nodelay           */
      struct hostent          *addr;    /* Address of host                */
      int                     sndbufsz; /* Size of TCP send buffer        */
      int                     rcvbufsz; /* Size of TCP receive buffer     */
  };

#elif defined(SCTP)
  #include <netdb.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netinet/sctp.h>
  #include <arpa/inet.h>
  
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct
  {
      struct sockaddr_in      sin1,   /* socket structure #1              */
                              sin2;   /* socket structure #2              */
      int                     nodelay;  /* Flag for TCP nodelay           */
      struct hostent          *addr;    /* Address of host                */
      int                     sndbufsz, /* Size of TCP send buffer        */
                              rcvbufsz; /* Size of TCP receive buffer     */
  };

#elif defined(TCP6)
  #include <netdb.h>
  #include <sys/socket.h>
  #include <netinet/in.h>
  #include <netinet/tcp.h>
  #include <arpa/inet.h>
  
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct
  {
      struct sockaddr_in6     sin1;   /* socket structure #1              */
      struct sockaddr_in6     sin2;   /* socket structure #2              */
      int                     nodelay;  /* Flag for TCP nodelay           */
      struct hostent          *addr;    /* Address of host                */
      int                     sndbufsz; /* Size of TCP send buffer        */
      int                     rcvbufsz; /* Size of TCP receive buffer     */
  };

#elif defined(IPX)
  #include <netdb.h>
  #include <sys/socket.h>
  #include <netipx/ipx.h>
  
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct
  {
      struct sockaddr_ipx     sipx1;   /* socket structure #1              */
      struct sockaddr_ipx     sipx2;   /* socket structure #2              */
      int                     sndbufsz; /* Size of send buffer        */
      int                     rcvbufsz; /* Size of receive buffer     */
  };

#elif defined(MPI)
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct 
  { 
    int nbor, iproc;
    int use_get;
    int no_fence;
  };

#elif defined(PVM)
  typedef struct protocolstruct ProtocolStruct;

  struct protocolstruct
  {
    int     mytid; /* Keep track of our task id */
    int     othertid; /* Keep track of the other's task id */
  };

/*
  Choose one of the following to determine the type of data
  encoding for the PVM message passing.
  
  DataDefault means that PVM uses XDR encoding which ensures that
  the data can be packed / unpacked across non-homogeneous machines.
  
  If you know that the machines are the same, then you can use DataRaw
  and save some time (DDT - does not seem to help).
  
  DataInPlace means that the data is not copied at pack time, but is
  copied directly from memory at send time (DDT - this helps a lot).

#define PVMDATA     PvmDataDefault
#define PVMDATA     PvmDataRaw
#define PVMDATA     PvmDataInPlace
*/
#define PVMDATA     PvmDataInPlace


#elif defined(TCGMSG)
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct { long nbor, nid; };

#elif defined(LAPI)
  typedef struct protocolstruct ProtocolStruct;   
  struct protocolstruct { int nbor; };

#elif defined(SHMEM)
  #if defined(GPSHMEM)
    #include "gpshmem.h"
  #else
    #include <shmem.h>
  #endif
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct
  {
          int nbor,ipe;
          volatile int *flag;
  };

#elif defined(ARMCI)
    /* basically same as for GPSHMEM */
  double   *pTime;
  int      *pNrepeat;
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct
  {
          int nbor,ipe;
          volatile int *flag;
  };


#elif defined(GM)
  #include "gm.h"
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct
  { 
     int nbor, iproc, num_stokens; 
     unsigned short host_id; /* Host id in routing info of myrinet card */
  };

  struct gm_port *gm_p;
  unsigned long *ltime, *lrpt;
  char *sync, *sync1;

#elif defined(ATOLL)

  #include <atoll.h>
  
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct
  {
      port_id id_self,       /* My port id */
              id_nbor;       /* My neighbor's port id */
  }

#elif defined(MEMCPY)
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct { int nothing; };

#elif defined(DISK)
  typedef struct protocolstruct ProtocolStruct;
  struct protocolstruct {
     char *dfile_name;
     int read;
     char read_type;   /* c-char  d-double  s-stream */
  };

#else
  #error "One of TCP, TCP6, SCTP, SCTP6, IPX, MPI, PVM, TCGMSG, LAPI, SHMEM, ATOLL, MEMCPY, DISK must be defined during compilation"

#endif


typedef struct argstruct ArgStruct;
struct argstruct 
{
    /* This is the common information that is needed for all tests           */
    int      cache;         /* Cache flag, 0 => limit cache, 1=> use cache   */
    char     *host;         /* Name of receiving host                        */

    int      servicefd,     /* File descriptor of the network socket         */
             commfd;        /* Communication file descriptor                 */
    short    port;          /* Port used for connection                      */
    char     *r_buff;       /* Aligned receive buffer                        */
    char     *r_buff_orig;  /* Original unaligned receive buffer             */
#if defined(USE_VOLATILE_RPTR)
    volatile                /* use volatile if polling on buffer in module   */
#endif
    char     *r_ptr;        /* Pointer to current location in send buffer    */
    char     *r_ptr_saved;  /* Pointer for saving value of r_ptr             */
    char     *s_buff;       /* Aligned send buffer                           */
    char     *s_buff_orig;  /* Original unaligned send buffer                */
    char     *s_ptr;        /* Pointer to current location in send buffer    */

    int      bufflen,       /* Length of transmitted buffer                  */
             upper,         /* Upper limit to bufflen                        */
             tr,rcv,        /* Transmit and Recv flags, or maybe neither     */
             bidir,         /* Bi-directional flag                           */
             nbuff;         /* Number of buffers to transmit                 */

    int      source_node;   /* Set to -1 (MPI_ANY_SOURCE) if -z specified    */
    int      preburst;      /* Burst preposted receives before timed runs    */
    int      reset_conn;    /* Reset connection flag                         */
    int      soffset,roffset;
    int      syncflag; /* flag for using sync sends vs. normal sends in MPI mod*/
    int	     use_sdp;       /* Use AF_INET_SDP instead of AF_INET */
    /* Now we work with a union of information for protocol dependent stuff  */
    ProtocolStruct prot;
};

typedef struct data Data;
struct data
{
    double t;
    double bps;
    double variance;
    int    bits;
    int    repeat;
};

double When(void);

void Init(ArgStruct *p, int* argc, char*** argv);

void Setup(ArgStruct *p);

void establish(ArgStruct *p);

void Sync(ArgStruct *p);

void PrepareToReceive(ArgStruct *p);

void SendData(ArgStruct *p);

void RecvData(ArgStruct *p);

void SendTime(ArgStruct *p, double *t);

void RecvTime(ArgStruct *p, double *t);

void SendRepeat(ArgStruct *p, int rpt);

void RecvRepeat(ArgStruct *p, int *rpt);

void FreeBuff(char *buff1, char *buff2);

void CleanUp(ArgStruct *p);

void InitBufferData(ArgStruct *p, int nbytes, int soffset, int roffset);

void MyMalloc(ArgStruct *p, int bufflen, int soffset, int roffset);

void Reset(ArgStruct *p);

void mymemset(int *ptr, int c, int n);

void flushcache(int *ptr, int n);

void SetIntegrityData(ArgStruct *p);

void VerifyIntegrity(ArgStruct *p);

void* AlignBuffer(void* buff, int boundary);

void AdvanceSendPtr(ArgStruct* p, int blocksize);

void AdvanceRecvPtr(ArgStruct* p, int blocksize);

void SaveRecvPtr(ArgStruct* p);

void ResetRecvPtr(ArgStruct* p);

void PrintUsage(void);

int getopt( int argc, char * const argv[], const char *optstring);

void AfterAlignmentInit( ArgStruct *p );
