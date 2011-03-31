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
/* Files needed for use:                                                     */
/*     * netpipe.c       ---- Driver source                                  */
/*     * netpipe.h       ---- General include file                           */
/*     * tcp.c           ---- TCP calls source                               */
/*     * tcp.h           ---- Include file for TCP calls and data structs    */
/*     * mpi.c           ---- MPI calls source                               */
/*     * pvm.c           ---- PVM calls source                               */
/*     * pvm.h           ---- Include file for PVM calls and data structs    */
/*     * tcgmsg.c        ---- TCGMSG calls source                            */
/*     * tcgmsg.h        ---- Include file for TCGMSG calls and data structs */
/*****************************************************************************/

#include "netpipe.h"

#if defined(MPLITE)
#include "mplite.h" /* Included for the malloc wrapper to protect from */
#endif


extern char *optarg;

int main(int argc, char **argv)
{
    FILE        *out;           /* Output data file                          */
    char        s[255],s2[255],delim[255],*pstr; /* Generic strings          */
    int         *memcache = NULL; /* used to flush cache                       */

    int         len_buf_align = 0;  /* meaningful when args.cache is 0. buflen   */
                                /* rounded up to be divisible by 8           */

    int         c,              /* option index                              */
                i, j, n, nq,    /* Loop indices                              */
                asyncReceive=0, /* Pre-post a receive buffer?                */
                bufalign=16*1024,/* Boundary to align buffer to              */
                errFlag,        /* Error occurred in inner testing loop      */
                nrepeat,        /* Number of time to do the transmission     */
                nrepeat_const=0,/* Set if we are using a constant nrepeat    */
                len,            /* Number of bytes to be transmitted         */
                inc=0,          /* Increment value                           */
                perturbation=DEFPERT, /* Perturbation value                  */
                pert,
                start= 1,       /* Starting value for signature curve        */
                end=32,     /* Ending value for signature curve          */
                streamopt=0,    /* Streaming mode flag                       */
		debug_wait=0;	/* spin and wait for a debugger		     */
   
    ArgStruct   args;           /* Arguments for all the calls               */

    double      t, t0,   /* Time variables                            */
        tlast;          /* Time for the last transmission            */

    Data        bwdata[NSAMP];  /* Bandwidth curve data                      */

    int         integCheck=0;   /* Integrity check                           */

    /* Initialize vars that may change from default due to arguments */

    strcpy(s, "np.out");   /* Default output file */

    /* Let modules initialize related vars, and possibly call a library init
       function that requires argc and argv */


    Init(&args, &argc, &argv);   /* This will set args.tr and args.rcv */

    args.preburst = 0; /* Default to not bursting preposted receives */
    args.bidir = 0; /* Turn bi-directional mode off initially */
    args.cache = 1; /* Default to use cache */
    args.upper = end;
    args.host  = NULL;
    args.soffset=0; /* default to no offsets */
    args.roffset=0; 
    args.syncflag=0; /* use normal mpi_send */
    args.use_sdp=0; /* default to no SDP */
    args.port = DEFPORT; /* just in case the user doesn't set this. */


    /* TCGMSG launches NPtcgmsg with a -master master_hostname
     * argument, so ignore all arguments and set them manually 
     * in netpipe.c instead.
     */

#if ! defined(TCGMSG)

    /* Parse the arguments. See Usage for description */
    while ((c = getopt(argc, argv, "AXSO:rIiPszgfaB2h:p:o:l:u:b:m:n:t:c:d:D:P:")) != -1)
    {
        switch(c)
        {
	    case 'A':
		      args.use_sdp=1;
		      break;
            case 'O':
                      strcpy(s2,optarg);
                      strcpy(delim,",");
                      if((pstr=strtok(s2,delim))!=NULL) {
                         args.soffset=atoi(pstr);
                         if((pstr=strtok((char *)NULL,delim))!=NULL)
                            args.roffset=atoi(pstr);
                         else /* only got one token */
                            args.roffset=args.soffset;
                      } else {
                         args.soffset=0; args.roffset=0;
                      }
                      printf("Transmit buffer offset: %d\nReceive buffer offset: %d\n",args.soffset,args.roffset);
                      break;
            case 'p': perturbation = atoi(optarg);
                      if( perturbation > 0 ) {
                         printf("Using a perturbation value of %d\n\n", perturbation);
                      } else {
                         perturbation = 0;
                         printf("Using no perturbations\n\n");
                      }
                      break;

            case 'B': if(integCheck == 1) {
                        fprintf(stderr, "Integrity check not supported with prepost burst\n");
                        exit(-1);
                      }
                      args.preburst = 1;
                      asyncReceive = 1;
                      printf("Preposting all receives before a timed run.\n");
                      printf("Some would consider this cheating,\n");
                      printf("but it is needed to match some vendor tests.\n"); fflush(stdout);
                      break;

            case 'I': args.cache = 0;
                      printf("Performance measured without cache effects\n\n"); fflush(stdout);
                      break;

            case 'o': strcpy(s,optarg);
                      printf("Sending output to %s\n", s); fflush(stdout);
                      break;

            case 's': streamopt = 1;
                      printf("Streaming in one direction only.\n\n");
#if defined(TCP) && ! defined(INFINIBAND) && !defined(OPENIB)
                      printf("Sockets are reset between trials to avoid\n");
                      printf("degradation from a collapsing window size.\n\n");
#endif
                      args.reset_conn = 1;
                      printf("Streaming does not provide an accurate\n");
                      printf("measurement of the latency since small\n");
                      printf("messages may get bundled together.\n\n");
                      if( args.bidir == 1 ) {
                        printf("You can't use -s and -2 together\n");
                        exit(0);
                      }
                      fflush(stdout);
                      break;

            case 'l': start = atoi(optarg);
                      if (start < 1)
                      {
                        fprintf(stderr,"Need a starting value >= 1\n");
                        exit(0);
                      }
                      break;

            case 'u': end = atoi(optarg);
                      break;

#if defined(TCP) && ! defined(INFINIBAND) && !defined(OPENIB)
            case 'b': /* -b # resets the buffer size, -b 0 keeps system defs */
                      args.prot.sndbufsz = args.prot.rcvbufsz = atoi(optarg);
                      break;
#endif

            case '2': args.bidir = 1;    /* Both procs are transmitters */
                         /* end will be maxed at sndbufsz+rcvbufsz */
                      printf("Passing data in both directions simultaneously.\n");
                      printf("Output is for the combined bandwidth.\n");
#if defined(TCP) && ! defined(INFINIBAND) && !defined(OPENIB)
                      printf("The socket buffer size limits the maximum test size.\n\n");
#endif
                      if( streamopt ) {
                        printf("You can't use -s and -2 together\n");
                        exit(0);
                      }
                      break;

            case 'h': args.tr = 1;       /* -h implies transmit node */
                      args.rcv = 0;
                      args.host = (char *)malloc(strlen(optarg)+1);
                      strcpy(args.host, optarg);
                      break;

#ifdef DISK
            case 'd': args.tr = 1;      /* -d to specify input/output file */
                      args.rcv = 0;
                      args.prot.read = 0;
                      args.prot.read_type = 'c';
                      args.prot.dfile_name = (char *)malloc(strlen(optarg)+1);
                      strcpy(args.prot.dfile_name, optarg);
                      break;

            case 'D': if( optarg[0] == 'r' )
                         args.prot.read = 1;
                      else
                         args.prot.read = 0;
                      args.prot.read_type = optarg[1];
                      break;
#endif

            case 'i': if(args.preburst == 1) {
                        fprintf(stderr, "Integrity check not supported with prepost burst\n");
                        exit(-1);
                      }
                      integCheck = 1;
                      perturbation = 0;
                      start = sizeof(int)+1; /* Start with integer size */
                      printf("Doing an integrity check instead of measuring performance\n"); fflush(stdout);
                      break;

#if defined(MPI)
            case 'z': args.source_node = -1;
                      printf("Receive using the ANY_SOURCE flag\n"); fflush(stdout);
                      break;

            case 'a': asyncReceive = 1;
                      printf("Preposting asynchronous receives\n"); fflush(stdout);
                      break;

            case 'S': args.syncflag=1;
                      fprintf(stderr,"Using synchronous sends\n");
                      break;
#endif
#if defined(MPI2)
            case 'g': if(args.prot.no_fence == 1) {
                        fprintf(stderr, "-f cannot be used with -g\n");
                        exit(-1);
                      } 
                      args.prot.use_get = 1;
                      printf("Using MPI-2 Get instead of Put\n");
                      break;

            case 'f': if(args.prot.use_get == 1) {
                         fprintf(stderr, "-f cannot be used with -g\n");
                         exit(-1);
                      }
                      args.prot.no_fence = 1;
                      bufalign = 0;
                      printf("Buffer alignment off (Required for no fence)\n");
                      break;
#endif /* MPI2 */

#if defined(INFINIBAND)
            case 'm': switch(atoi(optarg)) {
                        case 256: args.prot.ib_mtu = MTU256;
                          break;
                        case 512: args.prot.ib_mtu = MTU512;
                          break;
                        case 1024: args.prot.ib_mtu = MTU1024;
                          break;
                        case 2048: args.prot.ib_mtu = MTU2048;
                          break;
                        case 4096: args.prot.ib_mtu = MTU4096;
                          break;
                        default: 
                          fprintf(stderr, "Invalid MTU size, must be one of "
                                          "256, 512, 1024, 2048, 4096\n");
                          exit(-1);
                      }
                      break;
#endif

#if defined(OPENIB)
            case 'm': switch(atoi(optarg)) {
                        case 256: args.prot.ib_mtu = IBV_MTU_256;
                          break;
                        case 512: args.prot.ib_mtu = IBV_MTU_512;
                          break;
                        case 1024: args.prot.ib_mtu = IBV_MTU_1024;
                          break;
                        case 2048: args.prot.ib_mtu = IBV_MTU_2048;
                          break;
                        case 4096: args.prot.ib_mtu = IBV_MTU_4096;
                          break;
                        default: 
                          fprintf(stderr, "Invalid MTU size, must be one of "
                                          "256, 512, 1024, 2048, 4096\n");
                          exit(-1);
                      }
                      break;
#endif

#if defined(OPENIB)
            case 'D': args.prot.device_and_port = strdup(optarg);
                      break;
#endif

#if defined(OPENIB) || defined(INFINIBAND)
            case 't': if( !strcmp(optarg, "send_recv") ) {
                         printf("Using Send/Receive communications\n");
                         args.prot.commtype = NP_COMM_SENDRECV;
                      } else if( !strcmp(optarg, "send_recv_with_imm") ) {
                         printf("Using Send/Receive communications with immediate data\n");
                         args.prot.commtype = NP_COMM_SENDRECV_WITH_IMM;
                      } else if( !strcmp(optarg, "rdma_write") ) {
                         printf("Using RDMA Write communications\n");
                         args.prot.commtype = NP_COMM_RDMAWRITE;
                      } else if( !strcmp(optarg, "rdma_write_with_imm") ) {
                         printf("Using RDMA Write communications with immediate data\n");
                         args.prot.commtype = NP_COMM_RDMAWRITE_WITH_IMM;
                      } else {
                         fprintf(stderr, "Invalid transfer type "
                                 "specified, please choose one of:\n\n"
                                 "\tsend_recv\t\tUse Send/Receive communications\t(default)\n"
                                 "\tsend_recv_with_imm\tSame as above with immediate data\n"
                                 "\trdma_write\t\tUse RDMA Write communications\n"
                                 "\trdma_write_with_imm\tSame as above with immediate data\n\n");
                         exit(-1);
                      }
                      break;

            case 'c': if( !strcmp(optarg, "local_poll") ) {
                         printf("Using local polling completion\n");
                         args.prot.comptype = NP_COMP_LOCALPOLL;
                      } else if( !strcmp(optarg, "vapi_poll") ) {
                         printf("Using VAPI polling completion\n");
                         args.prot.comptype = NP_COMP_VAPIPOLL;
                      } else if( !strcmp(optarg, "event") ) {
                         printf("Using VAPI event completion\n");
                         args.prot.comptype = NP_COMP_EVENT;
                      } else {
                         fprintf(stderr, "Invalid completion type specified, "
                                 "please choose one of:\n\n"
                                 "\tlocal_poll\tWait for last byte of data\t(default)\n"
                                 "\tvapi_poll\tUse VAPI polling function\n"
                                 "\tevent\t\tUse VAPI event handling function\n\n");
                         exit(-1);
                      }
                      break;
#endif
	    case 'P': 
		      args.port = atoi(optarg);
		      break;

            case 'n': nrepeat_const = atoi(optarg);
                      break;

#if defined(TCP) && ! defined(INFINIBAND) && !defined(OPENIB)
            case 'r': args.reset_conn = 1;
                      printf("Resetting connection after every trial\n");
                      break;
#endif
	    case 'X': debug_wait = 1;
		      printf("Enableing debug wait!\n");
		      printf("Attach to pid %d and set debug_wait to 0 to conttinue\n", getpid());
		      break;

            default: 
                     PrintUsage(); 
                     exit(-12);
       }
   }

   while(debug_wait){
	   for(i=0;i<10000;i++){};
   	};
#endif /* ! defined TCGMSG */

#if defined(OPENIB) || defined(INFINIBAND)
   asyncReceive = 1;
   fprintf(stderr, "Preposting asynchronous receives (required for Infiniband)\n");
   if(args.bidir && (
          (args.cache && args.prot.commtype == NP_COMM_RDMAWRITE) || /* rdma_write only works with no-cache mode */
          (!args.preburst && args.prot.commtype != NP_COMM_RDMAWRITE) || /* anything besides rdma_write requires prepost burst */
          (args.preburst && args.prot.comptype == NP_COMP_LOCALPOLL && args.cache) || /* preburst with local polling in cache mode doesn't work */
          0)) {

      fprintf(stderr, 
         "\n"
         "Bi-directional mode currently only works with a subset of the\n"
         "Infiniband options. Restrictions are:\n"
         "\n"
         "  RDMA write (-t rdma_write) requires no-cache mode (-I).\n"
         "\n"
         "  Local polling (-c local_poll, default if no -c given) requires\n"
         "    no-cache mode (-I), and if not using RDMA write communication,\n"
         "    burst mode (-B).\n"
         "\n"
         "  Any other communication type and any other completion type\n"
         "    require burst mode (-B). No-cache mode (-I) may be used\n"
         "    optionally.\n"
         "\n"
         "  All other option combinations will fail.\n"
         "\n");
               
      exit(-1);      

   }
#endif

   if (start > end)
   {
       fprintf(stderr, "Start MUST be LESS than end\n");
       exit(420132);
   }
   args.nbuff = TRIALS;

   Setup(&args);

   if( args.bidir && end > args.upper ) {
      end = args.upper;
      if( args.tr ) {
         printf("The upper limit is being set to %d Bytes\n", end);
#if defined(TCP) && ! defined(INFINIBAND) && !defined(OPENIB)
         printf("due to socket buffer size limitations\n\n");
#endif
   }  }

#if defined(GM)

   if(streamopt && (!nrepeat_const || nrepeat_const > args.prot.num_stokens)) {
     printf("\nGM is currently limited by the driver software to %d\n", 
            args.prot.num_stokens);
     printf("outstanding sends. The number of repeats will be set\n");
     printf("to this limit for every trial in streaming mode.  You\n");
     printf("may use the -n switch to set a smaller number of repeats\n\n");

     nrepeat_const = args.prot.num_stokens;
   }

#endif

   if( args.tr )                     /* Primary transmitter */
   {
       if ((out = fopen(s, "w")) == NULL)
       {
           fprintf(stderr,"Can't open %s for output\n", s);
           exit(1);
       }
   }
   else out = stdout;

      /* Set a starting value for the message size increment. */

   inc = (start > 1) ? start / 2 : 1;
   nq = (start > 1) ? 1 : 0;

      /* Test the timing to set tlast for the first test */

   args.bufflen = start;
   MyMalloc(&args, args.bufflen, 0, 0);
   InitBufferData(&args, args.bufflen, 0, 0);

   if(args.cache) args.s_buff = args.r_buff;
   
   args.r_ptr = args.r_buff_orig = args.r_buff;
   args.s_ptr = args.s_buff_orig = args.s_buff;
      
   AfterAlignmentInit(&args);  /* MPI-2 needs this to create a window */

   /* Infiniband requires use of asynchronous communications, so we need
    * the PrepareToReceive calls below
    */
   if( asyncReceive )
      PrepareToReceive(&args);
   
   Sync(&args);    /* Sync to prevent race condition in armci module */

   /* For simplicity's sake, even if the real test below will be done in
    * bi-directional mode, we still do the ping-pong one-way-at-a-time test
    * here to estimate the one-way latency. Unless it takes significantly
    * longer to send data in both directions at once than it does to send data
    * one way at a time, this shouldn't be too far off anyway.
    */
   t0 = When();
      for( n=0; n<100; n++) {
         if( args.tr) {
            SendData(&args);
            RecvData(&args);
            if( asyncReceive && n<99 )
               PrepareToReceive(&args);
         } else if( args.rcv) {
            RecvData(&args);
            if( asyncReceive && n<99 )
               PrepareToReceive(&args);
            SendData(&args);
         }
      }
   tlast = (When() - t0)/200;

   /* Sync up and Reset before freeing the buffers */

   Sync(&args); 

   Reset(&args);
   
   /* Free the buffers and any other module-specific resources. */
   if(args.cache)
      FreeBuff(args.r_buff_orig, NULL);
   else
      FreeBuff(args.r_buff_orig, args.s_buff_orig);

      /* Do setup for no-cache mode, using two distinct buffers. */

   if (!args.cache)
   {

       /* Allocate dummy pool of memory to flush cache with */

       if ( (memcache = (int *)malloc(MEMSIZE)) == NULL)
       {
           perror("malloc");
           exit(1);
       }
       mymemset(memcache, 0, MEMSIZE/sizeof(int)); 

       /* Allocate large memory pools */

       MyMalloc(&args, MEMSIZE+bufalign, args.soffset, args.roffset); 

       /* Save buffer addresses */
       
       args.s_buff_orig = args.s_buff;
       args.r_buff_orig = args.r_buff;

       /* Align buffers */

       args.s_buff = AlignBuffer(args.s_buff, bufalign);
       args.r_buff = AlignBuffer(args.r_buff, bufalign);

       /* Post alignment initialization */

       AfterAlignmentInit(&args);

       /* Initialize send buffer pointer */
       
/* both soffset and roffset should be zero if we don't have any offset stuff, so this should be fine */
       args.s_ptr = args.s_buff+args.soffset;
       args.r_ptr = args.r_buff+args.roffset;
   }

       /**************************
        * Main loop of benchmark *
        **************************/

   if( args.tr ) fprintf(stderr,"Now starting the main loop\n");

   for ( n = 0, len = start, errFlag = 0; 
        n < NSAMP - 3 && tlast < STOPTM && len <= end && !errFlag; 
        len = len + inc, nq++ )
   {

           /* Exponentially increase the block size.  */

       if (nq > 2) inc = ((nq % 2))? inc + inc: inc;
       
          /* This is a perturbation loop to test nearby values */

       for (pert = ((perturbation > 0) && (inc > perturbation+1)) ? -perturbation : 0;
            pert <= perturbation; 
            n++, pert += ((perturbation > 0) && (inc > perturbation+1)) ? perturbation : perturbation+1)
       {

           Sync(&args);    /* Sync to prevent race condition in armci module */

               /* Calculate how many times to repeat the experiment. */

           if( args.tr )
           {
               if (nrepeat_const) {
                   nrepeat = nrepeat_const;
/*               } else if (len == start) {*/
/*                   nrepeat = MAX( RUNTM/( 0.000020 + start/(8*1000) ), TRIALS);*/
               } else {
                   nrepeat = MAX((RUNTM / ((double)args.bufflen /
                                  (args.bufflen - inc + 1.0) * tlast)),TRIALS);
               }
               SendRepeat(&args, nrepeat);
           }
           else if( args.rcv )
           {
               RecvRepeat(&args, &nrepeat);
           }

           args.bufflen = len + pert;

           if( args.tr )
               fprintf(stderr,"%3d: %7d bytes %6d times --> ",
                       n,args.bufflen,nrepeat);

           if (args.cache) /* Allow cache effects.  We use only one buffer */
           {
               /* Allocate the buffer with room for alignment*/

               MyMalloc(&args, args.bufflen+bufalign, args.soffset, args.roffset); 

               /* Save buffer address */

               args.r_buff_orig = args.r_buff;
               args.s_buff_orig = args.r_buff;

               /* Align buffer */

               args.r_buff = AlignBuffer(args.r_buff, bufalign);
               args.s_buff = args.r_buff;
               
               /* Initialize buffer with data
                *
                * NOTE: The buffers should be initialized with some sort of
                * valid data, whether it is actually used for anything else,
                * to get accurate results.  Performance increases noticeably
                * if the buffers are left uninitialized, but this does not
                * give very useful results as realworld apps tend to actually
                * have data stored in memory.  We are not sure what causes
                * the difference in performance at this time.
                */

               InitBufferData(&args, args.bufflen, args.soffset, args.roffset);


               /* Post-alignment initialization */

               AfterAlignmentInit(&args);

               /* Initialize buffer pointers (We use r_ptr and s_ptr for
                * compatibility with no-cache mode, as this makes the code
                * simpler) 
                */
               /* offsets are zero by default so this saves an #ifdef */
               args.r_ptr = args.r_buff+args.roffset;
               args.s_ptr = args.r_buff+args.soffset;

           }
           else /* Eliminate cache effects.  We use two distinct buffers */
           {

               /* this isn't truly set up for offsets yet */
               /* Size of an aligned memory block including trailing padding */

               len_buf_align = args.bufflen;
               if(bufalign != 0)
                 len_buf_align += bufalign - args.bufflen % bufalign;
 
               /* Initialize the buffers with data
                *
                * See NOTE above.
                */
               InitBufferData(&args, MEMSIZE, args.soffset, args.roffset); 
               

               /* Reset buffer pointers to beginning of pools */
               args.r_ptr = args.r_buff+args.roffset;
               args.s_ptr = args.s_buff+args.soffset;
            }

            bwdata[n].t = LONGTIME;
/*            t2 = t1 = 0;*/

            /* Finally, we get to transmit or receive and time */

            /* NOTE: If a module is running that uses only one process (e.g.
             * memcpy), we assume that it will always have the args.tr flag
             * set.  Thus we make some special allowances in the transmit 
             * section that are not in the receive section.
             */

            if( args.tr || args.bidir )
            {
                /*
                   This is the transmitter: send the block TRIALS times, and
                   if we are not streaming, expect the receiver to return each
                   block.
                */

                for (i = 0; i < (integCheck ? 1 : TRIALS); i++)
                {                    
                    if(args.preburst && asyncReceive && !streamopt)
                    {

                      /* We need to save the value of the recv ptr so
                       * we can reset it after we do the preposts, in case
                       * the module needs to use the same ptr values again
                       * so it can wait on the last byte to change to indicate
                       * the recv is finished.
                       */

                      SaveRecvPtr(&args);

                      for(j=0; j<nrepeat; j++)
                      {
                        PrepareToReceive(&args);
                        if(!args.cache)
                          AdvanceRecvPtr(&args, len_buf_align);
                      }

                      ResetRecvPtr(&args);
                    }

                    /* Flush the cache using the dummy buffer */
                    if (!args.cache)
                      flushcache(memcache, MEMSIZE/sizeof(int));

                    Sync(&args);

                    t0 = When();

                    for (j = 0; j < nrepeat; j++)
                    {
                        if (!args.preburst && asyncReceive && !streamopt)
                        {
                            PrepareToReceive(&args);
                        }

                        if (integCheck) SetIntegrityData(&args);

                        SendData(&args);

                        if (!streamopt)
                        {
                            RecvData(&args);

                            if (integCheck) VerifyIntegrity(&args);

                            if(!args.cache)
                              AdvanceRecvPtr(&args, len_buf_align);

                        }
                        
                        /* Wait to advance send pointer in case RecvData uses
                         * it (e.g. memcpy module).
                         */
                        if (!args.cache)
                          AdvanceSendPtr(&args, len_buf_align);

                    }

                       /* t is the 1-directional trasmission time */

                    t = (When() - t0)/ nrepeat;

                    if( !streamopt && !args.bidir) t /= 2; /* Normal ping-pong */

                    Reset(&args);

/* NOTE: NetPIPE does each data point TRIALS times, bouncing the message
 * nrepeats times for each trial, then reports the lowest of the TRIALS
 * times.  -Dave Turner
 */
                    bwdata[n].t = MIN(bwdata[n].t, t);
/*                    t1 += t;*/
/*                    t2 += t*t;*/
                }

                if (streamopt){  /* Get time info from Recv node */
                    RecvTime(&args, &bwdata[n].t);
/*                    RecvTime(&args, &t1);*/
/*                    RecvTime(&args, &t2);*/
                }

                   /* Calculate variance after completing this set of trials */

/*                bwdata[n].variance = t2/TRIALS - t1/TRIALS * t1/TRIALS;*/

            }
            else if( args.rcv )
            {
                /*
                   This is the receiver: receive the block TRIALS times, and
                   if we are not streaming, send the block back to the
                   sender.
                */
                for (i = 0; i < (integCheck ? 1 : TRIALS); i++)
                {
                    if (asyncReceive)
                    {
                       if (args.preburst)
                       {

                         /* We need to save the value of the recv ptr so
                          * we can reset it after we do the preposts, in case
                          * the module needs to use the same ptr values again
                          * so it can wait on the last byte to change to 
                          * indicate the recv is finished.
                          */

                         SaveRecvPtr(&args);

                         for (j=0; j < nrepeat; j++)
                         {
                              PrepareToReceive(&args);
                              if (!args.cache)
                                 AdvanceRecvPtr(&args, len_buf_align);
                         }
                         
                         ResetRecvPtr(&args);
                         
                       }
                       else
                       {
                           PrepareToReceive(&args);
                       }
                      
                    }
                    
                    /* Flush the cache using the dummy buffer */
                    if (!args.cache)
                      flushcache(memcache, MEMSIZE/sizeof(int));

                    Sync(&args);

                    t0 = When();
                    for (j = 0; j < nrepeat; j++)
                    {
                        RecvData(&args);

                        if (integCheck) VerifyIntegrity(&args);

                        if (!args.cache)
                        { 
                            AdvanceRecvPtr(&args, len_buf_align);
                        }
                        
                        if (!args.preburst && asyncReceive && (j < nrepeat-1))
                        {
                            PrepareToReceive(&args);
                        }

                        if (!streamopt)
                        {
                            if (integCheck) SetIntegrityData(&args);
                            
                            SendData(&args);

                            if(!args.cache) 
                              AdvanceSendPtr(&args, len_buf_align);
                        }

                    }
                    t = (When() - t0)/ nrepeat;

                    if( !streamopt && !args.bidir) t /= 2; /* Normal ping-pong */

                    Reset(&args);
                    
                    bwdata[n].t = MIN(bwdata[n].t, t);
/*                    t1 += t;*/
/*                    t2 += t*t;*/
                }
                if (streamopt){  /* Recv proc calcs time and sends to Trans */
                    SendTime(&args, &bwdata[n].t);
/*                    SendTime(&args, &t1);*/
/*                    SendTime(&args, &t2);*/
                }
            }
            else  /* Just going along for the ride */
            {
                for (i = 0; i < (integCheck ? 1 : TRIALS); i++)
                {
                    Sync(&args);
                }
            }

            /* Streaming mode doesn't really calculate correct latencies
             * for small message sizes, and on some nics we can get
             * zero second latency after doing the math.  Protect against
             * this.
             */
            if(bwdata[n].t == 0.0) {
              bwdata[n].t = 0.000001;
            }
            
            tlast = bwdata[n].t;
            bwdata[n].bits = args.bufflen * CHARSIZE * (1+args.bidir);
            bwdata[n].bps = bwdata[n].bits / (bwdata[n].t * 1024 * 1024);
            bwdata[n].repeat = nrepeat;
            
            if (args.tr)
            {
                if(integCheck) {
                  fprintf(out,"%8d %d", bwdata[n].bits / 8, nrepeat);

                } else {
                  fprintf(out,"%8d %lf %12.8lf",
                        bwdata[n].bits / 8, bwdata[n].bps, bwdata[n].t);

                }
                fprintf(out, "\n");
                fflush(out);
            }
    
            /* Free using original buffer addresses since we may have aligned
               r_buff and s_buff */

            if (args.cache)
                FreeBuff(args.r_buff_orig, NULL);
            
            if ( args.tr ) {
               if(integCheck) {
                 fprintf(stderr, " Integrity check passed\n");

               } else {
                 fprintf(stderr," %8.2lf Mbps in %10.2lf usec\n", 
                         bwdata[n].bps, tlast*1.0e6);
               }
            }


        } /* End of perturbation loop */

    } /* End of main loop  */
 
   /* Free using original buffer addresses since we may have aligned
      r_buff and s_buff */

   if (!args.cache) {
        FreeBuff(args.s_buff_orig, args.r_buff_orig);
   }
    if (args.tr) fclose(out);
         
    CleanUp(&args);
    return 0;
}


/* Return the current time in seconds, using a double precision number.      */
double When()
{
    struct timeval tp;
    gettimeofday(&tp, NULL);
    return ((double) tp.tv_sec + (double) tp.tv_usec * 1e-6);
}

/* 
 * The mymemset() function fills the first n integers of the memory area 
 * pointed to by ptr with the constant integer c. 
 */
void mymemset(int *ptr, int c, int n)  
{
    int i;

    for (i = 0; i < n; i++) 
        *(ptr + i) = c;
}

/* Read the first n integers of the memmory area pointed to by ptr, to flush  
 * out the cache   
 */
void flushcache(int *ptr, int n)
{
   static int flag = 0;
   int    i; 

   flag = (flag + 1) % 2; 
   if ( flag == 0) 
       for (i = 0; i < n; i++)
           *(ptr + i) = *(ptr + i) + 1;
   else
       for (i = 0; i < n; i++) 
           *(ptr + i) = *(ptr + i) - 1; 
    
}

/* For integrity check, set each integer-sized block to the next consecutive
 * integer, starting with the value 0 in the first block, and so on.  Earlier
 * we made sure the memory allocated for the buffer is of size i*sizeof(int) +
 * 1 so there is an extra byte that can be used as a flag to detect the end
 * of a receive.
 */
void SetIntegrityData(ArgStruct *p)
{
  int i;
  int num_segments;

  num_segments = p->bufflen / sizeof(int);

  for(i=0; i<num_segments; i++) {

    *( (int*)p->s_ptr + i ) = i;

  }
}

void VerifyIntegrity(ArgStruct *p)
{
  int i;
  int num_segments;
  int integrityVerified = 1;

  num_segments = p->bufflen / sizeof(int);

  for(i=0; i<num_segments; i++) {

    if( *( (int*)p->r_ptr + i )  != i ) {

      integrityVerified = 0;
      break;

    }

  }


  if(!integrityVerified) {
    
    fprintf(stderr, "Integrity check failed: Expecting %d but received %d\n",
            i, *( (int*)p->r_ptr + i ) );

    /* Dump argstruct */
    /*
    fprintf(stderr, " args struct:\n");
    fprintf(stderr, "  r_buff_orig %p [%c%c%c...]\n", p->r_buff_orig, p->r_buff_orig[i], p->r_buff_orig[i+1], p->r_buff_orig[i+2]);
    fprintf(stderr, "  r_buff      %p [%c%c%c...]\n", p->r_buff,      p->r_buff[i],      p->r_buff[i+1],      p->r_buff[i+2]);
    fprintf(stderr, "  r_ptr       %p [%c%c%c...]\n", p->r_ptr,       p->r_ptr[i],       p->r_ptr[i+1],       p->r_ptr[i+2]);
    fprintf(stderr, "  s_buff_orig %p [%c%c%c...]\n", p->s_buff_orig, p->s_buff_orig[i], p->s_buff_orig[i+1], p->s_buff_orig[i+2]);
    fprintf(stderr, "  s_buff      %p [%c%c%c...]\n", p->s_buff,      p->s_buff[i],      p->s_buff[i+1],      p->s_buff[i+2]);
    fprintf(stderr, "  s_ptr       %p [%c%c%c...]\n", p->s_ptr,       p->s_ptr[i],       p->s_ptr[i+1],       p->s_ptr[i+2]);
    */
    exit(-1);

  }

}  
    
void PrintUsage()
{
    printf("\n NETPIPE USAGE \n\n");
#if ! defined(INFINIBAND) && !defined(OPENIB)
    printf("a: asynchronous receive (a.k.a. preposted receive)\n");
#endif
    printf("B: burst all preposts before measuring performance\n");
#if (defined(TCP) || defined(TCP6)) && ! defined(INFINIBAND)
    printf("b: specify TCP send/receive socket buffer sizes\n");
#endif

#if defined(INFINIBAND) || defined(OPENIB)
    printf("c: specify type of completion <-c type>\n"
           "   valid types: local_poll, vapi_poll, event\n"
           "   default: local_poll\n");
#endif
    
#if defined(MPI2)
    printf("g: use get instead of put\n");
    printf("f: do not use fence during timing segment; may not work with\n");
    printf("   all MPI-2 implementations\n");
#endif

#if defined(TCP) || defined(TCP6) || defined(SCTP) || defined(SCTP6) || defined(INFINIBAND) || defined(OPENIB)
    printf("h: specify hostname of the receiver <-h host>\n");
#endif

    printf("I: Invalidate cache (measure performance without cache effects).\n"
           "   This simulates data coming from main memory instead of cache.\n");
    printf("i: Do an integrity check instead of measuring performance\n");
    printf("l: lower bound start value e.g. <-l 1>\n");

#if defined(INFINIBAND) || defined(OPENIB)
    printf("m: set MTU for Infiniband adapter <-m mtu_size>\n");
    printf("   valid sizes: 256, 512, 1024, 2048, 4096 (default 1024)\n");
#endif

    printf("n: Set a constant value for number of repeats <-n 50>\n");
    printf("o: specify output filename <-o filename>\n");
    printf("O: specify transmit and optionally receive buffer offsets <-O 1,3>\n");
    printf("p: set the perturbation number <-p 1>\n"
           "   (default = 3 Bytes, set to 0 for no perturbations)\n");

#if (defined(TCP) || defined(TCP6) || defined(SCTP) || defined(SCTP6)) && ! defined(INFINIBAND) && !defined(OPENIB)
    printf("r: reset sockets for every trial\n");
#endif

    printf("s: stream data in one direction only.\n");
#if defined(MPI)
    printf("S: Use synchronous sends.\n");
#endif

#if defined(INFINIBAND) || defined(OPENIB)
    printf("t: specify type of communications <-t type>\n"
           "   valid types: send_recv, send_recv_with_imm,\n"
           "                rdma_write, rdma_write_with_imm\n"
           "   defaul: send_recv\n");
#endif
#if defined(OPENIB)
    printf("D: specify an OpenFabrics device/port combination\n"
           "   to use on the local host.  For example:\n"
           "      -D mthca0:1\n"
           "   Uses the first port on the \"mthca0\" device\n"
           "   (NOTE: ports are indexed from 1, not 0)\n"
           "      -D mthca1\n"
           "   Uses the first active port on the mtcha1 device\n"
           "   No specification will result in using the first\n"
           "   active port on any valid device.\n");
#endif
    
    printf("u: upper bound stop value e.g. <-u 1048576>\n");
 
#if defined(MPI)
    printf("z: receive messages using the MPI_ANY_SOURCE flag\n");
#endif

    printf("2: Send data in both directions at the same time.\n");
    printf("P: Set the port number to one other than the default.\n");
#if defined(MPI)
    printf("   May need to use -a to choose asynchronous communications for MPI/n");
#endif
#if (defined(TCP) || defined(TCP6) || defined(SCTP) || defined (SCTP6)) && !defined(INFINIBAND) && !defined(OPENIB)
    printf("   The maximum test size is limited by the TCP buffer size\n");
#endif
#if defined(TCP)
    printf("A: Use SDP Address familty (AF_INET_SDP)\n");
#endif
    printf("\n");
}

void* AlignBuffer(void* buff, int boundary)
{
  if(boundary == 0)
    return buff;
  else
    /* char* typecast required for cc on IRIX */
    return ((char*)buff) + (boundary - ((unsigned long)buff % boundary) );
}

void AdvanceSendPtr(ArgStruct* p, int blocksize)
{
  /* Move the send buffer pointer forward if there is room */

  if(p->s_ptr + blocksize < p->s_buff + MEMSIZE - blocksize)
    
    p->s_ptr += blocksize;

  else /* Otherwise wrap around to the beginning of the aligned buffer */

    p->s_ptr = p->s_buff;
}

void AdvanceRecvPtr(ArgStruct* p, int blocksize)
{
  /* Move the send buffer pointer forward if there is room */

  if(p->r_ptr + blocksize < p->r_buff + MEMSIZE - blocksize)
    
    p->r_ptr += blocksize;

  else /* Otherwise wrap around to the beginning of the aligned buffer */

    p->r_ptr = p->r_buff;
}

void SaveRecvPtr(ArgStruct* p)
{
  /* Typecast prevents warning about loss of volatile qualifier */

  p->r_ptr_saved = (void*)p->r_ptr; 
}

void ResetRecvPtr(ArgStruct* p)
{
  p->r_ptr = p->r_ptr_saved;
}

/* This is generic across all modules */
void InitBufferData(ArgStruct *p, int nbytes, int soffset, int roffset)
{
  memset(p->r_buff, 'a', nbytes+MAX(soffset,roffset));

  /* If using cache mode, then we need to initialize the last byte
   * to the proper value since the transmitter and receiver are waiting
   * on different values to determine when the message has completely
   * arrive.
   */   
  if(p->cache)

    p->r_buff[(nbytes+MAX(soffset,roffset))-1] = 'a' + p->tr;

  /* If using no-cache mode, then we have distinct send and receive
   * buffers, so the send buffer starts out containing different values
   * from the receive buffer
   */
  else

    memset(p->s_buff, 'b', nbytes+soffset);
}
#if !defined(OPENIB) && !defined(INFINIBAND) && !defined(ARMCI) && !defined(LAPI) && !defined(GPSHMEM) && !defined(SHMEM) && !defined(GM) 

void MyMalloc(ArgStruct *p, int bufflen, int soffset, int roffset)
{
    if((p->r_buff=(char *)malloc(bufflen+MAX(soffset,roffset)))==(char *)NULL)
    {
        fprintf(stderr,"couldn't allocate memory for receive buffer\n");
        exit(-1);
    }
       /* if pcache==1, use cache, so this line happens only if flushing cache */
    
    if(!p->cache) /* Allocate second buffer if limiting cache */
      if((p->s_buff=(char *)malloc(bufflen+soffset))==(char *)NULL)
      {
          fprintf(stderr,"couldn't allocate memory for send buffer\n");
          exit(-1);
      }
}

void FreeBuff(char *buff1, char *buff2)
{
  if(buff1 != NULL)

   free(buff1);


  if(buff2 != NULL)

   free(buff2);
}

#endif
