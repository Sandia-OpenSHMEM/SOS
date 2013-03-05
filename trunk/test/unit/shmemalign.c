/* :vim:sw=4:ts=4: */
/*
 *  usage: shmemalign [-v]
 *    where: -v == Verbose display
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <shmem.h>

#define DFLT_NWORDS 257
#define DFLT_INCR 31
#define DFLT_LOOPS 1

#define DataType long

static DataType *target;
static int target_sz;

static char *pgm;
static int Verbose = 0;

void usage (void);
int getSize (char *);

void
usage (void)
{
    if (_my_pe() == 0 ) {
        fprintf (stderr,
            "Usage: %s [-p]  [nWords(%d)] [loops(%d)] [incWords(%d)]\n",
            pgm, DFLT_NWORDS, DFLT_LOOPS, DFLT_INCR);
        fprintf (stderr,
            "  -v == Verbose output\n"
            "  [nWords] # of longs to shmalloc()\n"
            "  [loops]  # of loops\n"
            "  [incWords] nWords += incWords per loop\n");
    }
    exit (1);
}

int
getSize (char *str)
{
    int size;
    char mod[32];

    switch (sscanf (str, "%d%1[mMkK]", &size, mod))
    {
    case 1:
        return (size);

    case 2:
        switch (*mod)
        {
        case 'm':
        case 'M':
            return (size << 20);

        case 'k':
        case 'K':
            return (size << 10);

        default:
            return (size);
        }

    default:
        return (-1);
    }
}

int
main(int argc, char **argv)
{
    int me, nProcs, c, l, j;
    int nWords, loops, incWords;

    pgm = strrchr(argv[0],'/');
    if ( pgm )
        pgm++;
    else
        pgm = argv[0];

    start_pes(0);
    me = _my_pe();
    nProcs = _num_pes();

    while ((c = getopt (argc, argv, "hpv")) != -1)
        switch (c)
        {
        case 'v':
            Verbose++;
            break;
        case 'h':
        default:
            usage();
            break;
        }

    if (optind == argc)
        nWords = DFLT_NWORDS;
    else if ((nWords = getSize (argv[optind++])) <= 0)
        usage ();

    if (optind == argc)
            loops = DFLT_LOOPS;
    else if ((loops = getSize (argv[optind++])) < 0)
        usage ();

    if (optind == argc)
        incWords = DFLT_INCR;
    else if ((incWords = getSize (argv[optind++])) < 0)
        usage ();

    if (Verbose && me == 0)
        fprintf (stderr, "nWords(%d) loops(%d) incWords(%d)]\n",
                 nWords, loops, incWords);

    for(l=0; l < loops; l++)
    {
        /* align 2**2 ... 2**23; 24 exceeds symetric heap max */
        for(j=0,c=2; j < 23; j++,c<<=1)
        {
            target_sz = nWords * sizeof(DataType);
            if (!(target = (DataType *)shmemalign(c,target_sz))) {
                perror ("Failed target memory allocation");
                exit (1);
            }

            if ( (unsigned long)target & (c-1) ) {
                    fprintf(stdout,"PE%d Unaligned? ",me);
                    fflush(stdout);
                    fprintf(stdout,"align[%#09x]target %p\n",
                                        c, (void*)target);
                    exit(1);
            }
            else if (Verbose > 1 && me == 0)
                    fprintf(stdout,"align[%#09x]target %p\n",
                                        c, (void*)target);
            shmem_barrier_all();
            shfree(target);
        }
        nWords += incWords;
        if (Verbose && me == 0)
            fprintf(stdout,"Fini loop %d\n",(l+1));
    }

    shmem_barrier_all();

    return 0;
}
