/* :vim:sw=4:ts=4: */
/*
 *  usage: shmalloc [-p] [nWords] [loops] [incWords-per-loop]
 *    where: -p == power-of-two allocation bump per loop
 *      [nWords] # of longs to shmalloc()\n"
 *      [loops(1)]  # of loops\n"
 *      [incWords(2)] nWords += incWords per loop\n");
 * Loop:
 *  PE* shmalloc(nWords)
 *   set *DataType = 1
 *  PE* shmalloc(nWords)
 *   set *DataType = 2
 *  PE* shmalloc(nWords)
 *   set *DataType = 3
 *
 *  for(1...3) allocated ranges
 *    verify
 *    shfree()
 * end-loop
 */

#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>

#include <shmem.h>

#define DFLT_NWORDS 32
#define DFLT_INCR 1025
#define DFLT_LOOPS 50

#define DataType long

static DataType *source;
static DataType *target;
static DataType *result;

static int source_sz;
static int target_sz;
static int result_sz;

static char *pgm;

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
            "  -p  == (2**0 ... 2**22) shmalloc(), other args ignored\n"
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
    int me, nProcs, c, l;
    int nWords, loops, incWords;
    int Verbose = 0, power2 = 0, modulo = 5;
    DataType *dp;

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
        case 'p':
            power2++;
            break;
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

    if (power2) {
        nWords = 1;
        modulo = 1;
        loops = 21;
    }

    if (Verbose && me == 0) {
        if (power2) {
            printf("%s: nWords(1) << 1 per loop.\n", pgm);
        }
        else
            printf("%s: nWords(%d) loops(%d) nWords-incr-per-loop(%d)\n",
                pgm, nWords, loops, incWords);
    }

    for(l=0; l < loops; l++) {

        result_sz = (nProcs-1) * (nWords * sizeof(DataType));
        result = (DataType *)shmalloc(result_sz);
        if (! result)
        {
            perror ("Failed result memory allocation");
            exit (1);
        }
        for(dp=result; dp < &result[(result_sz/sizeof(DataType))];)
            *dp++ = 1;


        target_sz = nWords * sizeof(DataType);
        if (!(target = (DataType *)shmalloc(target_sz)))
        {
            perror ("Failed target memory allocation");
            exit (1);
        }
        for(dp=target; dp < &target[(target_sz / sizeof(DataType))];)
            *dp++ = 2;

        source_sz = 2 * nWords * sizeof(DataType);
        if (!(source = (DataType *)shmalloc(source_sz)))
        {
            perror ("Failed source memory allocation");
            exit (1);
        }
        for(dp=source; dp < &source[(source_sz / sizeof(DataType))];)
            *dp++ = 3;
#if 0
        printf("[%d] source %p target %p result %p\n",
            me, (void*)source,(void*)target,(void*)result);
        shmem_barrier_all(); 
#endif

        shmem_barrier_all(); /* sync sender and receiver */

        for(dp=source; dp < &source[(source_sz / sizeof(DataType))]; dp++)
            if (*dp != 3 ) {
                printf("source not consistent @ 3?\n");
                break;
            }
        shfree(source);

        for(dp=target; dp < &target[(target_sz / sizeof(DataType))]; dp++)
            if (*dp != 2 ) {
                printf("target not consistent @ 2?\n");
                break;
            }
        shfree(target);

        for(dp=result; dp < &result[(result_sz / sizeof(DataType))]; dp++)
            if (*dp != 1 ) {
                printf("result not consistent @ 1?\n");
                break;
            }
        shfree(result);

        if (loops > 1) {
            if (Verbose && me == 0) {
                if (l == 0 || (l % modulo == 0))
                    printf("End loop %3d nWords(%d)\n",(l+1),nWords);
            }
            if (power2)
                nWords <<= 1;
            else
                nWords += incWords; // watch for double inc.
        }
    }

    shmem_barrier_all(); /* sync before exiting */

    return 0;
}
