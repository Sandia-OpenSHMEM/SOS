/*
 * SHMEM PingPong test
 * :vim:sw=4:ts=4:
 */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <sys/time.h>

int getSize (char *);
double gettime(void);
double dt (double *, double *);
void usage (char *);
void help (char *);
void printStats (int, int, int, int, double);

#define dprint if (Verbose) printf
#define DFLT_MIN_WORDS 128
#define DFLT_REPS 1000;

int Verbose=0;

#include <shmem.h>

int getSize (char *str)
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

double gettime()
{
	struct timeval tv;
	gettimeofday(&tv, 0);
	return (tv.tv_sec * 1000000 + tv.tv_usec);
}

double dt (double *tv1, double *tv2)
{
	return (*tv1 - *tv2);
}

void usage (char *name)
{
	fprintf (stderr, "Usage: %s [flags] nwords [maxWords] [incWords]\n", name);
	fprintf (stderr, " %s -h\n", name);
	exit (1);
}

void help (char *name)
{
	if (_my_pe() == 0) {
		printf ("Usage: %s [flags] nwords [maxWords] [incWords]\n\n", name);
		printf (" Flags may be any of\n");
		printf (" -n number repititions\n");
		printf (" -e everyone print timing info\n");
		printf (" -h print this info\n\n");
		printf (" Numbers may be postfixed with 'k' or 'm'\n\n");
	}
	shmem_barrier_all();
	exit (0);
}

void printStats (int proc, int peer, int doprint, int now, double t)
{
	if (doprint || (proc & 1))
		printf("%3d pinged %3d: %8d words %9.2f uSec %8.2f MB/s\n",
				proc, peer, now, t, sizeof(long)*now/(t));
}

int main (int argc, char *argv[])
{
	double t,tv[2];
	int reps = DFLT_REPS;
	int doprint = 1/*0*/;
	char *progName;
	int minWords;
	int maxWords;
	int incWords, nwords, nproc, proc, peer, c, r, i;
	long *rbuf;	/* remote buffer - sink */
	long *tbuf;	/* transmit buffer - src */

	start_pes(0);
	proc = _my_pe();
	nproc = _num_pes();
	if (nproc == 1) {
		fprintf(stderr, "ERR - Requires > 1 Processing Elements\n");
		return 1;
	}

	for (progName = argv[0] + strlen(argv[0]);
		 progName > argv[0] && *(progName - 1) != '/';
		 progName--)
		;

	while ((c = getopt (argc, argv, "n:evh")) != -1)
		switch (c)
		{
		case 'n':
			if ((reps = getSize (optarg)) <= 0)
				usage (progName);
			break;
		case 'e':
			doprint++;
			break;
		case 'v':
			Verbose++;
			break;
		case 'h':
			help (progName);
		default:
			usage (progName);
		}

	if (optind == argc)
		minWords = DFLT_MIN_WORDS;
	else if ((minWords = getSize (argv[optind++])) <= 0)
		usage (progName);

	if (optind == argc)
		maxWords = minWords;
	else if ((maxWords = getSize (argv[optind++])) < minWords)
		usage (progName);

	if (optind == argc)
		incWords = 0;
	else if ((incWords = getSize (argv[optind++])) < 0)
		usage (progName);

	if (!(rbuf = (long *)shmalloc(maxWords * sizeof(long))))
	{
		perror ("Failed memory allocation");
		exit (1);
	}
	memset (rbuf, 0, maxWords * sizeof (long));

	if (!(tbuf = (long *)shmalloc(maxWords * sizeof(long))))
	{
		perror ("Failed memory allocation");
		exit (1);
	}

	for (i = 0; i < maxWords; i++)
		tbuf[i] = 1000 + (i & 255);

	if (doprint)
		printf ("%d(%d): Shmem PING reps %d minWords %d maxWords %d "
				"incWords %d\n",
				proc, nproc, reps, minWords, maxWords, incWords);

        dprint("[%d] rbuf: %ld\n", proc, (unsigned long) rbuf);

	shmem_barrier_all();

	peer = proc ^ 1;
	if (peer >= nproc)
		doprint = 0;

	for (nwords = minWords;
		 nwords <= maxWords;
		 nwords = incWords ? nwords + incWords : nwords ? 2 * nwords : 1)
	{
		r = reps;
		shmem_barrier_all();
		tv[0] = gettime();
		if (peer < nproc)
		{
			if (proc & 1)
			{
				r--;
				shmem_wait(&rbuf[nwords-1], 0);
				rbuf[nwords-1] = 0;
			}

			while (r-- > 0)
			{
				shmem_long_put(rbuf, tbuf, nwords, peer);
				shmem_wait(&rbuf[nwords-1], 0);
				rbuf[nwords-1] = 0;
			}

			if (proc & 1)
			{
				shmem_long_put(rbuf, tbuf, nwords, peer);
			}
		}
		tv[1] = gettime();
		t = dt (&tv[1], &tv[0]) / (2 * reps);

		shmem_barrier_all();

		printStats (proc, peer, doprint, nwords, t);
	}

    shfree(rbuf);
    shfree(tbuf);

	shmem_barrier_all();
	return 0;
}
