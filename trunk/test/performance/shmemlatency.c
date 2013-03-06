/*
**  This is a pingpong test used to calculate 
**  latency and bandwidth for various message 
**  sizes.
**
**  SHMEM version
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "shmem.h"

#define SIZE		(10000000)

#define TRUE  (1)
#define FALSE (0)

void doit(int len, double *latency, double *bandwidth);

double aligned_buf[SIZE/sizeof(double)];

char *buf;

int my_node;

int 
main(int argc, char *argv[])
{

    extern char *optarg;
    int ch, error;
	
    int len, start_len, end_len, increment, inc, trials, i;
    int mega;
    double latency, bandwidth;
    double tot_latency, tot_bandwidth;
    double max_latency, max_bandwidth;
    double min_latency, min_bandwidth;

    start_pes(0);

    my_node   = shmem_my_pe();

    if ( shmem_n_pes() < 2)   {
        if (my_node == 0)   {
            fprintf(stderr, "Need to run on at least two nodes\n");
        }
        exit(77);
    }

    /* Set the defaults */
    error= FALSE;
    start_len= 1;
    end_len= 1024;
    increment = 16;
    trials= 1000;
    mega= TRUE;
 
    /* check command line args */
    while ((ch= getopt(argc, argv, "i:e:s:n:m")) != EOF)   {
        switch (ch)   {
        case 'i':
            increment= strtol(optarg, (char **)NULL, 0);
            break;
        case 'e':
            end_len= strtol(optarg, (char **)NULL, 0);
            break;
        case 's':
            start_len= strtol(optarg, (char **)NULL, 0);
            if ( start_len < 1 ) start_len = 1;
            break;
        case 'n':
            trials= strtol(optarg, (char **)NULL, 0);
            break;
        case 'm': 
            mega= FALSE;
            break;
        default:
            error= TRUE;
            break;
        }
    }
 
    if (error)   {
        if (my_node == 0)   {
            fprintf(stderr, "Usage: %s [-s start_length] [-e end_length] [-i inc] [-n trials] [-m (millions)]\n", argv[0]);
        }
        exit (-1);
    }


    if (my_node == 0)   {
        printf("\n");
        printf("Results for %d trials each of length %d through %d in increments of %d\n\n", 
               trials, start_len, end_len, increment);
        printf("Length                  Latency                             Bandwidth\n");
        printf("in bytes            in micro seconds                ");
        if (mega)   {
            printf("in mega bytes per second\n");
        } else   {
            printf("in million bytes per second\n");
        }
        printf("            minimum     average     maximum     minimum     average     maximum\n");
    }


    for (len= start_len; len <= end_len; len += inc)   {
        buf= (char *)aligned_buf;
        latency= tot_latency= 0.0;
        max_latency= 0.0;
        min_latency= 1000000000.0;
        bandwidth= tot_bandwidth= 0.0;
        max_bandwidth= 0.0;
        min_bandwidth= 1000000000.0;

        for (i= 0; i < trials; i++)   {

            buf[len-1] = (char)my_node;

            shmem_barrier_all();

            doit(len, &latency, &bandwidth);
            tot_latency= tot_latency + latency;
            if (latency < min_latency)   {
                min_latency= latency;
            }
            if (latency > max_latency)   {
                max_latency= latency;
            }
            tot_bandwidth= tot_bandwidth + bandwidth;
            if (bandwidth < min_bandwidth)   {
                min_bandwidth= bandwidth;
            }
            if (bandwidth > max_bandwidth)   {
                max_bandwidth= bandwidth;
            }
        }

        if (my_node == 0)   {
            printf("%9d  %8.2f    %8.2f    %8.2f    ",
                   len, min_latency, tot_latency / trials, max_latency);
            if (mega)   {
                printf("%8.2f    %8.2f    %8.2f\n", 
                       min_bandwidth / (1024 * 1024),
                       (tot_bandwidth / trials) / (1024 * 1024), 
                       max_bandwidth / (1024 * 1024));
            } else   {
                printf("%8.2f    %8.2f    %8.2f\n", 
                       min_bandwidth / 1000000.0, 
                       (tot_bandwidth / trials) / 1000000.0, 
                       max_bandwidth / 1000000.0);
            }
        }

        if ( len == 1 ) {
            inc = increment - 1;
        } else {
            inc = increment;
        }
    }

}  /* end of main() */


void
doit(int len, double *latency, double *bandwidth)
{

    double start, end;

    if (my_node == 0)   {

        start = shmem_wtime();

        shmem_putmem( buf, buf, len, 1 );

        shmem_wait( (long *)&buf[len-1], (long)0 );

        end = shmem_wtime();

        *latency= (end - start) * 1000000.0 / 2.0;

        if ( (end - start) != 0 ) {
            *bandwidth= len / (end - start) * 2.0;
        } else {
            *bandwidth = 0.0;
        }

    } else {

        shmem_wait( (long *)&buf[len-1], (long)1 );

        buf[len-1] = (char)1;

        shmem_putmem( buf, buf, len, 0 );

        *latency   = 1.0;
        *bandwidth = 10.0;
    }
}  /* end of doit() */
