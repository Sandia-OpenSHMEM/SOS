/*
 *  Copyright (c) 2015 Intel Corporation. All rights reserved.
 *  This software is available to you under the BSD license below:
 *
 * *	Redistribution and use in source and binary forms, with or
 *	without modification, are permitted provided that the following
 *	conditions are met:
 *
 *	- Redistributions of source code must retain the above
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
**
**  This is a latency and bandwidth test for put derived from shmemlatency
**
**  Notice: micro benchmark ~ two nodes only
**
**  Features of Test:
**  1) small put pingpong test latency and bandwidth
**  2) one sided latency test to calculate latency and bandwidth of various sizes
**    to the network stack
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <shmem.h>
#include <shmemx.h>
#include <string.h>

#define SIZE		(10000000)

#define TRUE  (1)
#define FALSE (0)
#define INIT_VALUE -1

void pingpong_p(double *latency, double *bandwidth, int my_node, int npes);
void ping_put(int len, double *latency, double *bandwidth, int my_node);

typedef struct perf_metrics {
    double latency, bandwidth;
    double tot_latency, tot_bandwidth;
    double max_latency, max_bandwidth;
    double min_latency, min_bandwidth;
} perf_metrics_t;

void data_flush(perf_metrics_t * data);
void calc_metric_data(perf_metrics_t * data);
void print_data_results(perf_metrics_t data, int len, int trials, int mega);
void print_results_header(int mega);

void data_flush(perf_metrics_t * data) {
   data->latency= data->tot_latency = 0.0;
   data->max_latency = 0.0;
   data->min_latency = 1000000000.0;
   data->bandwidth = data->tot_bandwidth= 0.0;
   data->max_bandwidth = 0.0;
   data->min_bandwidth = 1000000000.0;
}

void calc_metric_data(perf_metrics_t * data) {
   data->tot_latency = data->tot_latency + data->latency;
   if (data->latency < data->min_latency) {
       data->min_latency = data->latency;
   }
   if (data->latency > data->max_latency) {
       data->max_latency = data->latency;
   }
   data->tot_bandwidth = data->tot_bandwidth + data->bandwidth;
   if (data->bandwidth < data->min_bandwidth) {
       data->min_bandwidth = data->bandwidth;
   }
   if (data->bandwidth > data->max_bandwidth) {
       data->max_bandwidth = data->bandwidth;
   }
}

void print_data_results(perf_metrics_t data, int len, int trials, int mega) {
   printf("%9d  %8.2f    %8.2f    %8.2f    ",
          len, data.min_latency, data.tot_latency / trials, data.max_latency);
   if (mega) {
      printf("%8.2f    %8.2f    %8.2f\n",
            data.min_bandwidth / (1024 * 1024),
            (data.tot_bandwidth / trials) / (1024 * 1024),
            data.max_bandwidth / (1024 * 1024));
      } else {
         printf("%8.2f    %8.2f    %8.2f\n",
                data.min_bandwidth / 1000000.0,
                (data.tot_bandwidth / trials) / 1000000.0,
                data.max_bandwidth / 1000000.0);
      }
}

void print_results_header(int mega) {
   printf("\nLength                  Latency                             Bandwidth\n");
   printf("in bytes            in micro seconds                ");
   if (mega) {
      printf("in mega bytes per second\n");
   } else {
      printf("in million bytes per second\n");
   }
   printf("            minimum     average     maximum     minimum     average     maximum\n");

}

/* symmetric data for communication */
double aligned_buf[SIZE/sizeof(double)];
char *buf;
int pingpong_ball;

int main(int argc, char *argv[])
{

    extern char *optarg;
    int ch, error;

    int len, start_len, end_len, increment, inc, trials, i;
    int mega, my_node, num_pes, warmup;
    pingpong_ball = INIT_VALUE;
    perf_metrics_t data;
    data_flush(&data);
    buf = (char *)aligned_buf;
    int array_size = SIZE/sizeof(double);
    memset(buf, '\0', array_size);

    shmem_init();
    my_node = shmem_my_pe();
    num_pes = shmem_n_pes();

    if (num_pes != 2) {
        if (my_node == 0) {
            fprintf(stderr, "Currently only 2-node test\n");
        }
        exit(77);
    }

    /* Set the defaults */
    error = FALSE;
    start_len = 1;
    end_len = 1024;
    increment = 16;
    trials = 1000;
    warmup = 10;
    mega = TRUE;

    /* check command line args */
    while ((ch = getopt(argc, argv, "i:e:s:n:m")) != EOF) {
        switch (ch) {
        case 'i':
            increment = strtol(optarg, (char **)NULL, 0);
            break;
        case 'e':
            end_len = strtol(optarg, (char **)NULL, 0);
            break;
        case 's':
            start_len = strtol(optarg, (char **)NULL, 0);
            if ( start_len < 1 ) start_len = 1;
            break;
        case 'n':
            trials = strtol(optarg, (char **)NULL, 0);
            if(trials <= (warmup*2)) error = TRUE;
            break;
        case 'm':
            mega = FALSE;
            break;
        default:
            error = TRUE;
            break;
        }
    }

    if (error) {
        if (my_node == 0) {
            fprintf(stderr, "Usage: %s [-s start_length] [-e end_length] [-i inc] [-n trials (must be greater than 20)] [-m (millions)]\n", argv[0]);
        }
        exit (-1);
    }

   if(my_node == 0)
      printf("\nResults for %d trials each of length %d through %d in increments of %d\n", trials, start_len, end_len, increment);


/**************************************************************/
/*                   PINGPONG with small put                  */
/**************************************************************/

   if (my_node == 0) {
       printf("\nPing-Pong shmem_int_p results:\n");
       print_results_header(mega);
   }
        for (i= 0; i < trials + warmup; i++) {

            pingpong_ball = INIT_VALUE;

            pingpong_p(&data.latency, &data.bandwidth, my_node, num_pes);

            if(my_node == 0 && i >= warmup)
               calc_metric_data(&data);
        }

        if (my_node == 0)
           print_data_results(data, sizeof(int), trials, mega);


/**************************************************************/
/*                   PING over varying message sizes          */
/**************************************************************/


    if (my_node == 0) {
       printf("\nPing shmem_putmem results:\n");
       print_results_header(mega);
    }

    for (len= start_len; len <= end_len; len += inc) {

        data_flush(&data);

        shmem_barrier_all();

        for (i= 0; i < trials + warmup; i++) {

            ping_put(len, &data.latency, &data.bandwidth, my_node);

            if(my_node == 0 && i >= warmup)
               calc_metric_data(&data);
        }

        shmem_barrier_all();

        if (my_node == 0)
           print_data_results(data, len, trials, mega);

        if (len == 1) {
            inc = increment - 1;
        } else {
            inc = increment;
        }
    }

    shmem_barrier_all();

    shmem_finalize();
    return 0;
}  /* end of main() */


void
pingpong_p(double *latency, double *bandwidth, int my_node, int npes)
{

    double start, end;
    int dest = (my_node + 1) % npes;

    shmem_barrier_all();

    if (my_node == 0) {

        start = shmemx_wtime();

        shmem_int_p(&pingpong_ball, dest, dest);

        shmem_int_wait(&pingpong_ball, INIT_VALUE);

        end = shmemx_wtime();

        *latency = (end - start) * 1000000.0 / 2.0;

        if ((end - start) != 0 ) {
            *bandwidth = sizeof(int) / (end - start) * 2.0;
        } else {
            *bandwidth = 0.0;
        }

   } else {

        shmem_int_wait(&pingpong_ball, INIT_VALUE);

        shmem_int_p(&pingpong_ball, dest, dest);
   }

} /*gauge small put pathway round trip latency*/


void
ping_put(int len, double *latency, double *bandwidth, int my_node)
{
    double start, end;

    if (my_node == 0) {

        start = shmemx_wtime();

        shmem_putmem(buf, buf, len, 1 );
        shmem_quiet();

        end = shmemx_wtime();

        *latency = (end - start) * 1000000.0;

        if ((end - start) != 0 ) {
            *bandwidth = len / (end - start);
        } else {
            *bandwidth = 0.0;
        }

    }
} /* latency/bw for one-way trip */
