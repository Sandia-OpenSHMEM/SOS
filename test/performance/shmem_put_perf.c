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

#define MAX_MSG_SIZE        (1<<22)
#define START_LEN           1

#define INC 2
#define TRIALS 100
#define WARMUP 10

void pingpong_p(int *pingpong_ball, double *latency, double *bandwidth,
                int my_node, int npes);
void ping_put(char *buf, int len, double *latency, double *bandwidth,
              int my_node);

typedef struct perf_metrics {
    double latency, bandwidth;
    double tot_latency, tot_bandwidth;
    double max_latency, max_bandwidth;
    double min_latency, min_bandwidth;
} perf_metrics_t;

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

typedef struct metric_attrs {
   int start_len, max_len;
   int inc, trials;
   int mega, warmup;
} metric_attrs_t;

void metric_attrs_init(metric_attrs_t * data) {
   data->start_len = START_LEN;
   data->max_len = MAX_MSG_SIZE;
   data->inc = INC;
   data->trials = TRIALS;
   data->mega = TRUE;
   data->warmup = WARMUP; /*number of initial iterations to skip*/
}

void print_data_results(perf_metrics_t data, int len, metric_attrs_t attrs) {
   printf("%9d  %8.2f    %8.2f    %8.2f    ",
          len, data.min_latency, data.tot_latency / attrs.trials,
          data.max_latency);
   if (attrs.mega) {
      printf("%8.2f    %8.2f    %8.2f\n",
            data.min_bandwidth / (1024 * 1024),
            (data.tot_bandwidth / attrs.trials) / (1024 * 1024),
            data.max_bandwidth / (1024 * 1024));
      } else {
         printf("%8.2f    %8.2f    %8.2f\n",
                data.min_bandwidth / 1000000.0,
                (data.tot_bandwidth / attrs.trials) / 1000000.0,
                data.max_bandwidth / 1000000.0);
      }
}

void print_results_header(int mega) {
   printf("\nLength                  Latency                            "\
          "Bandwidth\n");
   printf("in bytes            in micro seconds                ");
   if (mega) {
      printf("in mega bytes per second\n");
   } else {
      printf("in million bytes per second\n");
   }
   printf("            minimum     average     maximum     minimum     "\
          "average     maximum\n");

}

char * buffer_alloc_and_init(int len) {
    unsigned long page_align;
    char *buf;
    int i;

    page_align = getpagesize();
    buf = shmem_malloc(len + page_align);
    buf = (char *) (((unsigned long) buf + (page_align - 1)) /
                    page_align * page_align);

    for(i = 0; i < len; i++)
       buf[i] = 'z';

    return buf;
}

void command_line_arg_check(int argc, char *argv[],
                            metric_attrs_t *metric_info, int my_node) {
    int ch, error = FALSE;
    extern char *optarg;

    /* check command line args */
    while ((ch = getopt(argc, argv, "i:e:s:n:m")) != EOF) {
        switch (ch) {
        case 'i':
            metric_info->inc = strtol(optarg, (char **)NULL, 0);
            break;
        case 'e':
            metric_info->max_len = strtol(optarg, (char **)NULL, 0);
            break;
        case 's':
            metric_info->start_len = strtol(optarg, (char **)NULL, 0);
            if ( metric_info->start_len < 1 ) metric_info->start_len = 1;
            break;
        case 'n':
            metric_info->trials = strtol(optarg, (char **)NULL, 0);
            if(metric_info->trials <= (metric_info->warmup*2)) error = TRUE;
            break;
        case 'm':
            metric_info->mega = FALSE;
            break;
        default:
            error = TRUE;
            break;
        }
    }

    if (error) {
        if (my_node == 0) {
            fprintf(stderr, "Usage: %s [-s start_length] [-e end_length] "\
                    ": lengths must be a power of two \n " \
                    "[-i inc] [-n trials (must be greater than 20)] "\
                    "[-m (millions)]\n", argv[0]);
        }
        shmem_finalize();
        exit (-1);
    }
}

/* symmetric data for communication */
int pingpong_ball;

int main(int argc, char *argv[])
{
    int len, i;
    int my_node, num_pes;
    int *pingpong_ball;
    char * buf = NULL;
    perf_metrics_t data;
    metric_attrs_t attrs;
    data_flush(&data);

    shmem_init();
    my_node = shmem_my_pe();
    num_pes = shmem_n_pes();

    if (num_pes != 2) {
        if (my_node == 0) {
            fprintf(stderr, "Currently only 2-node test\n");
        }
        shmem_finalize();
        exit(77);
    }

    /* init data */
    metric_attrs_init(&attrs);
    command_line_arg_check(argc, argv, &attrs, my_node);
    buf = buffer_alloc_and_init(attrs.max_len);
    pingpong_ball = shmem_malloc(sizeof(int));
    *pingpong_ball = INIT_VALUE;

    if(my_node == 0)
       printf("\nResults for %d trials each of length %d through %d in"\
              " increments of %d\n", attrs.trials, attrs.start_len,
              attrs.max_len, attrs.inc);


/**************************************************************/
/*                   PINGPONG with small put                  */
/**************************************************************/

   if (my_node == 0) {
       printf("\nPing-Pong shmem_int_p results:\n");
       print_results_header(attrs.mega);
   }
        for (i= 0; i < attrs.trials + attrs.warmup; i++) {

            *pingpong_ball = INIT_VALUE;

            pingpong_p(pingpong_ball, &data.latency, &data.bandwidth,
                       my_node, num_pes);

            if(my_node == 0 && i >= attrs.warmup)
               calc_metric_data(&data);
        }

        if (my_node == 0)
           print_data_results(data, sizeof(int), attrs);


/**************************************************************/
/*                   PING over varying message sizes          */
/**************************************************************/


    if (my_node == 0) {
       printf("\nPing shmem_putmem results:\n");
       print_results_header(attrs.mega);
    }

    for (len = attrs.start_len; len <= attrs.max_len; len *= attrs.inc) {

        data_flush(&data);

        shmem_barrier_all();

        for (i = 0; i < attrs.trials + attrs.warmup; i++) {

            ping_put(buf, len, &data.latency, &data.bandwidth, my_node);

            if(my_node == 0 && i >= attrs.warmup)
               calc_metric_data(&data);
        }

        shmem_barrier_all();

        if (my_node == 0)
           print_data_results(data, len, attrs);
    }

    shmem_barrier_all();

    shmem_finalize();
    return 0;
}  /* end of main() */


void
pingpong_p(int * pingpong_ball, double *latency, double *bandwidth,
           int my_node, int npes)
{

    double start, end;
    int dest = (my_node + 1) % npes;

    shmem_barrier_all();

    if (my_node == 0) {

        start = shmemx_wtime();

        shmem_int_p(pingpong_ball, dest, dest);

        shmem_int_wait(pingpong_ball, INIT_VALUE);

        end = shmemx_wtime();

        *latency = (end - start) * 1000000.0 / 2.0;

        if ((end - start) != 0 ) {
            *bandwidth = sizeof(int) / (end - start) * 2.0;
        } else {
            *bandwidth = 0.0;
        }

   } else {

        shmem_int_wait(pingpong_ball, INIT_VALUE);

        shmem_int_p(pingpong_ball, dest, dest);
   }

} /*gauge small put pathway round trip latency*/


void
ping_put(char * buf, int len, double *latency, double *bandwidth, int my_node)
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
