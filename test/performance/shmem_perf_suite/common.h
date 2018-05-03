/*
 *  Copyright (c) 2018 Intel Corporation. All rights reserved.
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

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <shmem.h>
#include <string.h>
#include <assert.h>
#include <stdbool.h>
#include <sys/time.h>
#include <time.h>
#include <stdint.h>
#include <limits.h>
#include <sys/param.h>

/* hostname length to check for hostname errors */
#ifdef MAXHOSTNAMELEN
#define MAX_HOSTNAME_LEN MAXHOSTNAMELEN
#else
#define MAX_HOSTNAME_LEN HOST_NAME_MAX
#endif

#define ONE 1

/* constants for experiments */
#define MAX_MSG_SIZE (1<<23)
#define START_LEN 1
#define INC 2
#define TRIALS 500
#define WINDOW_SIZE 64
#define WARMUP 50

/* constants for experiments with large message sizes */
#define TRIALS_LARGE  100
#define WINDOW_SIZE_LARGE 64
#define WARMUP_LARGE  10
#define LARGE_MESSAGE_SIZE  8192

#define TARGET_SZ_MIN 8
#define TARGET_SZ_MAX 4096

/* atomics common */
#define ATOMICS_N_DTs 3
/* note: ignoring cswap/swap for now in verification */
#define ATOMICS_N_OPs 4
/* PE 0 is printing its latency, thus have it not be the INCAST PE*/
#define INCAST_PE 1

/* perf metrics structures */
typedef enum {
    LAT,
    BW
} test_type;

typedef enum {
    UNI_DIR,
    BI_DIR
} bw_type;

typedef enum {
    STYLE_PUT,
    STYLE_GET,
    STYLE_RMA,
    STYLE_ATOMIC
} bw_style;

typedef enum {
    FIRST_HALF,
    SECOND_HALF,
    FULL_SET
} red_PE_set;

typedef enum {
    COMM_PAIRWISE,
    COMM_INCAST
} comm_style;

typedef enum {
    B,
    KB,
    MB
} bw_units;

typedef struct perf_metrics {
    /* common parameters */
    test_type t_type;
    unsigned long int start_len, max_len;
    unsigned long int size_inc, trials;
    unsigned long int window_size, warmup;
    int my_node, num_pes, sztarget, szinitiator, midpt;
    char *src, *dest;

    /* parameters for threaded tests */
    int nthreads;
    int thread_safety;

    /* parameters specific to bandwidth tests */
    bw_units unit;
    const char *bw_type_str;
    bw_type b_type;
    comm_style cstyle;
    bw_style bwstyle;
    int target_data;

    /* parameters specific to latency tests */
    long *target;

    /* misc parameters */
    int validate;
    int individual_report;
} perf_metrics_t;

/* default settings with no input provided */
static void set_metric_defaults(perf_metrics_t *metric_info) {
    metric_info->start_len = START_LEN;
    metric_info->max_len = MAX_MSG_SIZE;
    metric_info->size_inc = INC;
    metric_info->trials = TRIALS;
    metric_info->window_size = WINDOW_SIZE; /*back-to-back msg stream*/
    metric_info->warmup = WARMUP; /*number of initial iterations to skip*/

    metric_info->my_node = -1;
    metric_info->num_pes = -1;
    metric_info->midpt = -1;
    metric_info->sztarget = -1;
    metric_info->szinitiator = -1;

    metric_info->src = NULL;
    metric_info->dest = NULL;

    metric_info->thread_safety = SHMEM_THREAD_SINGLE;
    metric_info->nthreads = 1;

    metric_info->validate = false;
    metric_info->individual_report = -1;
}

/* update metrics after shmem init */
static void update_metrics(perf_metrics_t *metric_info) {
    metric_info->my_node = shmem_my_pe();
    metric_info->num_pes = shmem_n_pes();
    assert(metric_info->num_pes);
    metric_info->midpt = metric_info->num_pes / 2;
}

/* return microseconds */
double perf_shmemx_wtime(void);

double perf_shmemx_wtime(void)
{
    double wtime = 0.0;

#ifdef CLOCK_MONOTONIC
    struct timespec tv;
    clock_gettime(CLOCK_MONOTONIC, &tv);
    wtime = tv.tv_sec * 1e6;
    wtime += (double)tv.tv_nsec / 1000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec * 1e6;
    wtime += (double)tv.tv_usec;
#endif
    return wtime;
}

#ifdef CRAY_SHMEM
#define shmem_putmem_nbi(dest, source, nelems, pe) shmem_putmem_nb(dest, source, nelems, pe, NULL)
#define shmem_getmem_nbi(dest, source, nelems, pe) shmem_getmem_nb(dest, source, nelems, pe, NULL)
#endif

static char * aligned_buffer_alloc(int len)
{
    unsigned long alignment = 0;
    char *ptr1 = NULL, *ptr_aligned = NULL;
    size_t ptr_size = sizeof(uintptr_t);
    uintptr_t save_ptr1 = 0;

    alignment = getpagesize();

#ifndef VERSION_1_0
    ptr1 = shmem_malloc(ptr_size + alignment + len);
#else
    ptr1 = shmalloc(ptr_size + alignment + len);
#endif
    assert(ptr1 != NULL);

    save_ptr1 = (uintptr_t)ptr1;

    /* reserve at least ptr_size before alignment chunk */
    ptr1 = (char *) (ptr1 + ptr_size);

    /* only offset ptr by alignment to ensure len is preserved */
    /* clear bottom bits to ensure alignment */
    ptr_aligned = (char *) ( ((uintptr_t) ((char *) (ptr1 + alignment)))
                                                & ~(alignment-1));

    /* embed org ptr address in reserved ptr_size space */
    memcpy((ptr_aligned - ptr_size), &save_ptr1, ptr_size);

    return ptr_aligned;
}

static void aligned_buffer_free(char * ptr_aligned)
{
    char * ptr_org;
    uintptr_t temp_p;
    size_t ptr_size = sizeof(uintptr_t);

    /* grab ptr */
    memcpy(&temp_p, (ptr_aligned - ptr_size), ptr_size);
    ptr_org = (char *) temp_p;

#ifndef VERSION_1_0
    shmem_free(ptr_org);
#else
    shfree(ptr_org);
#endif
}

static inline 
int is_divisible_by_4(int num) {
    assert(num >= 0);
    assert(sizeof(int) == 4);
    return (!(num & 0x00000003));
}

/*to be a power of 2 must only have 1 set bit*/
static inline 
int is_pow_of_2(unsigned int num) {
    /*move first set bit all the way to right*/
    while(num && !((num >>=1 ) & 1));

    /*it will be 1 if its the only set bit*/
    return ((num == 1 || num == 0) ? true : false);
}

static 
void init_array(const char *buf, int len, int my_pe_num) {
    int i = 0;
    int array_size = len / sizeof(int);
    int *ibuf = (int *)buf;

    assert(is_divisible_by_4(len));

    for(i = 0; i < array_size; i++)
        ibuf[i] = my_pe_num;
}

static inline 
void validate_recv(char *buf, int len, int partner_pe) {
    int i = 0;
    int array_size = len / sizeof(int);
    int *ibuf = (int *)buf;

    assert(is_divisible_by_4(len));

    for(i = 0; i < array_size; i++) {
        if(ibuf[i] != partner_pe)
            printf("validation error at index %d: %d != %d \n", i, ibuf[i],
                    partner_pe);
    }
}

static 
const char *thread_safety_str(perf_metrics_t *metric_info) {
    if (metric_info->thread_safety == SHMEM_THREAD_SINGLE) {
        return "SINGLE";
    } else if (metric_info->thread_safety == SHMEM_THREAD_FUNNELED) {
        return "FUNNELED";
    } else if (metric_info->thread_safety == SHMEM_THREAD_SERIALIZED) {
        return "SERIALIZED";
    } else if (metric_info->thread_safety == SHMEM_THREAD_MULTIPLE) {
        return "MULTIPLE";
    } else {
        fprintf(stderr, "Unexpected thread safety value: %d. " 
                        "Setting it to SINGLE\n", metric_info->thread_safety);
        metric_info->thread_safety = SHMEM_THREAD_SINGLE;
        return "SINGLE";
    }
}

static inline 
void thread_safety_validation_check(perf_metrics_t *metric_info) {
    if (metric_info->nthreads == 1)
        return;
    else {
        if (metric_info->thread_safety != SHMEM_THREAD_MULTIPLE) {
            if(metric_info->my_node == 0) {
                fprintf(stderr, "Warning: argument \"-T %d\" is ignored"
                                " because of the thread level specified."
                                " Switching to single thread with thread" 
                                " safety %s\n", metric_info->nthreads,
                                  thread_safety_str(metric_info));
            }
            metric_info->nthreads = 1;
        }
        return;
    }
}

static inline 
int only_even_PEs_check(int my_node, int num_pes) {
    if (num_pes % 2 != 0) {
        if (my_node == 0) {
            fprintf(stderr, "Only even number of processes can be used\n");
        }
        return 77;
    } else
        return 0;
}

static inline 
int partner_node(perf_metrics_t my_info)
{
    if(my_info.num_pes == 1)
        return 0;

    if (my_info.t_type == BW) {
        if(my_info.cstyle == COMM_PAIRWISE) {
            int pairs = my_info.midpt;

            return (my_info.my_node < pairs ? (my_info.my_node + pairs) :
                   (my_info.my_node - pairs));
        } else {
            assert(my_info.cstyle == COMM_INCAST);
            return INCAST_PE;
        }
    } else {
        int pairs = my_info.midpt;

        return (my_info.my_node < pairs ? (my_info.my_node + pairs) :
               (my_info.my_node - pairs));
    }
}

static inline 
int is_streaming_node(perf_metrics_t my_info, int node)
{
    if(my_info.cstyle == COMM_PAIRWISE) {
        return (node < my_info.szinitiator);
    } else {
        assert(my_info.cstyle == COMM_INCAST);
        return true;
    }
}

static inline
int check_hostname_validation(perf_metrics_t my_info) {

    int hostname_status = -1;

    /* hostname_size should be a length divisible by 4 */
    int hostname_size = (MAX_HOSTNAME_LEN % 4 == 0) ? MAX_HOSTNAME_LEN :
                         MAX_HOSTNAME_LEN + (4 - MAX_HOSTNAME_LEN % 4);
    int i, errors = 0;

    /* pSync for fcollect of hostnames */
    static long pSync_collect[SHMEM_COLLECT_SYNC_SIZE];
    for (i = 0; i < SHMEM_COLLECT_SYNC_SIZE; i++)
        pSync_collect[i] = SHMEM_SYNC_VALUE;

    char *hostname = (char *) shmem_malloc (hostname_size * sizeof(char));
    char *dest = (char *) shmem_malloc (my_info.num_pes * hostname_size * 
                                        sizeof(char));

    if (hostname == NULL || dest == NULL) {
        fprintf(stderr, "shmem_malloc failed to allocate for hostname strings\n");
        return -1;
    }

    hostname_status = gethostname(hostname, hostname_size);
    if (hostname_status != 0) {
        fprintf(stderr, "gethostname failed (%d)\n", hostname_status);
        return -1;
    }
    shmem_barrier_all();

    /* nelems needs to be updated based on 32-bit API */
    shmem_fcollect32(dest, hostname, hostname_size/4, 0, 0, my_info.num_pes, 
                     pSync_collect);

    char *snode_name = NULL;
    char *tnode_name = NULL;
    for (i = 0; i < my_info.num_pes; i++) {
        char *curr_name = &dest[i * hostname_size];

        if (is_streaming_node(my_info, i)) {
            if (snode_name == NULL) {
                snode_name = curr_name;
            }

            if (strncmp(snode_name, curr_name, hostname_size) != 0) {
                fprintf(stderr, "PE %d on %s is a streaming node "
                                "but not placed on %s\n", i, curr_name, 
                                 snode_name);
                errors++;
            }
        } else {
            if (tnode_name == NULL) {
                tnode_name = curr_name;
            }

            if (strncmp(tnode_name, curr_name, hostname_size) != 0) {
                fprintf(stderr, "PE %d on %s is a target node "
                                "but not placed on %s\n", i, curr_name, 
                                 tnode_name);
                errors++;
            }
        }
    }

    if (snode_name == NULL || tnode_name == NULL) {
        fprintf(stderr, "Error: no streaming or target node\n");
        return -1;
    }

    if (strncmp(snode_name, tnode_name, hostname_size) == 0) {
        fprintf(stderr, "Warning: senders and receivers are running on the "
                        "same node %s\n", snode_name);
    }

    shmem_free(dest);
    shmem_free(hostname);

    return errors;
}

static
int error_checking_init_target_usage(perf_metrics_t *metric_info) {
    int error = false;
    assert(metric_info->midpt > 0);

    if(metric_info->sztarget != -1 && metric_info->szinitiator != -1)
        error = true; /* can't use them together  */

    if(metric_info->sztarget != -1) {
        if(metric_info->sztarget < 1 ||
           metric_info->sztarget > metric_info->midpt ||
           !metric_info->target_data) {
            error = true;
        }
    } else {
        metric_info->sztarget = metric_info->midpt;
    }

    if(metric_info->szinitiator != -1) {
        if(metric_info->szinitiator < 1 ||
           metric_info->szinitiator > metric_info->midpt ||
           !metric_info->target_data) {
            error = true;
        }
    } else {
        metric_info->szinitiator = metric_info->midpt;
    }

    if(error) {
        fprintf(stderr, "Invalid usage of command line arg -r/-l,"
                        " use --help for info\n");
        return -1;
    }
    return 0;
}

static inline
void large_message_metric_chg(perf_metrics_t *metric_info, int len) {
    if(len > LARGE_MESSAGE_SIZE) {
        metric_info->window_size = WINDOW_SIZE_LARGE;
        metric_info->trials = TRIALS_LARGE;
        metric_info->warmup = WARMUP_LARGE;
    }
}

static
void print_header(perf_metrics_t metric_info) {
    printf("\n%20sSandia OpenSHMEM Performance Suite%20s\n", " ", " ");
    printf("%20s==================================%20s\n", " ", " ");
    printf("Total Number of PEs:    %10d%6s", metric_info.num_pes, " ");
    printf("Window size:            %10lu\n", metric_info.window_size);
    printf("Number of source PEs:   %10d%6s", metric_info.szinitiator, " ");
    printf("Maximum message size:   %10lu\n", metric_info.max_len);
    printf("Number of target PEs:   %10d%6s", metric_info.sztarget, " ");
    printf("Number of threads:      %10d\n", metric_info.nthreads);
    printf("Iteration count:        %10lu%6s", metric_info.trials, " ");
    printf("Thread safety:          %10s\n", thread_safety_str(&metric_info));
}
