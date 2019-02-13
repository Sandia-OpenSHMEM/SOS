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

#ifndef MAX
#define MAX(A,B)   (((A)>(B)) ? (A) : (B))
#endif

#define ONE 1

/* constants for experiments */
#define MAX_MSG_SIZE (1<<23)
#define START_LEN 1
#define INC 2
#define TRIALS 1000
#define WINDOW_SIZE 64
#define WARMUP 100

/* constants for experiments with large message sizes */
#define TRIALS_LARGE  100
#define WINDOW_SIZE_LARGE 64
#define WARMUP_LARGE  10
#define LARGE_MESSAGE_SIZE  65536

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
} op_style;

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

typedef enum {
    OP_SET = 0,
    OP_INC,
    OP_ADD,
    OP_AND,
    OP_OR,
    OP_XOR,
    OP_FETCH,
    OP_CSWAP,
    OP_SWAP,
    OP_FINC,
    OP_FADD,
    OP_FAND,
    OP_FOR,
    OP_FXOR,
    LAST_OP
} atomic_op_type;

#define FIRST_FETCH_OP OP_FETCH

const char *atomic_op_name[] = { "set", "inc", "add", "and",
                                 "or", "xor", 
                                 "fetch", "cswap", "swap", "finc",
                                 "fadd", "fand", "for", "fxor" };

typedef struct perf_metrics {
    /* common parameters */
    test_type t_type;
    unsigned long int start_len, max_len;
    unsigned long int size_inc, trials;
    unsigned long int window_size, warmup;
    int my_node, num_pes, sztarget, szinitiator, midpt;
    char *src, *dest;
    op_style opstyle;

    /* parameters for threaded tests */
    int nthreads;
    int thread_safety;

    /* parameters specific to bandwidth tests */
    bw_units unit;
    const char *bw_type_str;
    bw_type b_type;
    comm_style cstyle;
    int target_data;
    int num_partners;

    /* parameters specific to latency tests */
    long *target;

    /* misc parameters */
    int validate;
    int individual_report;
} perf_metrics_t;

/* psync arrays used in metric calculation */
long red_psync[SHMEM_REDUCE_SYNC_SIZE];
long bar_psync[SHMEM_BARRIER_SYNC_SIZE];

/* default settings with no input provided */
static inline 
void set_metric_defaults(perf_metrics_t *metric_info) {
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

    metric_info->num_partners = 1;

#if defined(ENABLE_THREADS)
    metric_info->thread_safety = SHMEM_THREAD_SINGLE;
#else
    metric_info->thread_safety = 0;
#endif
    metric_info->nthreads = 1;

    metric_info->validate = false;
    metric_info->individual_report = -1;
}

/* update metrics after shmem init */
static inline 
void update_metrics(perf_metrics_t *metric_info) {
    metric_info->my_node = shmem_my_pe();
    metric_info->num_pes = shmem_n_pes();
    assert(metric_info->num_pes);
    metric_info->midpt = metric_info->num_pes / 2;
}

/* init psync arrays */
static inline
void init_psync_arrays(void) {
    int i;
    for(i = 0; i < SHMEM_REDUCE_SYNC_SIZE; i++)
        red_psync[i] = SHMEM_SYNC_VALUE;

    for(i = 0; i < SHMEM_BARRIER_SYNC_SIZE; i++)
        bar_psync[i] = SHMEM_SYNC_VALUE;
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
    if (num < 0)
        shmem_global_exit(1);
    return (num % 4 == 0);
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
int validate_recv(char *buf, int len, int partner_pe) {
    int i = 0;
    int array_size = len / sizeof(int);
    int *ibuf = (int *)buf;
    int errors = 0;

    assert(is_divisible_by_4(len));

    for (i = 0; i < array_size; i++) {
        if (ibuf[i] != partner_pe) {
            errors++;
        }
    }
    if (errors > 0) {
        printf("Validation error: stored_value = %d, expected value = %d\n", 
                                  ibuf[0], partner_pe);
    }
    return errors;
}

/**************************************************************/
/*                   Input Checking                           */
/**************************************************************/

static
int command_line_arg_check(int argc, char *argv[], perf_metrics_t * const metric_info) {
    int ch, errors = 0;
    extern char *optarg;

    /* check command line args */
    while ((ch = getopt(argc, argv, "e:s:n:w:p:r:l:kbivtC:T:")) != EOF) {
        switch (ch) {
        case 's':
            metric_info->start_len = strtoul(optarg, (char **)NULL, 0);
            if ( metric_info->start_len < 1 ) metric_info->start_len = 1;
            if(!is_pow_of_2(metric_info->start_len)) {
                fprintf(stderr, "Error: start_length must be a power of two\n");
                errors++;
            }
            if (metric_info->start_len > INT_MAX) {
                fprintf(stderr, "Error: start_length is out of integer range\n");
                errors++;
            }
            break;
        case 'e':
            metric_info->max_len = strtoul(optarg, (char **)NULL, 0);
            if(!is_pow_of_2(metric_info->max_len)) {
                fprintf(stderr, "Error: end_length must be a power of two\n");
                errors++;
            }
            if(metric_info->max_len < metric_info->start_len) {
                fprintf(stderr, "Error: end_length (%ld) must be >= "
                        "start_length (%ld)\n", metric_info->max_len,
                        metric_info->start_len);
                errors++;
            }
            if (metric_info->max_len > INT_MAX) {
                fprintf(stderr, "Error: end_length is out of integer range\n");
                errors++;
            }
            break;
        case 'n':
            metric_info->trials = strtoul(optarg, (char **)NULL, 0);
            if(metric_info->trials < (metric_info->warmup * 2)) {
                fprintf(stderr, "Error: trials (%ld) must be >= 2*warmup "
                        "(%ld)\n", metric_info->trials, metric_info->warmup * 2);
                errors++;
            }
            break;
        case 'p':
            metric_info->warmup = strtoul(optarg, (char **)NULL, 0);
            if(metric_info->warmup > (metric_info->trials/2)) {
                fprintf(stderr, "Error: warmup (%ld) must be <= trials/2 "
                        "(%ld)\n", metric_info->warmup, metric_info->trials/2);
                errors++;
            }
            break;
        case 'k':
            metric_info->unit = KB;
            if (metric_info->t_type != BW)
                errors++;
            break;
        case 'b':
            metric_info->unit = B;
            if (metric_info->t_type != BW)
                errors++;
            break;
        case 'v':
            metric_info->validate = true;
            if(metric_info->t_type == BW && metric_info->target_data) 
                errors++;
            break;
        case 'w':
            metric_info->window_size = strtoul(optarg, (char **)NULL, 0);
            if (metric_info->t_type != BW) {
                errors++;
            } else {
                if (metric_info->target_data) {
                    errors++;
                }
            }
            break;
        case 't':
            metric_info->target_data = true;
            if (metric_info->t_type != BW) {
                errors++;
            } else {
                if (metric_info->validate) {
                    errors++;
                }
            }
            break;
        case 'r':
            metric_info->sztarget = strtoul(optarg, (char **)NULL, 0);
            break;
        case 'l':
            metric_info->szinitiator = strtoul(optarg, (char **)NULL, 0);
            break;
        case 'C':
#if defined(ENABLE_THREADS)
            if (strcmp(optarg, "SINGLE") == 0) {
                metric_info->thread_safety = SHMEM_THREAD_SINGLE;
            } else if (strcmp(optarg, "FUNNELED") == 0) {
                metric_info->thread_safety = SHMEM_THREAD_FUNNELED;
            } else if (strcmp(optarg, "SERIALIZED") == 0) {
                metric_info->thread_safety = SHMEM_THREAD_SERIALIZED;
            } else if (strcmp(optarg, "MULTIPLE") == 0) {
                metric_info->thread_safety = SHMEM_THREAD_MULTIPLE;
            } else {
                fprintf(stderr, "Invalid threading level: \"%s\"\n", optarg);
                errors++;
            }
#else
            fprintf(stderr, "Threading support disabled. "
                            "Ignoring threading level: \"%s\"\n", optarg);
            metric_info->thread_safety = 0;
#endif
            break;
        case 'T':
            metric_info->nthreads = atoi(optarg);
            break;
        case 'i':
            metric_info->individual_report = 1;
            break;
        default:
            errors++;
            break;
        }
    }

    return errors;
}

static inline
void print_usage(int errors) {
    fprintf(stderr, "\nNumber of errors in the command line: %d\n", errors);
    fprintf(stderr, "\nUsage: <benchmark executable> [OPTION]\n" 
           " -s START_MSG_SIZE           Smallest message size. Must be power of 2\n"
           " -e END_MSG_SIZE             Largest message size. Must be power of 2\n"
           " -p WARMUP                   Number of warmup iterations\n"
           " -n TRIALS                   Number of trial iterations. Must be at\n"
           "                             least twice of WARMUP\n"
           " -w WINDOW_SIZE              Window size for streaming. Cannot be used\n"
           "                             in conjunction with -t. Specific to band-\n"
           "                             -width experiments\n"
           " -k                          Setting bandwidth metric to kbytes/second\n"
           " -b                          Setting bandwidth metric to bytes/second\n"
           " -v                          Turning on validation of data\n"
           " -i                          Turning on individual process reporting\n"
           " -t                          Output data for target side (default is \n"
           "                             initiator, only use with Put Bandwidth),\n"
           "                             cannot be used in conjunction with \n"
           "                             validate, special sizes used, trials + \n"
           "                             warmup * sizes (8/4KB) <= max length \n"
           " -r TARGET_SIZE              Number of target nodes, use only with -t;\n"
           " -l SOURCE_SIZE              Number of initiator nodes, use only with\n"
           "                             -t\n"
           " -T THREADS                  Number of threads\n"
           " -C THREAD_LEVEL             SHMEM thread level. Possible values: \n"
           "                             SINGLE, FUNNELED, SERIALIZED, MULTIPLE \n"
           );
}


#if defined(ENABLE_THREADS)
static 
const char *thread_safety_str(perf_metrics_t * const metric_info) {
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
void thread_safety_validation_check(perf_metrics_t * const metric_info) {
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
#endif

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


/* Returns partner node; Assumes only one partner */
static inline 
int partner_node(const perf_metrics_t * const my_info)
{
    if (my_info->num_pes == 1)
        return 0;

    if (my_info->t_type == BW) {
        if(my_info->cstyle == COMM_PAIRWISE) {
            int pairs = my_info->midpt;

            return (my_info->my_node < pairs ? (my_info->my_node + pairs) :
                   (my_info->my_node - pairs));
        } else {
            assert(my_info->cstyle == COMM_INCAST);
            return INCAST_PE;
        }
    } else {
        int pairs = my_info->midpt;

        return (my_info->my_node < pairs ? (my_info->my_node + pairs) :
               (my_info->my_node - pairs));
    }
}

static inline
int streaming_node(const perf_metrics_t * const my_info)
{
    if(my_info->cstyle == COMM_PAIRWISE) {
        return (my_info->my_node < my_info->szinitiator);
    } else {
        assert(my_info->cstyle == COMM_INCAST);
        return true;
    }
}

static inline
int target_node(const perf_metrics_t * const my_info)
{
    return (my_info->my_node >= my_info->midpt &&
        (my_info->my_node < (my_info->midpt + my_info->sztarget)));
}

static inline 
int is_streaming_node(const perf_metrics_t * const my_info, int node)
{
    if (my_info->cstyle == COMM_PAIRWISE) {
        return (node < my_info->szinitiator);
    } else {
        assert(my_info->cstyle == COMM_INCAST);
        return true;
    }
}

static inline
int check_hostname_validation(const perf_metrics_t * const my_info) {

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
    char *dest = (char *) shmem_malloc (my_info->num_pes * hostname_size * 
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
    shmem_fcollect32(dest, hostname, hostname_size/4, 0, 0, my_info->num_pes, 
                     pSync_collect);

    char *snode_name = NULL;
    char *tnode_name = NULL;
    for (i = 0; i < my_info->num_pes; i++) {
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
int error_checking_init_target_usage(perf_metrics_t * const metric_info) {
    int error = false;
    assert(metric_info->midpt > 0);

    if (metric_info->sztarget != -1 && metric_info->szinitiator == -1) {
        if (metric_info->sztarget < 1 ||
            metric_info->sztarget > metric_info->midpt ||
            !metric_info->target_data) {
            error = true;
        } else {
            metric_info->szinitiator = metric_info->midpt;
        }
    } else if (metric_info->sztarget == -1 && metric_info->szinitiator != -1) {
        if( metric_info->szinitiator < 1 ||
            metric_info->szinitiator > metric_info->midpt ||
            !metric_info->target_data) {
            error = true;
        } else {
            metric_info->sztarget = metric_info->midpt;
        }
    } else if (metric_info->sztarget == -1 && metric_info->szinitiator == -1) {
        metric_info->szinitiator = metric_info->midpt;
        metric_info->sztarget = metric_info->midpt;
    } else {
        if (!metric_info->target_data) {
            error = true;
        }
    }

    if (error) {
        fprintf(stderr, "Invalid usage of command line arg -r/-l\n");
        return -1;
    }
    return 0;
}

static inline
void large_message_metric_chg(perf_metrics_t * const metric_info, int len) {
    if(len > LARGE_MESSAGE_SIZE) {
        metric_info->window_size = WINDOW_SIZE_LARGE;
        metric_info->trials = TRIALS_LARGE;
        metric_info->warmup = WARMUP_LARGE;
    }
}

/* put/get bw use opposite streaming/validate nodes */
static inline
red_PE_set validation_set(perf_metrics_t * const my_info, int *nPEs)
{
    if(my_info->cstyle == COMM_PAIRWISE) {
        if(streaming_node(my_info)) {
            *nPEs = my_info->szinitiator;
            return FIRST_HALF;
        } else if(target_node(my_info)) {
            *nPEs = my_info->sztarget;
            return SECOND_HALF;
        } else {
            fprintf(stderr, "Warning: you are getting data from a node that "
                "wasn't a part of the perf set \n ");
            return 0;
        }
    } else {
        assert(my_info->cstyle == COMM_INCAST);
        *nPEs = my_info->num_pes;
        return FULL_SET;
    }
}

/* reduction to collect performance results from PE set
 * then start_pe will print results --- assumes num_pes is even */
static inline
void PE_set_used_adjustments(int *nPEs, int *stride, int *start_pe,
                             perf_metrics_t * const my_info) {
    red_PE_set PE_set = validation_set(my_info, nPEs);

    if(PE_set == FIRST_HALF || PE_set == FULL_SET) {
        *start_pe = 0;
    }
    else {
        assert(PE_set == SECOND_HALF);
        *start_pe = my_info->midpt;
    }

    *stride = 0; /* back to back PEs */
}

static
void print_header(perf_metrics_t * const metric_info) {
    printf("\n%20sSandia OpenSHMEM Performance Suite%20s\n", " ", " ");
    printf("%20s==================================%20s\n", " ", " ");
    printf("Total Number of PEs:    %10d%6sWindow size:            %10lu\n", 
            metric_info->num_pes, " ", metric_info->window_size);
    printf("Number of source PEs:   %10d%6sMaximum message size:   %10lu\n", 
            metric_info->szinitiator, " ", metric_info->max_len);
    printf("Number of target PEs:   %10d%6sNumber of threads:      %10d\n", 
            metric_info->sztarget, " ", metric_info->nthreads);
    printf("Iteration count:        %10lu%6s", metric_info->trials, " ");
#if defined(ENABLE_THREADS)
    printf("Thread safety:          %10s\n", thread_safety_str(metric_info));
#endif
    printf("\n");
}
