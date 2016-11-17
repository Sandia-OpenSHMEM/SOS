/*
 *  Copyright (c) 2016 Intel Corporation. All rights reserved.
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

/*
  Mandelbrot set calculation using OpenSHMEM

  Author: Mario Flajslik
  Company: Intel Corporation
  Email: mario.flajslik@intel.com
*/

#define _GNU_SOURCE
#include <stdio.h>
#include <stdlib.h>
#include <sys/time.h>
#include <pthread.h>
#include <shmem.h>
#include <utmpx.h>
#include <unistd.h>
#include <assert.h>
#include <papi.h>

// MAX_ITERATIONS must be less than 65536 to satisfy pgm file format
#define MAX_ITERATIONS 1000
// This PE prints stats and gather image data at the end
#define IMAGE_PE 0
// Number of CPUs on the machine
#define NUM_CPUS 12
// Pin threads to every CPU_PIN_OFFSET core. Setting this to 2 allows for
// portals progress_thread to have a dedicated core
#define CPU_PIN_OFFSET 2

// Default values for width and height
int width = 4096;
int height = 4096;

// An interesting transition point is job_points*sizeof(int) being
// smaller/bigger than max_volatile size for Portals implementation
int job_points = 128;

int me, npes;

// Pointer to shmalloc-ed array for image data
int *imageData;
// Counter used for work load balancing
long nextPoint = 0;
// Stats
long sumTime = 0;
long sumWorkRate = 0;
long sumL1_ICM = 0;
long sumL2_ICM = 0;

pthread_barrier_t fencebar;

static long getTime()
{
    struct timeval tv;

    gettimeofday(&tv, NULL);
    return tv.tv_sec*1e6 + tv.tv_usec;
}

void fileDump(){
    int i, j;
    FILE *fp;
    fp = fopen("mandelbrot.pgm", "w");

    if(NULL == fp) {
        perror ("File open failed!");
        exit(1);
    }

    fprintf(fp,"P2\n");
    fprintf(fp,"%d %d\n", width, height);
    fprintf(fp,"%d\n", MAX_ITERATIONS);

    for (j = 0; j < height; j++) {
        for (i = 0; i < width; i++) {
            fprintf(fp,"%d ", imageData[i + j * width]);
        }
        fprintf(fp,"\n");
    }

    fclose(fp);
}

int computeSingle(int cx, int cy) {

    int i;
    double x, y, x0, y0, x2, y2;

    // cx is in range [-2.5, 1.5] (x range = 4.0)
    // cy is in range [-2.0, 2.0] (y range = 4.0)

    x0 = -2.5 + cx * (4.0 / width);
    y0 = -2.0 + cy * (4.0 / height);

    x = 0;
    y = 0;
    x2 = x*x;
    y2 = y*y;

    for(i = 0; (i < MAX_ITERATIONS) && (x2 + y2 < 4); i++){
        y = 2*x*y + y0;
        x = x2-y2 + x0;
        x2 = x*x;
        y2 = y*y;
    }
    return MAX_ITERATIONS - i;
}

struct th_arg{
    int tid;
    shmem_ctx_t ctx[2];
    int cpu;
};

void *thread_worker(void *arg){
    int tid = ((struct th_arg*)arg)->tid;
    shmem_ctx_t *ctx = ((struct th_arg*)arg)->ctx;
    int my_cpu = ((struct th_arg*)arg)->cpu;
    int i, j, rr_i;
    long timer;
    long work_start, work_start_n, work_end;
    int *pixels[2];
    cpu_set_t cpuset;
    pthread_t thread = pthread_self();
    int rr_pe = me;        // next PE in round-robin scheme
    int rr_pe_n = me;      // next next PE in round-robin scheme
    int pe_pending = npes; // number of PEs with work left
    int *pe_mask;          // flags indicating PEs with work left
    int *pe_ct_max;        // max work counter value for each PE
    int index = 0;         // index for comm/comp overlap
    int index_n = 0;       // next index for comm/comp overlap
    long total_work = 0;   // total amound of work in this thread

    // we are tracking the instruction L1/L2 misses to show why a threaded implementation
    // performs better than a pure PE implementation
    long long counters[4];
    int PAPI_events[] = {PAPI_L1_ICM, PAPI_L2_ICM,
                         PAPI_L1_ICA, PAPI_L2_ICA};

    // Pin each thread
    CPU_ZERO(&cpuset);
    CPU_SET(my_cpu, &cpuset);
    pthread_setaffinity_np(thread, sizeof(cpu_set_t), &cpuset);

    // Malloc local (non-symmetric) buffers
    pixels[0] = malloc(sizeof(int)*job_points);
    pixels[1] = malloc(sizeof(int)*job_points);
    pe_mask = malloc(sizeof(int)*npes);
    pe_ct_max = malloc(sizeof(int)*npes);
    if(NULL == pixels[0] || NULL == pixels[1] || NULL == pe_mask || NULL == pe_ct_max){
        printf("ERROR: malloc failed\n");
        return NULL;
    }

    // Initialize the PE work available flags
    for(i = 0; i < npes; i++) pe_mask[i] = 1;

    // Pre-calculate max value for all remote counters
    for(i = 0; i < npes; i++){
        if(i < npes-1){
            pe_ct_max[i] = (width*height / npes)*(i+1);
        }
        else{
            pe_ct_max[i] = width*height;
        }
    }

    // Synchornize all thraeds on all PEs before starting work
    pthread_barrier_wait(&fencebar);
    if (0 == tid) shmem_barrier_all();
    pthread_barrier_wait(&fencebar);

    // Start PAPI cache miss counters
    PAPI_start_counters(PAPI_events, 4);

    // Initialize pipeline
    rr_pe_n = me;
    index_n = 0;
    shmemx_ctx_long_fadd_nb(ctx[index_n], &nextPoint, job_points, &work_start_n, rr_pe_n);

    // Start work
    timer = getTime();
    while(pe_pending){
        index = index_n;
        index_n ^= 1;

        // Next round-robin PE
        rr_pe = rr_pe_n;
        for(rr_i = 0; rr_i < npes; rr_i++){
            rr_pe_n = ++rr_pe_n % npes;
            if(pe_mask[rr_pe_n]) break;
        }

        // get work start location from remote PEs counter
        shmemx_ctx_quiet(ctx[index]);
        work_start = *((volatile long *)&work_start_n);
        if(rr_i < npes) shmemx_ctx_long_fadd_nb(ctx[index_n], &nextPoint, job_points, &work_start_n, rr_pe_n);

        work_end = work_start + job_points;

        // Check if all work at this PE has been done
        if(work_start >= pe_ct_max[rr_pe]){ // No more work at this PE
            pe_pending--;
            pe_mask[rr_pe] = 0;
            continue;
        }

        if(work_end >= pe_ct_max[rr_pe]){ // This is the last bit of work on this PE
            work_end = pe_ct_max[rr_pe];
            pe_pending--;
            pe_mask[rr_pe] = 0;
        }

        // Do actual compute work
        for(i = work_start, j = 0; i < work_end; i++, j++){
            pixels[index][j] = computeSingle(i%width, i/width);
        }

        // Return the computed image data to the PE responsible for it
        shmemx_ctx_putmem_nb(ctx[index], &imageData[work_start], pixels[index],
                             (work_end-work_start)*sizeof(int), rr_pe);

        total_work += work_end - work_start;
    }
    shmem_quiet();
    timer = getTime() - timer;

    //if(me == 0) printf("completed: %d\n", tid);

    // Read PAPI cache miss counters
    PAPI_read_counters(counters, 4);

    // send stats data to IMAGE_PE
    shmem_long_add(&sumTime, timer, IMAGE_PE);
    shmem_long_add(&sumWorkRate, (long)(total_work / ((double)timer / 1e6) + 0.5), IMAGE_PE);
    shmem_long_add(&sumL1_ICM, counters[0]*1e6/counters[2], IMAGE_PE);
    shmem_long_add(&sumL2_ICM, counters[1]*1e6/counters[3], IMAGE_PE);

    free(pixels[0]);
    free(pixels[1]);
    free(pe_mask);
    free(pe_ct_max);

    return NULL;
}

static void printUsage(){
    printf("USAGE: mandelbrot [options]\n");
    printf("                  -t <num_threads> number of worker threads (def: 1)\n");
    printf("                  -w <width>       width of the mandelbrot domain (def: 4096)\n");
    printf("                  -w <height>      height of the mandelbrot domain (def: 4096)\n");
    printf("                  -j <job_points>  load balancing granularity (def: 256)\n");
    printf("                  -o               output image mandelbrot.pgm (def: off)\n");
    printf("                  -c               use OpenSHMEM contexts (def: off)\n");
    printf("                  -?               prints this message\n");
}

int main(int argc, char** argv) {
    int tl, i, j;
    int c;
    int num_threads = 1;
    int out_file = 0;
    int use_contexts = 0;
    pthread_t *threads;
    struct th_arg *t_arg;
    cpu_set_t cpuset;
    shmem_ctx_t *ctx;
    pthread_t thread = pthread_self();
    int p4_cpu;

    // Must pin the main thread here before calling PtlInit() inside shmemx_init().
    // This is the way we control the pinning of Portals4 progress_thread.
    // see portals4 configuration flag: --enable-progress-thread-polling
    //p4_cpu = CPU_PIN_OFFSET*(getpid()%(NUM_CPUS/CPU_PIN_OFFSET));
    p4_cpu = 3;
    CPU_ZERO(&cpuset);
    CPU_SET(p4_cpu, &cpuset);
    pthread_setaffinity_np(thread, sizeof(cpu_set_t), &cpuset);

    while ((c = getopt (argc, argv, "?cot:w:h:j:")) != -1){
        switch(c){
        case 't':
            num_threads = atoi(optarg);
            break;
        case 'o':
            out_file = 1;
            break;
        case 'c':
            use_contexts = 1;
            break;
        case 'w':
            width = atoi(optarg);
            break;
        case 'h':
            height = atoi(optarg);
            break;
        case 'j':
            job_points = atoi(optarg);
            break;
        case '?':
            printUsage();
            return 0;
        default:
            printUsage();
            return 1;
        }
    }

    // Init PAPI
    PAPI_library_init(PAPI_VER_CURRENT);
    // This is a little sketcy since pthread_self is not guaranteed to return an integer, but it works.
    if(num_threads > 1 && PAPI_thread_init(pthread_self) != PAPI_OK){
        printf("ERROR: PAPI thread init failed\n");
        return 1;
    }

    // Allocate local memory (non-symmetric)
    t_arg = malloc(sizeof(struct th_arg)*num_threads);
    threads = malloc(sizeof(pthread_t)*num_threads);
    ctx = malloc(sizeof(shmem_ctx_t)*2*num_threads);
    if(NULL == t_arg || NULL == threads || NULL == ctx){
        printf("ERROR: malloc failed\n");
        return 1;
    }

    // Start SHMEM (allso calls PtlInit())
    if(num_threads > 1){
        shmemx_init(SHMEM_THREAD_MULTIPLE, &tl);
        // If OpenSHMEM doesn't support multithreading, exit gracefully
        if (SHMEM_THREAD_MULTIPLE != tl) {
            printf("Warning: Exiting because threading is disabled, tested nothing\n");
            shmemx_finalize();
            return 0;
        }
    }
    else{
        shmemx_init(SHMEM_THREAD_SINGLE, &tl);
    }

    // Allocate symmtric memory for image data
    // For shmalloc of more than 512MB, set SMA_SYMMETRIC_SIZE to increase symmetric heap size.
    imageData = shmalloc(sizeof(int)*width*height);
    if(NULL == imageData){
        printf("ERROR: shmalloc failed\n");
        return 1;
    }

    me = shmem_my_pe();
    npes = shmem_n_pes();

    // Divide work balancing counter between all PEs
    nextPoint = (width*height / npes) * me;

    // Create SHMEM contexts
    shmemx_ctx_create(2*num_threads, SHMEM_THREAD_SINGLE, ctx);

    // Initalize barrier for thread synchronization inside PE
    pthread_barrier_init(&fencebar, NULL, num_threads);

    // Ready to go...
    if (me == IMAGE_PE) printf("Starting benchmark on %d PEs, %d threads/PE\n", npes, num_threads);

    // Initialize worker threads
    for (i = 0; i < num_threads; i++) {
        int err;
        t_arg[i].tid = i;
        if(1 == use_contexts){
            t_arg[i].ctx[0] = ctx[i];
            t_arg[i].ctx[1] = ctx[i+1];
        }
        else{
            t_arg[i].ctx[0] = SHMEM_CTX_WORLD;
            t_arg[i].ctx[1] = SHMEM_CTX_WORLD;
        }
        t_arg[i].cpu = (p4_cpu+i+1)%NUM_CPUS;
        err = pthread_create(&threads[i], NULL, thread_worker, (void*) &t_arg[i]);
        assert(0 == err);
    }

    // Wait for local threads to finish work
    for (i = 0; i < num_threads; i++) {
        int err;
        err = pthread_join(threads[i], NULL);
        assert(0 == err);
    }

    // Wait for all PEs to finish work
    shmem_barrier_all();

    // Collect all image data on IMAGE_PE and dump it to a file
    if(1 == out_file){
        if(me != IMAGE_PE){
            if(me < npes-1){
                shmem_putmem(&imageData[(width*height / npes) * me],
                             &imageData[(width*height / npes) * me],
                             (width*height / npes)*sizeof(int), IMAGE_PE);
            }
            else{
                shmem_putmem(&imageData[(width*height / npes) * me],
                             &imageData[(width*height / npes) * me],
                             (width*height - (width*height / npes) * me)*sizeof(int),  IMAGE_PE);
            }
        }
        shmem_barrier_all();
        if(me == IMAGE_PE){
            fileDump();
        }
    }

    // Print stats
    if(me == IMAGE_PE){
        printf("Average thread L1 instruction misses(%%): %f\n", (double)sumL1_ICM/(npes*num_threads)/1e4);
        printf("Average thread L2 instruction misses(%%): %f\n", (double)sumL2_ICM/(npes*num_threads)/1e4);
        printf("Total cumulative runtime (sec)         : %f\n", (double)sumTime/1e6);
        printf("Average thread work rate (points/sec)  : %f\n", (double)sumWorkRate/(npes*num_threads));
        printf("Average thread work runtime (sec)      : %f\n", ((double)sumTime/1e6)/(npes*num_threads));
        printf("Total work rate (points/sec)           : %e\n",
               width*height/(((double)sumTime/1e6)/(npes*num_threads)));
    }

    // Cleanup
    pthread_barrier_destroy(&fencebar);
    shfree(imageData);
    shmemx_ctx_destroy(2*num_threads, ctx);
    shmemx_finalize();
    free(t_arg);
    free(threads);
    free(ctx);
    return 0;
}
