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
 */

#include <shmem.h>
#include <shmemx.h>
#include <pthread.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define CHECK_ERROR(num,...) do {  \
    if((num)) {                    \
      fprintf(stderr,__VA_ARGS__); \
      return (num);                \
    }                              \
  } while(0)

#define NUM_UNSAFE_DOM 0
#define NUM_DOMAINS    64
#define NUM_CONTEXTS   128

#define NUM_THREADS (4*(NUM_CONTEXTS))

typedef struct {
  int idx;
  unsigned seed;
  long received;
  long can_send;
  long rec_val;
  long send_val;
  int stride;
} channel;

shmemx_domain_t domains[NUM_DOMAINS];
shmemx_ctx_t contexts[NUM_CONTEXTS];
int ctx_is_safe[NUM_CONTEXTS];
pthread_mutex_t ctx_locks[NUM_CONTEXTS];
channel* channels;
int me,n_pes,n_threads;
int max_steps;

// naive LCM of 1..numPes
static int get_max_steps(int numPes) {
  int i;
  int ret = 0;
  int done = 0;
  while(!done) {
    ++ret;
    done = 1;
    for(i = 1; i < numPes; ++i) {
      if((ret*i)%numPes != 0) {
        done = 0;
        break;
      }
    }
  }

  return ret;
}

static void* runchannel(void* chan) {
  channel *c = chan;
  int nextpe = (me + c->stride)%n_pes;
  int prevpe = (me + n_pes - c->stride)%n_pes;
  /* printf("[%d:%d] prevpe = %d, nextpe = %d\n",me,c->idx,prevpe, */
  /*     nextpe); */
  int i;

  for(i = 0; i < max_steps; ++i) {
    unsigned ctxind = ((unsigned)rand_r(&c->seed))%NUM_CONTEXTS;
    /* printf("[%d:%d] %d\n",me,c->idx,ctxind); */
    int safe = ctx_is_safe[ctxind];
    if(!safe) {
      pthread_mutex_lock(&ctx_locks[ctxind]);
    }
    shmemx_ctx_long_p(&c->can_send,1L,prevpe,contexts[ctxind]);
    if(!safe) {
      pthread_mutex_unlock(&ctx_locks[ctxind]);
    }

    shmem_wait_until(&c->can_send,SHMEM_CMP_NE,0L);
    c->can_send = 0;

    ctxind = ((unsigned)rand_r(&c->seed))%NUM_CONTEXTS;
    safe = ctx_is_safe[ctxind];

    if(!safe) {
      pthread_mutex_lock(&ctx_locks[ctxind]);
    }
    shmemx_ctx_long_p(&c->rec_val,c->send_val,nextpe,
        contexts[ctxind]);
    shmemx_ctx_fence(contexts[ctxind]);
    shmemx_ctx_long_p(&c->received,1L,nextpe,contexts[ctxind]);
    if(!safe) {
      pthread_mutex_unlock(&ctx_locks[ctxind]);
    }

    shmem_wait_until(&c->received,SHMEM_CMP_NE,0L);
    c->received = 0;
    c->send_val = c->rec_val;
  }

  return 0;
}

int main(int argc, char* argv[]) {
    int i;
    for(i = 0; i < NUM_CONTEXTS; ++i) {
      int err;
      err = pthread_mutex_init(&ctx_locks[i],NULL);
      assert(!err);
    }

    int err;
    int errors = 0;
    int tl = SHMEMX_THREAD_MULTIPLE, tl_provided;

    shmemx_init_thread(tl,&tl_provided);
    assert(tl == tl_provided);

    n_pes = shmem_n_pes();
    me = shmem_my_pe();

    srand(1+me);

    n_threads = 4 * n_pes;
    channels = shmem_malloc(n_threads*sizeof(channel));
    memset(channels,0,n_threads*sizeof(channel));

    long* vals = malloc(sizeof(long)*n_threads);
    for(i = 0; i < n_threads; ++i) {
      channels[i].idx = i;
      channels[i].received = 0;
      channels[i].can_send = 0;
      channels[i].rec_val = 0;
      channels[i].send_val = vals[i] = rand();

      channels[i].stride = (i)%(n_pes-1) + 1;
      channels[i].seed = 1 + rand() + i;
    }

    max_steps = get_max_steps(n_pes);

    /* printf("[%d] Making %d/%d unsafe domains\n",me,NUM_UNSAFE_DOM, */
    /*     NUM_DOMAINS); */
    err = shmemx_domain_create(SHMEMX_THREAD_SINGLE,NUM_UNSAFE_DOM,
        domains);
    CHECK_ERROR(err,"Failed to create unsafe domains\n");

    /* printf("[%d] Making %d/%d safe domains\n",me,NUM_DOMAINS-NUM_UNSAFE_DOM, */
    /*     NUM_DOMAINS); */
    err = shmemx_domain_create(SHMEMX_THREAD_MULTIPLE,
        NUM_DOMAINS-NUM_UNSAFE_DOM,domains+NUM_UNSAFE_DOM);
    CHECK_ERROR(err,"Failed to create safe domains\n");

    for(i = 0; i < NUM_CONTEXTS; ++i) {
      int domind = i % NUM_DOMAINS;
      ctx_is_safe[i] = (domind >= NUM_UNSAFE_DOM);
      err = shmemx_ctx_create(domains[domind],&contexts[i]);
      /* printf("[%d]   ctx %d (%d) on domain %d (%d) is %s\n",me,i, */
      /*     contexts[i], domind,domains[domind], */
      /*     (ctx_is_safe[i] ? "safe" : "unsafe")); */
      CHECK_ERROR(err,"Failed to create ctx %d\n",i);
    }

    shmemx_domain_destroy(NUM_DOMAINS,domains);

    pthread_t* threads = calloc(sizeof(pthread_t),n_threads);
    for(i = 0; i < n_threads; ++i) {
      int err;
      err = pthread_create(&threads[i],NULL,runchannel,&channels[i]);
      assert(0 == err);
    }

    for (i = 0; i < n_threads; i++) {
        int err;
        err = pthread_join(threads[i], NULL);
        assert(0 == err);
    }
    free(threads);

    for(i = 0; i < NUM_CONTEXTS; ++i) {
      shmemx_ctx_quiet(contexts[i]);
      shmemx_ctx_destroy(contexts[i]);
      int err;
      err = pthread_mutex_destroy(&ctx_locks[i]);
      assert(!err);
    }

    for(i = 0; i < n_threads; ++i) {
      if(vals[i] != channels[i].rec_val) {
        ++errors;
        fprintf(stderr,
            "PE %d, Thread %d (stride %d): got %ld, expected %ld\n",
            me,i,channels[i].stride,channels[i].rec_val,vals[i]);
      }
    }
    shmem_free(channels);
    free(vals);

    shmem_finalize();

    return errors;
}

