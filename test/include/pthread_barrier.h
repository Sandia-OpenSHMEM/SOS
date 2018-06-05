/* Apple currently doesn't provide a pthread_barrier.  This file implements the
 * functions needed for threaded SHMEM tests using the other parts of the
 * PThread API. */
#ifdef __APPLE__

#ifndef PTHREAD_BARRIER_H_
#define PTHREAD_BARRIER_H_

#include <pthread.h>
#include <errno.h>

#define PTHREAD_BARRIER_SERIAL_THREAD -1

typedef int pthread_barrierattr_t;

typedef struct {
    int cur_count;
    int count;
    pthread_cond_t cond;
    pthread_mutex_t mutex;
} pthread_barrier_t;


static int pthread_barrier_init(pthread_barrier_t *barrier, const pthread_barrierattr_t *attr, unsigned int count) {
    int err;

    err = pthread_mutex_init(&barrier->mutex, 0);
    if (err) return err;
    err = pthread_cond_init(&barrier->cond, 0);
    if (err) {
        pthread_mutex_destroy(&barrier->mutex);
        return err;
    }

    barrier->cur_count = barrier->count = count;

    return 0;
}


static int pthread_barrier_destroy(pthread_barrier_t *barrier) {
    pthread_cond_destroy(&barrier->cond);
    pthread_mutex_destroy(&barrier->mutex);
    return 0;
}


static int pthread_barrier_wait(pthread_barrier_t *barrier) {
    int ret = 0;

    pthread_mutex_lock(&barrier->mutex);

    if (barrier->cur_count <= 0) return EINVAL;
    else {
        barrier->cur_count--;

        if (barrier->cur_count == 0) {
            ret = PTHREAD_BARRIER_SERIAL_THREAD;
            barrier->cur_count = barrier->count;
            pthread_cond_broadcast(&barrier->cond);
        } else {
            pthread_cond_wait(&barrier->cond, &barrier->mutex);
        }
    }

    pthread_mutex_unlock(&barrier->mutex);
    return ret;
}

#endif /* PTHREAD_BARRIER_H_ */
#endif /* __APPLE__ */
