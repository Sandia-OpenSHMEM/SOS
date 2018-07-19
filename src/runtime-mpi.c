
#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mpi.h"


#include "runtime.h"
#include "shmem_internal.h"
#include "uthash.h"

#define MAX_KV_COUNT 20
#define MAX_KV_LENGTH 64

static int rank = -1;
static int size = 0;
static MPI_Comm SHMEM_RUNTIME_WORLD;
static int length = 0;

char* kv_store_me;
char* kv_store_all;


int
shmem_runtime_init(void)
{
    int initialized;
    if (MPI_SUCCESS != MPI_Initialized(&initialized)) {
        return 1;
    }
    if (!initialized) {
        if(getenv("MPI_THREAD_LEVEL") == NULL){
            if (MPI_SUCCESS != MPI_Init(NULL, NULL)) {
                return 2;
            }
        }
        else{
            int provided = 0;
            char* thread_level = getenv("MPI_THREAD_LEVEL");
            int int_thread_level = 0;
            if(strcmp(thread_level, "MPI_THREAD_SINGLE") == 0){
                int_thread_level = MPI_THREAD_SINGLE;
            }
            else if(strcmp(thread_level, "MPI_THREAD_FUNNELED") == 0){
                int_thread_level   = MPI_THREAD_FUNNELED;
            }
            else if(strcmp(thread_level, "MPI_THREAD_SERIALIZED") == 0){
                int_thread_level = MPI_THREAD_SERIALIZED;
            }
            else if(strcmp(thread_level, "MPI_THREAD_MULTIPLE") == 0){
                int_thread_level = MPI_THREAD_MULTIPLE;
            }
            else{
                return 16;
            }
            if(MPI_SUCCESS != MPI_Init_thread(NULL, NULL, int_thread_level, &provided)){
                return 2;
            }
        }
    }
    if(MPI_SUCCESS != MPI_Comm_dup(MPI_COMM_WORLD, &SHMEM_RUNTIME_WORLD)){
    	return 3;
    }
    if (MPI_SUCCESS !=  MPI_Comm_rank(SHMEM_RUNTIME_WORLD, &rank)) {
        return 4;
    }

    if (MPI_SUCCESS != MPI_Comm_size(SHMEM_RUNTIME_WORLD, &size)) {
        return 5;
    }

    kv_store_me = (char*)malloc(MAX_KV_COUNT * sizeof(char)* MAX_KV_LENGTH);
    
    return 0;
}

int
shmem_runtime_fini(void)
{
    int finalized = 0;
    MPI_Finalized(&finalized);
    if(!finalized){
        MPI_Finalize();
    }

    free(kv_store_all);
    if(size != 1){
        free(kv_store_me);
    }

    return 0;
}

void
shmem_runtime_abort(int exit_code, const char msg[])
{

#ifdef HAVE___BUILTIN_TRAP
    if (shmem_internal_params.TRAP_ON_ABORT)
        __builtin_trap();
#endif

    if (size == 1) {
        fprintf(stderr, "%s\n", msg);
        exit(exit_code);
    }

    MPI_Abort(SHMEM_RUNTIME_WORLD, exit_code);

    /* MPI_Abort should not return */
    abort();
}


int
shmem_runtime_get_rank(void)
{
    return rank;
}


int
shmem_runtime_get_size(void)
{
    return size;
}

int
shmem_runtime_exchange(void)
{
    if (size == 1){
        kv_store_all = kv_store_me;
        return 0;
    }

    int chunkSize = length * sizeof(char) * MAX_KV_LENGTH;
    kv_store_all = (char*)malloc(chunkSize * size);

    if (MPI_SUCCESS != MPI_Allgather(kv_store_me, chunkSize, MPI_CHAR, kv_store_all, chunkSize, MPI_CHAR, SHMEM_RUNTIME_WORLD)) {
        return 2;   
    }
    if (MPI_SUCCESS != MPI_Barrier(SHMEM_RUNTIME_WORLD)) {
        return 6;
    }
    // if(rank == 0){
    //     for(int i = 0; i < length*size; i++){
    //         printf("%s, %s, strcmp: %i\n", (kv_store_all + i * MAX_KV_LENGTH), (kv_store_me + i * MAX_KV_LENGTH), strcmp((kv_store_all + i * MAX_KV_LENGTH), kv_store_me + i * MAX_KV_LENGTH));
    //     }
    //     printf("\n");
    // }
    if (MPI_SUCCESS != MPI_Barrier(SHMEM_RUNTIME_WORLD)) {
        return 6;
    }
    
    return 0;
}


int 
shmem_runtime_put(char *key, void *value, size_t valuelen)
{
    if(length < MAX_KV_COUNT){

        memcpy((kv_store_me + length * MAX_KV_LENGTH), key, MAX_KV_LENGTH);
        length++;
        memcpy((kv_store_me + length * MAX_KV_LENGTH), value, MAX_KV_LENGTH);
        length++;

    } else {
        return MAX_KV_COUNT;
    }

    return 0;
}

int
shmem_runtime_get(int pe, char *key, void *value, size_t valuelen)
{
    int flag = 0;
    for(int i = pe * length; i < length * size; i+= 2){
        if(strcmp((kv_store_all + i * MAX_KV_LENGTH), key) == 0){
            memcpy(value, (kv_store_all + (i+1) * MAX_KV_LENGTH), valuelen);
            // printf("Value found: %s for pe %i\n", (kv_store_all + (i+1) * MAX_KV_LENGTH ), pe);
            // if(pe == rank){
            //     printf("\nstrcmp: %i\n", strcmp((kv_store_me + MAX_KV_LENGTH), ((kv_store_all + (i+1) * MAX_KV_LENGTH))));
            // }
            flag = 1;
            break;
        }
    }
    if(0 == flag){
        return 3;
    }

    MPI_Barrier(SHMEM_RUNTIME_WORLD);
    
    return 0;
}

void
shmem_runtime_barrier(void)
{
    MPI_Barrier(SHMEM_RUNTIME_WORLD);
}
