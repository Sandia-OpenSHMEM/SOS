
#include "config.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "mpi.h"


#include "runtime.h"
#include "shmem_internal.h"
#include "uthash.h"


static int rank = -1;
static int size = 0;
static char *kvs_name, *kvs_key, *kvs_value;
static int max_name_len, max_key_len, max_val_len;
static MPI_Group SHMEM_RUNTIME_WORLD;
static char** container;
static char** my_table;
static int length = 0;

#define SINGLETON_KEY_LEN 128
#define SINGLETON_VAL_LEN 256

typedef struct {
    char key[SINGLETON_KEY_LEN];
    char val[SINGLETON_VAL_LEN];
    UT_hash_handle hh;
} singleton_kvs_t;

singleton_kvs_t *singleton_kvs = NULL;

static int
encode(const void *inval, int invallen, char *outval, int outvallen)
{
    static unsigned char encodings[] = {
        '0','1','2','3','4','5','6','7', 
        '8','9','a','b','c','d','e','f' };
    int i;

    if (invallen * 2 + 1 > outvallen) {
        return 1;
    }

    for (i = 0; i < invallen; i++) {
        outval[2 * i] = encodings[((unsigned char *)inval)[i] & 0xf];
        outval[2 * i + 1] = encodings[((unsigned char *)inval)[i] >> 4];
    }

    outval[invallen * 2] = '\0';

    return 0;
}

static int
decode(const char *inval, void *outval, size_t outvallen)
{
    size_t i;
    char *ret = (char*) outval;

    if (outvallen != strlen(inval) / 2) {
        return 1;
    }

    for (i = 0 ; i < outvallen ; ++i) {
        if (*inval >= '0' && *inval <= '9') {
            ret[i] = *inval - '0';
        } else {
            ret[i] = *inval - 'a' + 10;
        }
        inval++;
        if (*inval >= '0' && *inval <= '9') {
            ret[i] |= ((*inval - '0') << 4);
        } else {
            ret[i] |= ((*inval - 'a' + 10) << 4);
        }
        inval++;
    }

    return 0;
}

int
shmem_runtime_init(void)
{
    int initialized;
    if (MPI_SUCCESS != MPI_Initialized(&initialized)) {
        return 1;
    }
    if (!initialized) {
        if (MPI_SUCCESS != MPI_Init(NULL, NULL)) {
            
            return 2;
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

    if (size > 1) { 

    	max_name_len = MPI_MAX_OBJECT_NAME;
        max_key_len = MPI_MAX_OBJECT_NAME;
        max_val_len = MPI_MAX_OBJECT_NAME;

        //WARN: THIS ASSUMES THAT YOU CAN ONLY HAVE 10 KEY VALUE PAIRS
        container = (char**) malloc(sizeof(char) * max_name_len * size * 2 * 10);
        if(NULL == container) return 6;
        my_table = (char**) malloc(sizeof(char) * max_name_len * 2 * 10);
        if(NULL == my_table) return 7;

    }
    else {
        /* Use a local KVS for singleton runs */
        max_key_len = SINGLETON_KEY_LEN;
        max_val_len = SINGLETON_VAL_LEN;
        kvs_name = NULL;
        max_name_len = 0;
    }

    kvs_key = (char*) malloc(max_key_len);
    if (NULL == kvs_key) return 8;

    kvs_value = (char*) malloc(max_val_len);
    if (NULL == kvs_value) return 9;

    return 0;
}

int
shmem_runtime_fini(void)
{
    int finalized = 0;
    MPI_Finalized(&finalized);
    if(!finalized){
        free(container);
        MPI_Finalize();
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
    /* Use singleton KVS for single process jobs */
    if (size == 1){
        return 0;
    }
    
    for(int i = 0; i < length; i++)
    {
        *(container + i) = (char*)malloc(max_val_len * sizeof(char) * size);

        if (MPI_SUCCESS != MPI_Allgather(*(my_table+i), max_val_len, MPI_CHAR, *(container + i), max_val_len, MPI_CHAR, SHMEM_RUNTIME_WORLD)) {
            return 2;   
        }
        if (MPI_SUCCESS != MPI_Barrier(SHMEM_RUNTIME_WORLD)) {
            return 6;
        }
    }
    return 0;
}


int 
shmem_runtime_put(char *key, void *value, size_t valuelen)
{
    snprintf(kvs_key, max_key_len, "shmem-%lu-%s", (long unsigned) rank, key);
    if (0 != encode(value, valuelen, kvs_value, max_val_len)) {
        return 1;
    }

    if (size == 1) {
        singleton_kvs_t *e = malloc(sizeof(singleton_kvs_t));
        if (e == NULL) return 3;
        strncpy(e->key, kvs_key, max_key_len);
        strncpy(e->val, kvs_value, max_val_len);
        HASH_ADD_STR(singleton_kvs, key, e);
    } else {
        if(length < 20){
            *(my_table + length) = key;
            length++;
            *(my_table + length) = kvs_value;
            length++;
        } else {
            return 20;
        }
    }

    return 0;
}

int
shmem_runtime_get(int pe, char *key, void *value, size_t valuelen)
{
	snprintf(kvs_key, max_key_len, "shmem-%lu-%s", (long unsigned) pe, key);
    if (size == 1) {
        singleton_kvs_t *e;
        HASH_FIND_STR(singleton_kvs, kvs_key, e);
        if (e == NULL)
            return 3;
        kvs_value = e->val;
    }
    else {
        for(int i = 0; i < 2 * length; i += 2){
            if(strcmp(key, (*(container+i) + pe * max_val_len)) == 0){
                kvs_value = (*(container+i + 1) + pe * max_val_len);
                break;
            }
        }
    }
    if (0 != decode(kvs_value, value, valuelen)) {
        return 2;
    }
    return 0;
}

void
shmem_runtime_barrier(void)
{
    MPI_Barrier(SHMEM_RUNTIME_WORLD);
}
