#ifndef SOS_MPL
#define SOS_MPL

#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#define ATTRIBUTE __attribute__
#define MPI_MAX_PORT_NAME 256

#define MPL_snprintf(...)     \
    ({ snprintf(__VA_ARGS__); \
        0;                    \
    })

#define MPL_strncpy(...)                                \
    ({ strncpy(__VA_ARGS__);                            \
        0;                                              \
    })

#define MPL_exit     exit

#define MPL_strnapp(...)                    \
    ({ strncat(__VA_ARGS__);                \
        0;                                  \
    })

#define MPL_internal_error_printf(...) \
    fprintf(stderr, __VA_ARGS__);

#endif /* SOS_MPL */
