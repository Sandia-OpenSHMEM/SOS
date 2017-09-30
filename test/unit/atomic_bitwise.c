/*
 *  Copyright (c) 2017 Rice University. All rights reserved.
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
 * Test various bitwise atomics. These tests assume that an unsigned int is at
 * least 4 bytes.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <string.h>
#include <shmem.h>
#include <assert.h>

#define XOR_MASK 0xff
#define XOR_MASK2 0xffff
#define OR_MASK 0xff
#define OR_MASK2 0xff0000
#define AND_MASK 0xffff00
#define AND_MASK2 0xff0000

unsigned int shared_uint = 0;
unsigned long shared_ulong = 0;
unsigned long long shared_ulonglong = 0;
int32_t shared_int32 = 0;
int64_t shared_int64 = 0;
uint32_t shared_uint32 = 0;
uint64_t shared_uint64 = 0;

int
main(int argc, char* argv[])
{
    shmem_init();
    int my_rank = shmem_my_pe();
    int num_ranks = shmem_n_pes();
    if (num_ranks == 1) {
        fprintf(stderr, "ERR - Requires > 1 PEs\n");
        shmem_finalize();
        return 0;
    }
    if (num_ranks % 2 != 0) {
        fprintf(stderr, "ERR - Requires even number of PEs\n");
        shmem_finalize();
        return 0;
    }
    if (sizeof(unsigned int) < 4) {
        fprintf(stderr, "ERR - Expected ints to be at least 4 bytes\n");
        shmem_finalize();
        return 0;
    }

    int neighbor;
    if ((my_rank % 2) == 0) {
        neighbor = my_rank + 1;
    } else {
        neighbor = my_rank - 1;
    }

    /*
     * Test non-fetching XOR. This should result in each shared variable having
     * its value set to XOR_MASK. All shared values are initialized to zero.
     */
    shmem_uint_atomic_xor(&shared_uint, (unsigned int)XOR_MASK, neighbor);
    shmem_ulong_atomic_xor(&shared_ulong, (unsigned long)XOR_MASK, neighbor);
    shmem_ulonglong_atomic_xor(&shared_ulonglong, (unsigned long long)XOR_MASK, neighbor);
    shmem_int32_atomic_xor(&shared_int32, (int32_t)XOR_MASK, neighbor);
    shmem_int64_atomic_xor(&shared_int64, (int64_t)XOR_MASK, neighbor);
    shmem_uint32_atomic_xor(&shared_uint32, (uint32_t)XOR_MASK, neighbor);
    shmem_uint64_atomic_xor(&shared_uint64, (uint64_t)XOR_MASK, neighbor);

    shmem_barrier_all();

    assert(shared_uint == XOR_MASK);
    assert(shared_ulong == XOR_MASK);
    assert(shared_ulonglong == XOR_MASK);
    assert(shared_int32 == XOR_MASK);
    assert(shared_int64 == XOR_MASK);
    assert(shared_uint32 == XOR_MASK);
    assert(shared_uint64 == XOR_MASK);

    shmem_barrier_all();

    /*
     * Test fetching XOR. Prior to this block, all shared variables contain the
     * value XOR_MASK. Here, we XOR them with XOR_MASK2.
     */
    unsigned int fetched_uint = shmem_uint_atomic_fetch_xor(&shared_uint, (unsigned int)XOR_MASK2, neighbor);
    unsigned long fetched_ulong = shmem_ulong_atomic_fetch_xor(&shared_ulong, (unsigned long)XOR_MASK2, neighbor);
    unsigned long long fetched_ulonglong = shmem_ulonglong_atomic_fetch_xor(&shared_ulonglong, (unsigned long long)XOR_MASK2, neighbor);
    int32_t fetched_int32 = shmem_int32_atomic_fetch_xor(&shared_int32, (int32_t)XOR_MASK2, neighbor);
    int64_t fetched_int64 = shmem_int64_atomic_fetch_xor(&shared_int64, (int64_t)XOR_MASK2, neighbor);
    uint32_t fetched_uint32 = shmem_uint32_atomic_fetch_xor(&shared_uint32, (uint32_t)XOR_MASK2, neighbor);
    uint64_t fetched_uint64 = shmem_uint64_atomic_fetch_xor(&shared_uint64, (uint64_t)XOR_MASK2, neighbor);

    shmem_barrier_all();

    assert(fetched_uint == XOR_MASK); assert(shared_uint == ((unsigned int)XOR_MASK ^ (unsigned int)XOR_MASK2));
    assert(fetched_ulong == XOR_MASK); assert(shared_ulong == ((unsigned long)XOR_MASK ^ (unsigned long)XOR_MASK2));
    assert(fetched_ulonglong == XOR_MASK); assert(shared_ulonglong == ((unsigned long long)XOR_MASK ^ (unsigned long long)XOR_MASK2));
    assert(fetched_int32 == XOR_MASK); assert(shared_int32 == ((int32_t)XOR_MASK ^ (int32_t)XOR_MASK2));
    assert(fetched_int64 == XOR_MASK); assert(shared_int64 == ((int64_t)XOR_MASK ^ (int64_t)XOR_MASK2));
    assert(fetched_uint32 == XOR_MASK); assert(shared_uint32 == ((uint32_t)XOR_MASK ^ (uint32_t)XOR_MASK2));
    assert(fetched_uint64 == XOR_MASK); assert(shared_uint64 == ((uint64_t)XOR_MASK ^ (uint64_t)XOR_MASK2));

    shmem_barrier_all();

    /*
     * Test non-fetching OR. Prior to this block, all shared variables have the
     * value XOR_MASK ^ XOR_MASK2 (i.e. 0xff00). Here, we do a bitwise OR with
     * OR_MASK (i.e. 0xff).
     */
    shmem_uint_atomic_or(&shared_uint, (unsigned int)OR_MASK, neighbor);
    shmem_ulong_atomic_or(&shared_ulong, (unsigned long)OR_MASK, neighbor);
    shmem_ulonglong_atomic_or(&shared_ulonglong, (unsigned long long)OR_MASK, neighbor);
    shmem_int32_atomic_or(&shared_int32, (int32_t)OR_MASK, neighbor);
    shmem_int64_atomic_or(&shared_int64, (int64_t)OR_MASK, neighbor);
    shmem_uint32_atomic_or(&shared_uint32, (uint32_t)OR_MASK, neighbor);
    shmem_uint64_atomic_or(&shared_uint64, (uint64_t)OR_MASK, neighbor);

    shmem_barrier_all();

    assert(shared_uint == 0xffff);
    assert(shared_ulong == 0xffff);
    assert(shared_ulonglong == 0xffff);
    assert(shared_int32 == 0xffff);
    assert(shared_int64 == 0xffff);
    assert(shared_uint32 == 0xffff);
    assert(shared_uint64 == 0xffff);

    shmem_barrier_all();

    /*
     * Test fetching OR. Prior to this block, all shared variables have the
     * value (XOR_MASK ^ XOR_MASK2) | OR_MASK (i.e. 0xffff). Here we OR with
     * OR_MASK2 (i.e. 0xff0000).
     */
    fetched_uint = shmem_uint_atomic_fetch_or(&shared_uint, (unsigned int)OR_MASK2, neighbor);
    fetched_ulong = shmem_ulong_atomic_fetch_or(&shared_ulong, (unsigned long)OR_MASK2, neighbor);
    fetched_ulonglong = shmem_ulonglong_atomic_fetch_or(&shared_ulonglong, (unsigned long long)OR_MASK2, neighbor);
    fetched_int32 = shmem_int32_atomic_fetch_or(&shared_int32, (int32_t)OR_MASK2, neighbor);
    fetched_int64 = shmem_int64_atomic_fetch_or(&shared_int64, (int64_t)OR_MASK2, neighbor);
    fetched_uint32 = shmem_uint32_atomic_fetch_or(&shared_uint32, (uint32_t)OR_MASK2, neighbor);
    fetched_uint64 = shmem_uint64_atomic_fetch_or(&shared_uint64, (uint64_t)OR_MASK2, neighbor);

    shmem_barrier_all();

    assert(fetched_uint == 0xffff); assert(shared_uint == (unsigned int)0xffffff);
    assert(fetched_ulong == 0xffff); assert(shared_ulong == (unsigned long)0xffffff);
    assert(fetched_ulonglong == 0xffff); assert(shared_ulonglong == (unsigned long long)0xffffff);
    assert(fetched_int32 == 0xffff); assert(shared_int32 == (int32_t)0xffffff);
    assert(fetched_int64 == 0xffff); assert(shared_int64 == (int64_t)0xffffff);
    assert(fetched_uint32 == 0xffff); assert(shared_uint32 == (uint32_t)0xffffff);
    assert(fetched_uint64 == 0xffff); assert(shared_uint64 == (uint64_t)0xffffff);

    shmem_barrier_all();

    /*
     * Test non-fetching AND. All shared variables store the value 0xffffff.
     * Here, we AND with 0xffff00.
     */
    shmem_uint_atomic_and(&shared_uint, (unsigned int)AND_MASK, neighbor);
    shmem_ulong_atomic_and(&shared_ulong, (unsigned long)AND_MASK, neighbor);
    shmem_ulonglong_atomic_and(&shared_ulonglong, (unsigned long long)AND_MASK, neighbor);
    shmem_int32_atomic_and(&shared_int32, (int32_t)AND_MASK, neighbor);
    shmem_int64_atomic_and(&shared_int64, (int64_t)AND_MASK, neighbor);
    shmem_uint32_atomic_and(&shared_uint32, (uint32_t)AND_MASK, neighbor);
    shmem_uint64_atomic_and(&shared_uint64, (uint64_t)AND_MASK, neighbor);

    shmem_barrier_all();

    assert(shared_uint      == AND_MASK);
    assert(shared_ulong     == AND_MASK);
    assert(shared_ulonglong == AND_MASK);
    assert(shared_int32     == AND_MASK);
    assert(shared_int64     == AND_MASK);
    assert(shared_uint32    == AND_MASK);
    assert(shared_uint64    == AND_MASK);

    shmem_barrier_all();

    /*
     * Test fetching AND. All shared variables store the value 0xffff00. Here,
     * we AND with 0xff0000.
     */
    fetched_uint = shmem_uint_atomic_fetch_and(&shared_uint, (unsigned int)AND_MASK2, neighbor);
    fetched_ulong = shmem_ulong_atomic_fetch_and(&shared_ulong, (unsigned long)AND_MASK2, neighbor);
    fetched_ulonglong = shmem_ulonglong_atomic_fetch_and(&shared_ulonglong, (unsigned long long)AND_MASK2, neighbor);
    fetched_int32 = shmem_int32_atomic_fetch_and(&shared_int32, (int32_t)AND_MASK2, neighbor);
    fetched_int64 = shmem_int64_atomic_fetch_and(&shared_int64, (int64_t)AND_MASK2, neighbor);
    fetched_uint32 = shmem_uint32_atomic_fetch_and(&shared_uint32, (uint32_t)AND_MASK2, neighbor);
    fetched_uint64 = shmem_uint64_atomic_fetch_and(&shared_uint64, (uint64_t)AND_MASK2, neighbor);

    shmem_barrier_all();

    assert(fetched_uint      == AND_MASK); assert(shared_uint == (unsigned int)AND_MASK2);
    assert(fetched_ulong     == AND_MASK); assert(shared_ulong == (unsigned long)AND_MASK2);
    assert(fetched_ulonglong == AND_MASK); assert(shared_ulonglong == (unsigned long long)AND_MASK2);
    assert(fetched_int32     == AND_MASK); assert(shared_int32 == (int32_t)AND_MASK2);
    assert(fetched_int64     == AND_MASK); assert(shared_int64 == (int64_t)AND_MASK2);
    assert(fetched_uint32    == AND_MASK); assert(shared_uint32 == (uint32_t)AND_MASK2);
    assert(fetched_uint64    == AND_MASK); assert(shared_uint64 == (uint64_t)AND_MASK2);

    shmem_finalize();

    if (my_rank == 0) {
        printf("Passed!\n");
    }

    return 0;
}
