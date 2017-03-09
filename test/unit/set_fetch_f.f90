!
!  Copyright 2011 Sandia Corporation. Under the terms of Contract
!  DE-AC04-94AL85000 with Sandia Corporation, the U.S.  Government
!  retains certain rights in this software.
!
!   Copyright (c) 2017 Intel Corporation. All rights reserved.
!   This software is available to you under the BSD license below:
!
!       Redistribution and use in source and binary forms, with or
!       without modification, are permitted provided that the following
!       conditions are met:
!
!       - Redistributions of source code must retain the above
!         copyright notice, this list of conditions and the following
!         disclaimer.
!
!       - Redistributions in binary form must reproduce the above
!         copyright notice, this list of conditions and the following
!         disclaimer in the documentation and/or other materials
!         provided with the distribution.
!
!  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
!  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
!  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
!  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
!  BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
!  ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
!  CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
!  SOFTWARE.
!

program set_fetch
    implicit none
    include 'shmem.fh'

    integer*4           :: var_i4, val_i4
    integer*8           :: var_i8, val_i8
    real*4              :: var_r4, val_r4
    real*8              :: var_r8, val_r8

    integer npes, me

    common /symmetricvars/ var_i4, var_r4, var_i8, var_r8

    integer   shmem_my_pe, shmem_n_pes
    integer*4 shmem_int4_fetch
    integer*8 shmem_int8_fetch
    real*4    shmem_real4_fetch
    real*8    shmem_real8_fetch

    call shmem_init()

    me = shmem_my_pe()
    npes = shmem_n_pes()

    var_i4 = 0
    var_i8 = 0
    var_r4 = 0.0
    var_r8 = 0.0

    val_i4 = me + 1
    val_i8 = me + 1
    val_r4 = me + 1.0
    val_r8 = me + 1.0

    call shmem_barrier_all()

    call shmem_int4_set(var_i4, val_i4, MOD(me+1, npes))
    call shmem_int8_set(var_i8, val_i8, MOD(me+1, npes))
    call shmem_real4_set(var_r4, val_r4, MOD(me+1, npes))
    call shmem_real8_set(var_r8, val_r8, MOD(me+1, npes))

    call shmem_barrier_all()

    val_i4 = shmem_int4_fetch(var_i4, MOD(me+1, npes))
    val_i8 = shmem_int8_fetch(var_i8, MOD(me+1, npes))
    val_r4 = shmem_real4_fetch(var_r4, MOD(me+1, npes))
    val_r8 = shmem_real8_fetch(var_r8, MOD(me+1, npes))

    if (val_i4 .ne. me + 1) then
        write (*,*) "PE ", me, " int4 test failed: ", val_i4
        call shmem_global_exit(1)
    endif

    if (val_i8 .ne. me + 1) then
        write (*,*) "PE ", me, " int8 test failed: ", val_i8
        call shmem_global_exit(2)
    endif

    if (val_r4 - (me + 1) .gt. epsilon(val_r4)) then
        write (*,*) "PE ", me, " real4 test failed: ", val_r4
        call shmem_global_exit(3)
    endif

    if (val_r8 - (me + 1) .gt. epsilon(val_r8)) then
        write (*,*) "PE ", me, " real8 test failed: ", val_r8
        call shmem_global_exit(4)
    endif

    call shmem_finalize()
end program set_fetch
