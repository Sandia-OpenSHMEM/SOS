!
!
! Copyright (c) 2011, 2012
!   University of Houston System and Oak Ridge National Laboratory.
! 
! All rights reserved.
! 
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions
! are met:
! 
! o Redistributions of source code must retain the above copyright notice,
!   this list of conditions and the following disclaimer.
! 
! o Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer in the
!   documentation and/or other materials provided with the distribution.
! 
! o Neither the name of the University of Houston System, Oak Ridge
!   National Laboratory nor the names of its contributors may be used to
!   endorse or promote products derived from this software without specific
!   prior written permission.
! 
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
! "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
! LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
! A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
! HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
! SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
! TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
! PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
! LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
! NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
! SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!
!

program test_shmem_barrier
  implicit none

  include 'shmem.fh'

  integer       :: pSync(1)
  integer*8     :: pSync_ptr
  pointer       (pSync_ptr, pSync)

  integer*4       :: flag(1)
  integer*8     :: flag_ptr
  pointer       (flag_ptr, flag)

  integer       :: me, npes, i

  integer       :: errcode, abort

  
  call start_pes(0);
  me   = my_pe();
  npes = num_pes();
  
  if(npes .gt. 4) then

    call shpalloc(pSync_ptr, SHMEM_BARRIER_SYNC_SIZE, errcode, abort)
    call shpalloc (flag_ptr, 1, errcode, abort)

    flag(1) = 0

    do i = 1, SHMEM_BARRIER_SYNC_SIZE
      pSync(i) = SHMEM_SYNC_VALUE 
    end do

    call shmem_barrier_all()

    if(me .ge. 2) then
      call shmem_barrier(2, 0, npes - 2, pSync) ! Only PEs 2, 3, 4, ... + perform the barrier
    end if

    if(me .ge. 2) then
      call shmem_barrier(2, 0, npes - 2, pSync) ! Only PEs 2, 3, 4, ... + perform the barrier
    end if

    if(me .ge. 1) then
      call shmem_int4_inc(flag, 0)
    end if

    if(me .eq. 0) then
      call shmem_int4_wait_until(flag(1), SHMEM_CMP_EQ, (npes - 1) )
      write (*,*) "test_shmem_barrier_01.f90: Passed" 
    end if

    call shpdeallc(pSync_ptr, errcode, abort)
    call shpdeallc(flag_ptr, errcode, abort)

  else
    write (*,*) 'Number of PEs must be 5+ to test barrier, test skipped'
  end if  
end program test_shmem_barrier
