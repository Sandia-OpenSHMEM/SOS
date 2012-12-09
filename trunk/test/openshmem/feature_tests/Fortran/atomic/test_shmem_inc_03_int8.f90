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

program test_shmem_atomics
  implicit none
  include 'shmem.fh'

  logical, parameter        :: true_val = .TRUE.
  logical, save             :: success1
  logical, save             :: success2

  integer*8                 :: target(1)
  integer*8                :: target_ptr
  pointer (target_ptr, target)

  integer*8                 :: swapped_val, new_val

  integer                   :: errcode, abort
  integer                   :: me, npes


  call start_pes(0)
  me = my_pe()
  npes = num_pes()

  call shmem_barrier_all()

  ! Make sure this job is running with at least 2 PEs.

  if (npes .gt. 1) then
    success1 = .FALSE.

    call shpalloc(target_ptr, 1, errcode, abort)

    target(1) = 51234

    call shmem_barrier_all()

    if(me .eq. 0) then
      call shmem_int8_inc(target, npes - 1) 
    end if

    call shmem_barrier_all()

    ! To validate the working of swap we need to check the value received at the PE that initiated the swap 
    !  as well as the target PE

    if(me .eq. npes - 1) then
      if(target(1) .eq. 51234 + 1) then
        call shmem_logical_put(success1, true_val, 1, 0)
      end if
    end if

    call shmem_barrier_all()

    if(me .eq. 0) then
      if(success1 .eqv. .TRUE.) then
        write (*,*) "Test 01 shmem_int8_inc: Passed"
      else
        write (*,*) "Test 01 shmem_int8_inc: Failed"
      end if
    end if

    call shmem_barrier_all()

    call shpdeallc(target_ptr, errcode, abort)

  else
    write (*,*) "Number of PEs must be > 1 to test shmem atomics, test skipped"
  end if 

end program test_shmem_atomics
