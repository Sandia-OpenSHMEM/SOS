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

program test_shmem_reduction
  implicit none
  include 'shmem.fh'

  integer,   parameter :: min_npes = 3
  integer,   parameter :: nelems = 10

  integer*8, save      :: src(nelems)
  integer*8, save      :: target(nelems)
  integer*8, save    :: target_expected(nelems)

  integer,   save      :: pSync(SHMEM_REDUCE_SYNC_SIZE)
  integer  , save      :: pWrk(SHMEM_REDUCE_MIN_WRKDATA_SIZE)

  integer              :: me, npes, i, pe
  logical              :: success
  character*(*), parameter :: TEST_NAME = 'shmem_sum'

  ! Function definitions
  integer                   :: my_pe, num_pes

  success = .TRUE.

  call start_pes(0)

  me   = my_pe()
  npes = num_pes()

  if (npes .ge. min_npes) then

    pSync(:) = SHMEM_SYNC_VALUE

    do i = 1, nelems, 1
      target(i) = 0
      src(i) = INT(me + i, KIND=8)
    end do

    target_expected(:) = 0
    do pe = 0, npes - 1, 1
      do i = 1, nelems, 1
        target_expected(i) =  target_expected(i) + INT((pe + i), KIND=8)
      end do
    end do

    call shmem_barrier_all()
    
    call shmem_int8_sum_to_all(target, src, nelems, 0, 0, npes, pWrk, pSync)

    do i = 1, nelems, 1
      if(target(i) .ne. target_expected(i)) then
        success = .FALSE.
      end if 
    end do

    if(me .eq. 0) then
      if(success .eqv. .TRUE.) then
        write(*,*) TEST_NAME, ': Passed'
      else
        write(*,*) TEST_NAME, ': Failed'
      end if
    end if
    
  else
    if(me .eq. 0) then
      write(*,*) 'This test requires ', min_npes, ' or more PEs.'
    end if
  end if
end program test_shmem_reduction
