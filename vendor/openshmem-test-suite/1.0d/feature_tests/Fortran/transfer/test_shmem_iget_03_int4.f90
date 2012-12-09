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

program test_shmem_iget
  implicit none
  include 'shmem.fh'

  character*80       :: argv ! testing reading cmdline argument
  integer            :: argc

  integer, parameter :: N = 10 

  integer            ::  i,j
  integer            ::  nextpe
  integer            ::  me, npes
  logical            ::  success

  integer*4           :: dest(N)

  integer*4           :: src(1)
  integer*8          :: src_ptr
  pointer            (src_ptr, src)

  integer            :: errcode, abort

! Function definitions
  integer            :: my_pe, num_pes

  call start_pes(0)
  
  me   = my_pe();
  npes = num_pes();

  if(npes .gt. 1) then

    success = .TRUE.

    call shpalloc(src_ptr, N, errcode, abort)

    dest(:) = -9

    do i = 1, N, 1
      src(i) = INT(54321 + i, KIND=4)
    end do

    call shmem_barrier_all()

    if(me .eq. 0) then
      call shmem_integer_iget(dest, src, 1, 2, N/2, npes - 1)
    end if

    call shmem_barrier_all()

    if(me .eq. 0) then
      do i = 1, N/2, 1
        if(dest(i) .ne. src((i-1)*2 + 1)) then
          success = .FALSE.
        end if
      end do 

      if(success .eqv. .TRUE.) then
        write(*,*) "Test shmem_integer_iget: Passed" 
      else
        write(*,*) "Test shmem_integer_iget: Failed"
      end if
    end if 

    call shmem_barrier_all()
    
    call shpdeallc(src_ptr, errcode, abort)

  else
    write(*,*) "Number of PEs must be > 1 to test shmem get, test skipped"
  end if
end program
