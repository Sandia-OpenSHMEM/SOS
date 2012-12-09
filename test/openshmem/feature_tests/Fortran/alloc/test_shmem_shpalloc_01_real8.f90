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

program test_shmem_shpalloc
  implicit none
  include 'shmem.fh'

  integer, parameter :: min_npes = 3
  integer, parameter :: nelems = 50

  real*8           :: array(1)    
  integer*8          :: array_addr
  pointer            (array_addr, array)

  real*8           :: buffer(nelems)
  
  integer            :: errcode, abort, me, npes, pe, i
  logical            :: success

  character*(*), parameter  :: TEST_NAME='shpalloc'

  ! Function return value types
  integer            :: my_pe, num_pes

  call start_pes(0)

  me = my_pe()
  npes = num_pes()

  success = .TRUE.

  if(npes .ge. min_npes) then

    ! allocate remotely accessible block
    call shpalloc(array_addr, nelems, errcode, abort)

    do i = 1, nelems 
      array(i) = REAL(54321.67, KIND=8) 
    end do

    call shmem_barrier_all();

    if(me .eq. 0) then
      do pe = 1, npes - 1, 1
        ! Reset the contents of our local buffer
        do i = 1, nelems, 1
          buffer(i) = -9 
        end do

        ! Get data on PE 'pe'
        call shmem_real_get(buffer, array, nelems, pe) 

        ! Check that values are correct
        do i = 1, nelems, 1
          if(buffer(i) .ne. REAL(54321.67, KIND=8)) then
            success = .FALSE.
          end if
        end do
      end do

      if(success .eqv. .TRUE.) then
        write (*,*) TEST_NAME, ': Passed'
      else
        write (*,*) TEST_NAME, ': Failed'
      end if
    end if

    ! All PEs wait until PE 0 has finished.
    call shmem_barrier_all()

    call shpdeallc(array_addr, errcode, abort)

  else
   if(me .eq. 0) then
     write (*,*) 'This test requires ', min_npes, ' or more PEs.'
   end if
  end if
  
end program
