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

program test_shmem_synchronization
  implicit none
  include 'shmem.fh'

  integer         :: me, npes
  integer*8, save :: dest
  integer*8       :: src
  integer         :: i

! Function definitions
  integer    :: my_pe, num_pes

  src = 9

  call start_pes(0)
  me   = my_pe()
  npes = num_pes()
  
  if(npes .gt. 1) then
  
    dest = 9
  
    call shmem_barrier_all()
  
    if (me .eq. 0) then
      do i = 1, 4, 1
        call shmem_integer_put(dest, src, 1, 1);
      end do
  
      do i = 1, 10, 1
        src = get_random_number()
        call shmem_integer_put(dest, src, 1, 1)
        if (src .ne. 9) then
          exit
        end if
      end do
    end if
  
    call shmem_barrier_all()
  
    if (me .eq. 1) then
      call shmem_int8_wait(dest, 9)
      write(*,*) "Test for conditional wait: Passed"
    end if
  
    call shmem_barrier_all()
    
    dest = 9
    call shmem_barrier_all()
  
    if (me .eq. 0) then
      do i = 1, 4, 1
        src = 9
        call shmem_integer_put(dest, src, 1, 1)
      end do
  
      do i = 1, 10, 1
        src = mod(get_random_number(), 10)
        call shmem_integer_put(dest, src, 1, 1)
        if (src .ne. 9) then
          exit
        end if
      end do 
    end if
  
    call shmem_barrier_all()
  
    if (me .eq. 1) then
      call shmem_int8_wait_until(dest, SHMEM_CMP_NE, 9)
      write (*,*) "Test for explicit conditional wait: Passed"
    end if
  
    call shmem_barrier_all()
    
  else
    write(*,*) "Test for conditional wait requires more than 1 PE, test skipped"
  end if
contains
  integer function get_random_number()
    implicit none
    real :: numbers(3)
    integer :: a_number

    call init_random_seed()
    call random_number(numbers)

    a_number = int(numbers(1) * 100)

    get_random_number = a_number

  end function get_random_number

  subroutine init_random_seed()
    implicit none
    integer :: i, n, clock
    integer, dimension(:), allocatable :: seed

    call random_seed(size = n)
    allocate(seed(n))

    call system_clock(count=clock)

    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    call random_seed(put = seed)

    deallocate(seed)
  end subroutine

end program test_shmem_synchronization
