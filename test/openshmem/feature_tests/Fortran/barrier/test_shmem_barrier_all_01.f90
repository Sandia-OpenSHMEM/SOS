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
  integer, parameter :: min_npes = 3

  integer   :: flag
  integer*8 :: flag_ptr
  pointer      (flag_ptr, flag)

  integer       :: me, npes, i
  integer     :: errcode, abort


  call start_pes(0);

  me   = my_pe();
  npes = num_pes();

  if (npes .gt. 1) then

    call shpalloc(flag_ptr, 1, errcode, abort)    

    if(me .ne. 0) then
      call shmem_int4_inc(flag, 0)
    end if

    call shmem_barrier_all()

! Tests that processes wait until data in flight has been received before continuing.
! This is probably too difficult to test
    if(me .eq. 0) then
      if(flag .eq. npes - 1) then
        write (*,*) 'Test shmem_barrier_all: Passed'
      else
        write (*,*) 'Test shmem_barrier_all: Failed'
      end if
    end if

    call shpdeallc(flag_ptr, errcode, abort)

  else
    if(me .eq. 0) then
      write (*,*) 'This test requirest ', min_npes, ' or more PEs.'
    end if
  end if  
end program test_shmem_barrier
