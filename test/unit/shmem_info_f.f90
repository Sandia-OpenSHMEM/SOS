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

program shmem_info
    include 'shmem.fh'

    integer me, major_ver, minor_ver
    character (len=SHMEM_MAX_NAME_LEN) lib_name

    call shmem_init()
    me = shmem_my_pe()

    call shmem_info_get_version(major_ver, minor_ver)
    call shmem_info_get_name(lib_name)

    ! Note: The spec does not require these strings to be identical.
    !       They are implemented this way in SOS, and we take advantage
    !       of that property to check the C/Fortran bindings linkage.
    if (lib_name .ne. SHMEM_VENDOR_STRING) then
        print *, me, "Vendor strings did not match!"
        print *, me, "shmem_info_get_name: ", lib_name
        print *, me, "SHMEM_VENDOR_STRING: ", SHMEM_VENDOR_STRING
        call shmem_global_exit(1)
    endif

    if ((major_ver .ne. SHMEM_MAJOR_VERSION) .OR.                       &
        (minor_ver .ne. SHMEM_MINOR_VERSION)) then
        print *, me, "Version numbers did not match!"
        print *, me, "shmem_info_get_version: ", major_ver, minor_ver
        print *, me, "Library constants     : ", SHMEM_MAJOR_VERSION,   &
            SHMEM_MINOR_VERSION
        call shmem_global_exit(1)
    end if

    call shmem_finalize()
end program shmem_info
