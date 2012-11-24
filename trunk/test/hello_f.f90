program hello
  include 'shmem.fh'

  integer npes, me

  call start_pes(0)
  npes = num_pes()
  me = my_pe()

  print *,'I am ', me, ' of ', npes

end program hello
