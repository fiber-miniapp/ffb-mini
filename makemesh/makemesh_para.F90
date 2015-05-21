program makemesh_para
  use mpi
  use makemesh
  implicit none

  integer:: npx, npy, npz, n
  integer:: ipx, ipy, ipz, ip
  integer :: ierr, myrank, nprocs
  character(11) :: file_mesh, file_boun

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)

  npx = get_int_arg(1)
  npy = get_int_arg(2)
  npz = get_int_arg(3)
  n   = get_int_arg(4)

  if (npx*npy*npz /= nprocs) then
    if (myrank == 0) then
      write(*, '(a)') '***error: npx*npy*npz /= nprocs'
    end if
    call print_usage_and_exit
  end if

  call makemesh_init(npx, npy, npz, n)

  ip = myrank + 1

  !  (ip-1) = (ipx-1) + (ipy-1) * npx + (ipz-1) * npx*npy
  ipx = mod(myrank, npx) + 1
  ipy = mod(myrank/npx, npy) + 1
  ipz = myrank/(npx*npy) + 1

!*write(*, *) myrank,  ': (', ipx, ',', ipy, ',', ipz, ')'

  write(file_mesh, '("MESH.P", i4.4)') ip
  write(file_boun, '("BOUN.P", i4.4)') ip

  call makemesh_mesh(ipx, ipy, ipz, file_mesh)
  call makemesh_boun(ipx, ipy, ipz, file_boun)

  call MPI_Finalize(ierr)

contains

subroutine print_usage_and_exit()
  if (myrank == 0) then
    write(*, '(a)') "usage: makemesh_para npx npy npz n"
  end if 
  call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
end subroutine print_usage_and_exit

function get_int_arg(i) result(val)
  integer, intent(in) :: i
  integer :: val
  integer :: len, stat
  character(256) :: arg

  call get_command_argument(i, arg, len, stat)
  if (stat /= 0) call print_usage_and_exit
  read(arg, '(i255)', iostat=stat) val
  if (stat /= 0) call print_usage_and_exit
end function get_int_arg

end program makemesh_para
