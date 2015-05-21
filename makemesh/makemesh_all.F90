program makemesh_all
  use makemesh
  implicit none

  integer:: npx, npy, npz, n
  integer:: ipx, ipy, ipz, ip
  character(11) :: file_mesh, file_boun

  npx = get_int_arg(1)
  npy = get_int_arg(2)
  npz = get_int_arg(3)
  n   = get_int_arg(4)

  write(*,*) 'npx = ', npx
  write(*,*) 'npy = ', npy
  write(*,*) 'npz = ', npz
  write(*,*) 'n = ', n

  call makemesh_init(npx, npy, npz, n)

  ip = 0
  do ipz = 1, npz
    do ipy = 1, npy
      do ipx = 1, npx
        ip = ip + 1
!*      write(file_mesh, '("MESH.P", i5.5)') ip
!*      write(file_boun, '("BOUN.P", i5.5)') ip
        write(file_mesh, '("MESH.P", i4.4)') ip
        write(file_boun, '("BOUN.P", i4.4)') ip
        write(*,*) 'ipx, ipy, ipz = ', ipx, ipy, ipz
        write(*,*) file_mesh
        write(*,*) file_boun
        call makemesh_mesh(ipx, ipy, ipz, file_mesh)
        call makemesh_boun(ipx, ipy, ipz, file_boun)
      end do
    end do
  end do

contains

subroutine print_usage_and_exit()
  write(*, '(a)') "usage: makemesh npx npy npz n"
  stop 
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

end program makemesh_all
