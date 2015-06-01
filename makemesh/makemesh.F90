module makemesh
  implicit none

  private
  public makemesh_init, makemesh_mesh, makemesh_boun

  integer, parameter :: NAME_LEN = 30, COMMENT_LEN = 60
  integer :: npx, npy, npz, n

contains

function node_num(ix, iy, iz) result(i)
  integer, intent(in) :: ix, iy, iz
  integer :: i
  i = ix + iy*(n+1) + iz*(n+1)*(n+1) + 1
end function node_num

function proc_num(ipx, ipy, ipz) result(ip)
  integer, intent(in) :: ipx, ipy, ipz
  integer :: ip
  ip = ipx + (ipy-1)*npx + (ipz-1)*npx*npy
end function proc_num

subroutine makemesh_init(npx_in, npy_in, npz_in, n_in)
  integer, intent(in) :: npx_in, npy_in, npz_in, n_in
  npx = npx_in
  npy = npy_in
  npz = npz_in
  n = n_in
end subroutine makemesh_init

subroutine makemesh_mesh(ipx, ipy, ipz, file_mesh)
  integer, intent(in) :: ipx, ipy, ipz
  character(*), intent(in) :: file_mesh
  integer :: nv, ne
  real, allocatable :: x(:), y(:), z(:)
  integer, allocatable :: node(:,:)
  real :: d, offset_x, offset_y, offset_z
  integer :: ix, iy, iz, iv, ie, i
  integer, parameter :: fout = 10
  character(NAME_LEN) :: name
  character(COMMENT_LEN) :: comment

  ne = n**3
  nv = (n+1)**3

  d = 1.0 / float(n)
  offset_x = 1.0 * (ipx - 1)
  offset_y = 1.0 * (ipy - 1)
  offset_z = 1.0 * (ipz - 1)

  allocate(x(nv), y(nv), z(nv), node(8,ne))

  iv = 0
  do iz = 0, n
    do iy = 0, n
      do ix = 0, n
        iv = iv + 1
        x(iv) = offset_x + ix * d
        y(iv) = offset_y + iy * d
        z(iv) = offset_z + iz * d
      end do
    end do
  end do
  if (iv /= nv) stop 'iv /= nv'

  ie = 0
  do iz = 1, n
    do iy = 1, n
      do ix = 1, n
        ie = ie + 1
        node(1,ie) = node_num(ix-1, iy-1, iz-1)
        node(2,ie) = node(1,ie) + 1
        node(3,ie) = node(2,ie) + n + 1
        node(4,ie) = node(1,ie) + n + 1
        node(5,ie) = node(1,ie) + (n + 1)**2
        node(6,ie) = node(2,ie) + (n + 1)**2
        node(7,ie) = node(3,ie) + (n + 1)**2
        node(8,ie) = node(4,ie) + (n + 1)**2
      end do
    end do
  end do
  if (ie /= ne) stop 'ie /= ne'

  open(fout, file=file_mesh, form="unformatted")

  write(fout) '#U_GF_V1'
  write(fout) 1
! write(fout) '** CUBIC CAVITY FLOW **'
  comment = '** CUBIC CAVITY FLOW **'
  write(fout) comment
  write(fout) '#NEW_SET'
  write(fout) 1
! write(fout) ' TEST DATA '
  comment = ' TEST DATA '
  write(fout) comment
  write(fout) '#FLT_ARY'
  write(fout) '*GRID_3D'
! write(fout) ' GRID COORDINATES (3-D)'
  name = ' GRID COORDINATES (3-D)'
  write(fout) name
  write(fout) 3, nv
  write(fout) (x(iv), y(iv), z(iv), iv=1, nv)
  write(fout) '#INT_ARY'
  write(fout) '*NODE_3D'
! write(fout) ' NODE TABLE (3-D)'
  name = ' NODE TABLE (3-D)'
  write(fout) name
  write(fout) 8, ne
  write(fout) ((node(i,ie), i=1, 8), ie=1, ne)
  write(fout) '#ENDFILE'

  close(fout)
  deallocate(x, y, z, node)

end subroutine makemesh_mesh

subroutine makemesh_boun(ipx, ipy, ipz, file_boun)
  integer, intent(in) :: ipx, ipy, ipz
  character(*), intent(in) :: file_boun
#ifdef MOVE_X_TOP
  real, parameter :: vel(3) = (/ 0.0, 0.0, 1.0 /)
#elif defined(MOVE_Z_TOP)
  real, parameter :: vel(3) = (/ 0.0, 1.0, 0.0 /)
#else /* MOVE_Y_TOP */
  real, parameter :: vel(3) = (/ 1.0, 0.0, 0.0 /)
#endif
  integer :: nv_x, nv_y, nv_z
  integer :: nv_wall, nv_mwall, nv_int
  integer, allocatable :: iv_wall(:), iv_mwall(:)
  integer, allocatable :: iv_int1(:), iv_int2(:), iv_int3(:)
  integer :: ix, iy, iz, iv, iv_w, iv_mw, iv_i, ip
  integer, parameter :: fout = 10
  character(NAME_LEN) :: name
  character(COMMENT_LEN) :: comment

  nv_x = n + 1
  nv_y = n + 1
  nv_z = n + 1
  if (ipx == 1)   nv_x = nv_x - 1
  if (ipx == npx) nv_x = nv_x - 1
  if (ipy == 1)   nv_y = nv_y - 1
  if (ipy == npy) nv_y = nv_y - 1
  if (ipz == 1)   nv_z = nv_z - 1
  if (ipz == npz) nv_z = nv_z - 1
  nv_wall = (n + 1)**3 - nv_x*nv_y*nv_z

  nv_mwall = 0
#ifdef MOVE_X_TOP
  if (ipx == npx) nv_mwall = (n + 1)**2
#elif defined(MOVE_Z_TOP)
  if (ipz == npz) nv_mwall = (n + 1)**2
#else /* MOVE_Y_TOP */
  if (ipy == npy) nv_mwall = (n + 1)**2
#endif
  nv_wall = nv_wall - nv_mwall

  nv_int = 0
  ! faces
  if (ipx /= 1)   nv_int = nv_int + (n + 1)**2
  if (ipx /= npx) nv_int = nv_int + (n + 1)**2
  if (ipy /= 1)   nv_int = nv_int + (n + 1)**2
  if (ipy /= npy) nv_int = nv_int + (n + 1)**2
  if (ipz /= 1)   nv_int = nv_int + (n + 1)**2
  if (ipz /= npz) nv_int = nv_int + (n + 1)**2

  ! edges
  if (ipz /= 1) then
    if (ipx /= 1)   nv_int = nv_int + (n + 1)
    if (ipx /= npx) nv_int = nv_int + (n + 1)
    if (ipy /= 1)   nv_int = nv_int + (n + 1)
    if (ipy /= npy) nv_int = nv_int + (n + 1)
  end if
  if (ipy /= 1) then
    if (ipx /= 1)   nv_int = nv_int + (n + 1)
    if (ipx /= npx) nv_int = nv_int + (n + 1)
  end if
  if (ipy /= npy) then
    if (ipx /= 1)   nv_int = nv_int + (n + 1)
    if (ipx /= npx) nv_int = nv_int + (n + 1)
  end if
  if (ipz /= npz) then
    if (ipx /= 1)   nv_int = nv_int + (n + 1)
    if (ipx /= npx) nv_int = nv_int + (n + 1)
    if (ipy /= 1)   nv_int = nv_int + (n + 1)
    if (ipy /= npy) nv_int = nv_int + (n + 1)
  end if

  ! corners
  if (ipz /= 1) then
    if (ipy /= 1) then
      if (ipx /= 1)   nv_int = nv_int + 1
      if (ipx /= npx) nv_int = nv_int + 1
    end if
    if (ipy /= npy) then
      if (ipx /= 1)   nv_int = nv_int + 1
      if (ipx /= npx) nv_int = nv_int + 1
    end if
  end if
  if (ipz /= npz) then
    if (ipy /= 1) then
      if (ipx /= 1)   nv_int = nv_int + 1
      if (ipx /= npx) nv_int = nv_int + 1
    end if
    if (ipy /= npy) then
      if (ipx /= 1)   nv_int = nv_int + 1
      if (ipx /= npx) nv_int = nv_int + 1
    end if
  end if

  write(*, '("(", i3, ",", i3, ",", i3, "): ",  &
      & "nv_wall=", i0, ", nv_mwall=", i0, ", nv_int=", i0)')  &
    ipx, ipy, ipz, nv_wall, nv_mwall, nv_int
!*write(*, *) '  nv_wall = ', nv_wall
!*write(*, *) '  nv_mwall = ', nv_mwall
!*write(*, *) '  nv_int = ', nv_int
  allocate(iv_wall(nv_wall), iv_mwall(nv_mwall))
  allocate(iv_int1(nv_int), iv_int2(nv_int), iv_int3(nv_int))

  iv = 0
  iv_w = 0
  iv_mw = 0
  do iz = 0, n
    do iy = 0, n
      do ix = 0, n
        iv = iv + 1
#ifdef MOVE_X_TOP
        if (ipx == npx .and. ix == n) then
#elif defined(MOVE_Z_TOP)
        if (ipz == npz .and. iz == n) then
#else /* MOVE_Y_TOP */
        if (ipy == npy .and. iy == n) then
#endif
          iv_mw = iv_mw + 1
          iv_mwall(iv_mw) = iv
          cycle
        end if
        if ((ipx == 1   .and. ix == 0) .or.  &
            (ipx == npx .and. ix == n) .or.  &
            (ipy == 1   .and. iy == 0) .or.  &
            (ipy == npy .and. iy == n) .or.  &
            (ipz == 1   .and. iz == 0) .or.  &
            (ipz == npz .and. iz == n)) then
          iv_w = iv_w + 1
          iv_wall(iv_w) = iv
        end if
      end do
    end do
  end do
  if (iv_w /= nv_wall) stop 'iv_w /= nv_wall'
  if (iv_mw /= nv_mwall) stop 'iv_mw /= nv_mwall'

  iv_i = 0

  !faces
  if (ipx /= 1) then
    ip = proc_num(ipx-1, ipy, ipz)
    do iz = 0, n
      do iy = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(0, iy, iz)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(n, iy, iz)
      end do
    end do
  end if
  if (ipx /= npx) then
    ip = proc_num(ipx+1, ipy, ipz)
    do iz = 0, n
      do iy = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(n, iy, iz)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(0, iy, iz)
      end do
    end do
  end if
  if (ipy /= 1) then
    ip = proc_num(ipx, ipy-1, ipz)
    do iz = 0, n
      do ix = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(ix, 0, iz)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(ix, n, iz)
      end do
    end do
  end if
  if (ipy /= npy) then
    ip = proc_num(ipx, ipy+1, ipz)
    do iz = 0, n
      do ix = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(ix, n, iz)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(ix, 0, iz)
      end do
    end do
  end if
  if (ipz /= 1) then
    ip = proc_num(ipx, ipy, ipz-1)
    do iy = 0, n
      do ix = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(ix, iy, 0)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(ix, iy, n)
      end do
    end do
  end if
  if (ipz /= npz) then
    ip = proc_num(ipx, ipy, ipz+1)
    do iy = 0, n
      do ix = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(ix, iy, n)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(ix, iy, 0)
      end do
    end do
  end if

  ! edges
  if (ipz /= 1) then
    if (ipx /= 1)  then
      ip = proc_num(ipx-1, ipy, ipz-1)
      do iy = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(0, iy, 0)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(n, iy, n)
      end do
    end if
    if (ipx /= npx) then
      ip = proc_num(ipx+1, ipy, ipz-1)
      do iy = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(n, iy, 0)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(0, iy, n)
      end do
    end if
    if (ipy /= 1) then
      ip = proc_num(ipx, ipy-1, ipz-1)
      do ix = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(ix, 0, 0)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(ix, n, n)
      end do
    end if
    if (ipy /= npy) then
      ip = proc_num(ipx, ipy+1, ipz-1)
      do ix = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(ix, n, 0)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(ix, 0, n)
      end do
    end if
  end if
  if (ipy /= 1) then
    if (ipx /= 1) then
      ip = proc_num(ipx-1, ipy-1, ipz)
      do iz = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(0, 0, iz)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(n, n, iz)
      end do
    end if
    if (ipx /= npx) then
      ip = proc_num(ipx+1, ipy-1, ipz)
      do iz = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(n, 0, iz)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(0, n, iz)
      end do
    end if
  end if
  if (ipy /= npy) then
    if (ipx /= 1) then
      ip = proc_num(ipx-1, ipy+1, ipz)
      do iz = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(0, n, iz)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(n, 0, iz)
      end do
    end if
    if (ipx /= npx) then
      ip = proc_num(ipx+1, ipy+1, ipz)
      do iz = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(n, n, iz)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(0, 0, iz)
      end do
    end if
  end if
  if (ipz /= npz) then
    if (ipx /= 1) then
      ip = proc_num(ipx-1, ipy, ipz+1)
      do iy = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(0, iy, n)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(n, iy, 0)
      end do
    end if
    if (ipx /= npx) then
      ip = proc_num(ipx+1, ipy, ipz+1)
      do iy = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(n, iy, n)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(0, iy, 0)
      end do
    end if
    if (ipy /= 1) then
      ip = proc_num(ipx, ipy-1, ipz+1)
      do ix = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(ix, 0, n)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(ix, n, 0)
      end do
    end if
    if (ipy /= npy) then
      ip = proc_num(ipx, ipy+1, ipz+1)
      do ix = 0, n
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(ix, n, n)
        iv_int2(iv_i) = ip
        iv_int3(iv_i) = node_num(ix, 0, 0)
      end do
    end if
  end if

  ! corners
  if (ipz /= 1) then
    if (ipy /= 1) then
      if (ipx /= 1) then
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(0, 0, 0)
        iv_int2(iv_i) = proc_num(ipx-1, ipy-1, ipz-1)
        iv_int3(iv_i) = node_num(n, n, n)
      end if
      if (ipx /= npx) then
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(n, 0, 0)
        iv_int2(iv_i) = proc_num(ipx+1, ipy-1, ipz-1)
        iv_int3(iv_i) = node_num(0, n, n)
      end if
    end if
    if (ipy /= npy) then
      if (ipx /= 1) then
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(0, n, 0)
        iv_int2(iv_i) = proc_num(ipx-1, ipy+1, ipz-1)
        iv_int3(iv_i) = node_num(n, 0, n)
      end if
      if (ipx /= npx) then
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(n, n, 0)
        iv_int2(iv_i) = proc_num(ipx+1, ipy+1, ipz-1)
        iv_int3(iv_i) = node_num(0, 0, n)
      end if
    end if
  end if
  if (ipz /= npz) then
    if (ipy /= 1) then
      if (ipx /= 1) then
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(0, 0, n)
        iv_int2(iv_i) = proc_num(ipx-1, ipy-1, ipz+1)
        iv_int3(iv_i) = node_num(n, n, 0)
      end if
      if (ipx /= npx) then
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(n, 0, n)
        iv_int2(iv_i) = proc_num(ipx+1, ipy-1, ipz+1)
        iv_int3(iv_i) = node_num(0, n, 0)
      end if
    end if
    if (ipy /= npy) then
      if (ipx /= 1) then
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(0, n, n)
        iv_int2(iv_i) = proc_num(ipx-1, ipy+1, ipz+1)
        iv_int3(iv_i) = node_num(n, 0, 0)
      end if
      if (ipx /= npx) then
        iv_i = iv_i + 1
        iv_int1(iv_i) = node_num(n, n, n)
        iv_int2(iv_i) = proc_num(ipx+1, ipy+1, ipz+1)
        iv_int3(iv_i) = node_num(0, 0, 0)
      end if
    end if
  end if
  if (iv_i /= nv_int) stop 'iv_i /= nv_int'

  open(fout, file=file_boun, form="unformatted")
  write(fout) '#U_GF_V1'
  write(fout) 1
! write(fout) '** CUBIC CAVITY FLOW **'
  comment = '** CUBIC CAVITY FLOW **'
  write(fout) comment
  write(fout) '#NEW_SET'
  write(fout) 1
! write(fout) ' TEST DATA '
  comment  = ' TEST DATA '
  write(fout) comment

  if (nv_wall > 0) then
  write(fout) '#INT_ARY'
  write(fout) '*BC_WALL'
! write(fout) ' WALL  BOUNDARY NODES'
  name = ' WALL  BOUNDARY NODES'
  write(fout) name
  write(fout) 1, nv_wall
  write(fout) (iv_wall(iv), iv=1, nv_wall)
  endif

  if (nv_mwall > 0) then
  write(fout) '#INT_ARY'
  write(fout) '*BC_MWAL'
! write(fout) ' MOVING WALL BOUNDARY NODES'
  name = ' MOVING WALL BOUNDARY NODES'
  write(fout) name
  write(fout) 1, nv_mwall
  write(fout) (iv_mwall(iv), iv=1, nv_mwall)

  write(fout) '#FLT_ARY'
  write(fout) '*BC_WV3D'
! write(fout) ' MOVING WALL VELOCITIES    3-D'
  name = ' MOVING WALL VELOCITIES    3-D'
  write(fout) name
  write(fout) 3, nv_mwall
  write(fout) (vel(1), vel(2), vel(3), iv=1, nv_mwall)
  endif

  if (nv_int > 0) then
  write(fout) '#INT_ARY'
  write(fout) '*BC_INTR'
! write(fout) ' INTER-CONNECT BOUNDARY NODES'
  name = ' INTER-CONNECT BOUNDARY NODES'
  write(fout) name
  write(fout) 3, nv_int
  write(fout) (iv_int1(iv), iv_int2(iv), iv_int3(iv), iv=1, nv_int)
  end if

  write(fout) '#ENDFILE'
  close(fout)
  deallocate(iv_wall, iv_mwall, iv_int1, iv_int2, iv_int3)

end subroutine makemesh_boun

end module makemesh
