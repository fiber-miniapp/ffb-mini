program ffb_mini
  use mpi
  use makemesh
  implicit none

  integer :: ierr
  include 'param.h'
  character(9), parameter :: PARAM_FILE = 'PARMLES3X'
  integer, parameter :: U = 10
  integer :: myrank, nprocs
  integer :: line
  integer :: narg, iarg
  integer :: npx, npy, npz, n, ipx, ipy, ipz, ip
  character(20) :: file_parm, file_mesh, file_boun

  character(10) :: test ="off"
  character(10) :: ntime = "100"
  character(10) :: isolp = "2"
  character(10) :: dt = "0.005"
  character(10) :: nmaxp = "50"
  character(10) :: nmaxt = "5"
!*character(10) :: epst = "1.0e-30"
!*character(10) :: epsp = "1.0e-30"
  character(10) :: epst = "0.0"
  character(10) :: epsp = "0.0"
!*character(60) :: reorder_ndiv = "#RO_SORT 1 10 10 10"
!*character(60) :: color_partsize = "#RO_COLR 1 2000 2000 2000 2000"
!*character(60) :: unroll = "#AXUNROL"
  character(60) :: reorder_ndiv = ""
  character(60) :: color_partsize = ""
  character(60) :: unroll = ""

  call MPI_Init(ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, myrank, ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)

  narg = command_argument_count()
  if (narg < 4) call print_usage_and_exit

  npx = get_int_arg(1)
  npy = get_int_arg(2)
  npz = get_int_arg(3)
  n   = get_int_arg(4)

  if (myrank == 0) then
    write(*, *)
    write(*, '(a)') 'generate mesh data ...'
    write(*, '("  domain arrangement: ", i0, " x ", i0, " x ", i0)') &
      npx, npy, npz
    write(*, '("  number of elements in each domain: ", i0, "^3 = ", i0)') &
      n, n**3
    write(*, '("  total number of elements: ", i0)') &
      n**3 * npx * npy * npz
    write(*, *)
  end if
  flush(6)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)

  if (npx*npy*npz /= nprocs) then
    if (myrank == 0) then
      write(*, '(a)') '***error: npx*npy*npz /= nprocs'
    end if
    call print_usage_and_exit
  end if

  ip = myrank + 1

  do iarg = 5, narg
    call parse_arg(iarg)
  end do

  write(file_parm, '("PARMLES3X.P", i4.4)') ip
  open(U, file=file_parm, form="formatted")
  do line = 1, NLINE
    call substitute_param(param_lines(line), '%ntime%', ntime)
    call substitute_param(param_lines(line), '%isolp%', isolp)
    call substitute_param(param_lines(line), '%dt%', dt)
    call substitute_param(param_lines(line), '%nmaxp%', nmaxp)
    call substitute_param(param_lines(line), '%nmaxt%', nmaxt)
    call substitute_param(param_lines(line), '%epst%', epst)
    call substitute_param(param_lines(line), '%epsp%', epsp)
    call substitute_param(param_lines(line), '%reorder_ndiv%', reorder_ndiv)
    call substitute_param(param_lines(line), '%color_partsize%', color_partsize)
    call substitute_param(param_lines(line), '%unroll%', unroll)
    write(U, '(a)') trim(param_lines(line))
  end do
  close(U)

  if (test == 'on') then
    call MPI_Finalize(ierr)
    stop
  end if

  call makemesh_init(npx, npy, npz, n)

  !  nprocs = (ip-1) = (ipx-1) + (ipy-1) * npx + (ipz-1) * npx*npy
  ipx = mod(myrank, npx) + 1
  ipy = mod(myrank/npx, npy) + 1
  ipz = myrank/(npx*npy) + 1

  write(file_mesh, '("MESH.P", i4.4)') ip
  write(file_boun, '("BOUN.P", i4.4)') ip

  call makemesh_mesh(ipx, ipy, ipz, file_mesh)
  call makemesh_boun(ipx, ipy, ipz, file_boun)

  flush(6)
  call MPI_Barrier(MPI_COMM_WORLD, ierr)
  if (myrank == 0) then
      write(*, '(/,a,/)') '... done'
  end if

  call LES3X(file_parm)

contains

subroutine print_usage_and_exit()
  if (myrank == 0) then
    write(*, '(a)') "usage: ffb_mini npx npy npz n [param=value ...]"
  end if
  call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
end subroutine print_usage_and_exit

subroutine print_message_and_exit(msg)
  character(*), intent(in) :: msg
  if (myrank == 0) then
    write(*, '(a)') msg
  end if
  call MPI_Abort(MPI_COMM_WORLD, 1, ierr)
end subroutine print_message_and_exit

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

subroutine parse_arg(i)
  integer, intent(in) :: i
  integer :: len, stat, ip
  character(256) :: arg
  character(20) :: key, val
  integer :: num

  call get_command_argument(i, arg, len, stat)
  if (stat /= 0) call print_usage_and_exit
  ip = index(arg, '=')
  if (ip == 0) call print_usage_and_exit
  key = arg(1:ip-1)
  val = trim(arg(ip+1:))
  select case(key)
    case ('test')
      test = val
    case ('ntime')
      ntime = val
    case ('isolp')
      isolp = val
    case ('dt')
      dt = val
    case ('nmaxp')
      nmaxp = val
    case ('nmaxt')
      nmaxt = val
    case ('epsp')
      epsp = val
    case ('epst')
      epst = val
    case ('color_partsize')
      read(val, *, iostat=stat) num
      if (stat /= 0) call print_message_and_exit("*** invalid argument: "//trim(arg))
      if (num > 0) then
        write(color_partsize, '("#RO_COLR 1", 4(" ", i0))') num, num, num, num
      else
        color_partsize = ''
      end if
    case ('reorder_ndiv')
      read(val, *, iostat=stat) num
      if (stat /= 0) call print_message_and_exit("*** invalid argument: "//trim(arg))
      if (num > 0) then
        write(reorder_ndiv, '("#RO_SORT 1", 3(" ", i0))') num, num, num
      else
        reorder_ndiv = ''
      end if
    case ('unroll')
      if (val == 'on') then
        unroll = "#AXUNROL"
      else if (val == 'off') then
        unroll = ''
      else
        call print_message_and_exit("*** invalid argument: "//trim(arg))
      end if
    case default
      call print_message_and_exit("*** invalid argument: "//trim(arg))
  end select

end subroutine parse_arg

subroutine substitute_param(line, key, val)
  character(*), intent(inout) :: line
  character(*), intent(in) :: key, val
  integer :: ip0, ip1
  ip0 = index(line, key)
  if (ip0 == 0) return
  ip1 = ip0 + len_trim(key) - 1
  line = line(1:ip0-1) // trim(val) // trim(line(ip1+1:))
end subroutine substitute_param

end program ffb_mini
