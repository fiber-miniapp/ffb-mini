      subroutine opening(u, version, filein)
c
c  display opening messsage
c
      implicit none

c--- in
      integer:: u
      character(*):: version
      character(*):: filein

c--- local
      character(8) :: date
      character(10) :: time
      include 'mpif.h'
      character(MPI_MAX_PROCESSOR_NAME) :: name
      integer :: nprocs, name_len, ierr

c---
      call date_and_time(date, time)
      call MPI_Comm_size(MPI_COMM_WORLD, nprocs, ierr)
      call MPI_Get_processor_name(name, name_len, ierr)

      write(u, '(a)') repeat('*', 50)
      write(u, '(2x, a)') 'FFB_MINI ' // FFB_MINI_VERSION
      write(u, '(2x, a)') '(based on FFB8.1, ' // trim(version) // ')'
      write(u, '(4x, "date: ", a, "/", a, "/", a)')
     &     date(1:4), date(5:6), date(7:8)
      write(u, '(4x, "time: ", a, ":", a, ":", a)')
     &      time(1:2), time(3:4), time(5:6)
      write(u, '(4x, "host: ", a)') trim(name)
      write(u, '(4x, "nodes: ", i0)') nprocs
      write(u, '(4x, a)') 'parameter file: ' // trim(filein)
      write(u, '(a)') repeat('*', 50)

      end subroutine opening

c=========================================================

      subroutine calave(NE, NP, PE, U, V, W, ave_p, ave_v, max_v)
c
c  calculate average pressure, average velocity and
c  maximum velocity over local domain 
c
      implicit none

c--- in
      integer :: NE, NP
      real(4) :: PE(NE)
      real(4) :: U(NP), V(NP), W(NP)

c--- out
      real(4) :: ave_p, ave_v, max_v

c--- local
      integer :: ie, ip
      real(4) :: abs_v

c---
      ave_p = 0.0e0
      do ie = 1, NE
        ave_p = ave_p + PE(ie)
      end do
      ave_p = ave_p / NE

      ave_v = 0.0e0
      max_v = 0.0e0
      do ip = 1, NP
        abs_v = sqrt(U(ip)**2+V(ip)**2+W(ip)**2)
        ave_v = ave_v + abs_v
        max_v = max(max_v, abs_v)
      end do
      ave_v = ave_v / NP

      end subroutine calave
 
c=========================================================

      integer*4 function total(ival)
c
c   return total amount of an integer variable over whole domains
c
      implicit none

c--- in
      integer :: ival

c--- local
      integer :: ierr
      include 'mpif.h'

c--
      call MPI_Allreduce(ival, total, 1, MPI_INTEGER,
     &                   MPI_SUM, MPI_COMM_WORLD, ierr)

      end function total
