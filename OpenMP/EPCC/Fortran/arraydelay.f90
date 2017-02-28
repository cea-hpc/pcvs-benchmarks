!***************************************************************************
!*                                                                         *
!*             OpenMP MicroBenchmark Suite - Version 2.0                   *
!*                                                                         *
!*                            produced by                                  *
!*                                                                         *
!*                     Mark Bull and Fiona Reid                            *
!*                                                                         *
!*                                at                                       *
!*                                                                         *
!*                Edinburgh Parallel Computing Centre                      *
!*                                                                         *
!*         email: markb@epcc.ed.ac.uk or fiona@epcc.ed.ac.uk               *
!*                                                                         *
!*                                                                         *
!*      This version copyright (c) The University of Edinburgh, 2004.      *
!*                         All rights reserved.                            *
!*                                                                         *
!***************************************************************************

subroutine arraydelay(n,a)

  implicit none

  integer, parameter :: dp = kind(1.0d0)
  integer :: i, n
  real (kind=dp), dimension(*):: a   ! assumed size
  real (kind=dp) :: aaaa

  a(1) = 1.0
  aaaa=0.0
  do i=1,n
    aaaa=aaaa+a(1)
  end do
  if (aaaa < 0) print *,aaaa
  return

end subroutine arraydelay
