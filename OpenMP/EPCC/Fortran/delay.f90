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

subroutine delay(n)

  implicit none

  integer, intent(in)  :: n
  integer :: i  
  integer, parameter :: dp = kind(1.0d0)
  real (kind = dp) :: aaaa

  aaaa = 0.0
  do i = 1, n
    aaaa = aaaa + i
  end do
  if (aaaa < 0) print *,aaaa

end subroutine delay

