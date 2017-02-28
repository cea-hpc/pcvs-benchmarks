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

module arraydata

  use setprecision
  implicit none

  integer, parameter :: sz = IDA
  real (kind = dp), dimension(IDA) :: thrtest
  real (kind = dp), dimension(IDA) :: redtest

!$OMP THREADPRIVATE(thrtest)

end module arraydata
