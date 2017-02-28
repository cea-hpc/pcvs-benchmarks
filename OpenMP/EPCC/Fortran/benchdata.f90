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

module benchdata

  use omp_lib
  use setprecision
  implicit none

  integer, parameter :: outerreps = 20
  integer, parameter :: itersperthr = 128, mhz = 900
  integer, parameter :: innerreps_sched = 1000
  integer :: innerreps, delaylength, nthreads, cksz
  real (kind = sp), parameter :: conf95 = 1.96
  real (kind = dp), dimension(0:outerreps) :: time
  real (kind = dp) :: reftime, refsd

end module benchdata
