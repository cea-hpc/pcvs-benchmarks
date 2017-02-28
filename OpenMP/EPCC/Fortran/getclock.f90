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

function getclock() 

  implicit none

  integer, parameter :: dp = kind(1.0d0)

#ifdef OMPCLOCK
  real (kind = dp) :: getclock, OMP_GET_WTIME      
  getclock = OMP_GET_WTIME()
#endif

#ifdef F90CLOCK 
  integer count,rate
  real (kind = dp) :: getclock      
  call system_clock(count,rate) 
  getclock = dble(count)/dble(rate)
#endif  

end function getclock
