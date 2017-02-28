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

module benchstats
  implicit none
  private
  public :: stats
contains

  subroutine stats (meantime,sd)

    use benchdata
    implicit none

    real (kind = dp) :: meantime, totaltime, sumsq, mintime, maxtime, sd
    real (kind = dp) :: cutoff
    integer :: nr, i

    mintime = 1.0e20
    maxtime = 0.
    totaltime = 0.

    do i = 1, outerreps
      mintime = min(mintime,time(i))
      maxtime = max(maxtime,time(i))
      totaltime = totaltime + time(i)
    end do 

    meantime  = totaltime/ real (outerreps)

    sumsq = 0.
    do i = 1, outerreps
      sumsq = sumsq + (time(i)-meantime)**2 
    end do 

    sd = sqrt(sumsq/(outerreps-1))

! COUNT OUTLIERS 

    cutoff = 3.0 * sd 
    nr = 0 

    do i = 1, outerreps
      if (abs(time(i)-meantime) > cutoff) nr = nr + 1
    end do

    write (6,*)
    write(6,'(a11,8x,a7,6x,a3,10x,a3,11x,a5,6x,a8)')&
    'Sample_size','Average','Min','Max','S.D.','Outliers'
    write (6,1002) outerreps, meantime, mintime,maxtime, sd, nr 
    write (6,*)
    1002 format(4x,i7,4x,4f13.5,4x,i7)
    return

  end subroutine stats

end module benchstats
