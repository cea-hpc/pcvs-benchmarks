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

program schedbench

  use benchdata
  implicit none

! GET NUMBER OF THREADS

!$OMP PARALLEL 
!$OMP MASTER
  nthreads=omp_get_num_threads()
!$OMP END MASTER 
!$OMP END PARALLEL 
  print *,"Running OpenMP scheduling benchmark on ",nthreads,&
  " thread(s)"

! FIX LOOP BODY LENGTH 

  call getdelay() 

! GENERATE REFERENCE TIME 

  call refer() 

! TEST STATIC 

  call teststatic()

! TEST STATIC,n 
 
  cksz = 1
  do while (cksz .le. itersperthr)
    call teststaticn() 
    cksz = cksz * 2 
  end do 

! TEST DYNAMIC,n 

  cksz = 1 
  do while (cksz .le. itersperthr)
    call testdynamicn() 
    cksz = cksz * 2 
  end do 

! TEST GUIDED,n 

  cksz = 1 
  do while (cksz .le. itersperthr/nthreads)
    call testguidedn() 
    cksz = cksz * 2 
  end do 

  stop
end program schedbench

subroutine teststatic()

  use benchdata
  use benchstats
  implicit none

  integer :: i, j, k, dl
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd 
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing STATIC time") 
      
  dl = delaylength 

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL PRIVATE(J) 
      do j=1,innerreps_sched
!$OMP DO SCHEDULE(STATIC) 
        do i=1,itersperthr*nthreads
          call delay(dl)
        end do
      end do
!$OMP END PARALLEL
      time(k) = (getclock() - start) * 1.0e6 / dble (innerreps_sched) 
    end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("STATIC time =                         ",f10.2,&
      " microseconds +/- ", f8.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("STATIC overhead =                   ",f10.2,&
      " microseconds +/- ", f8.3)
  return

end subroutine teststatic

subroutine teststaticn()

  use benchdata
  use benchstats
  implicit none

  integer :: i, j, k, dl, mycksz 
!  mycksz - horrid workaround for Guide 3.7 bug! 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100) cksz 
  1010 format("--------------------------------------------------------")
  1100 format("Computing STATIC",i5," time") 

  dl = delaylength 
      
  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL PRIVATE (j,mycksz) 
    mycksz = cksz  
    do j=1,innerreps_sched
!$OMP DO SCHEDULE(STATIC,mycksz) 
      do i=1,itersperthr*nthreads
        call delay(dl)
      end do
    end do
!$OMP END PARALLEL
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps_sched) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) cksz, meantime, conf95*sd
  1000 format("STATIC",i5," time =                       ",f10.2,&
      " microseconds +/- ", f8.3)
  write (6,*) 
  write (6,1001) cksz, meantime-reftime, conf95*(sd+refsd)
  1001 format("STATIC",i5," overhead =                   ",f10.2,&
      " microseconds +/- ", f8.3)
  return

end subroutine teststaticn

subroutine testdynamicn()

  use benchdata
  use benchstats
  implicit none

  integer :: i, j, k, dl, mycksz 
!  mycksz - horrid workaround for Guide 3.7 bug! 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100) cksz 
  1010 format("--------------------------------------------------------")
  1100 format("Computing DYNAMIC",i5," time") 
       
  dl = delaylength 

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL PRIVATE (j, mycksz)
    mycksz = cksz  
    do j=1,innerreps_sched
!$OMP DO SCHEDULE(DYNAMIC,mycksz) 
      do i=1,itersperthr*nthreads
        call delay(dl)
      end do
    end do
!$OMP END PARALLEL
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps_sched) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) cksz, meantime, conf95*sd
  1000 format("DYNAMIC",i5," time =                       ",f10.2,&
      " microseconds +/- ", f8.3)
  write (6,*) 
  write (6,1001) cksz, meantime-reftime, conf95*(sd+refsd)
  1001 format("DYNAMIC",i5," overhead =                   ",f10.2,&
      " microseconds +/- ", f8.3)
  return

end subroutine testdynamicn

subroutine testguidedn()

  use benchdata
  use benchstats
  implicit none

  integer :: i, j, k, dl, mycksz 
!  mycksz - horrid workaround for Guide 3.7 bug! 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100) cksz 
  1010 format("--------------------------------------------------------")
  1100 format("Computing GUIDED",i5," time") 

  dl = delaylength 
      
  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL PRIVATE (j,mycksz)
    mycksz = cksz 
    do j=1,innerreps_sched
!$OMP DO SCHEDULE(GUIDED,mycksz) 
      do i=1,itersperthr*nthreads
        call delay(dl)
      end do
    end do
!$OMP END PARALLEL
    time(k) = (getclock() - start) * 1.0e6 / real (innerreps_sched) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) cksz, meantime, conf95*sd
  1000 format("GUIDED",i5," time =                       ",f10.2,&
      " microseconds +/- ", f8.3)
  write (6,*) 
  write (6,1001) cksz, meantime-reftime, conf95*(sd+refsd)
  1001 format("GUIDED",i5," overhead =                   ",f10.2,&
      " microseconds +/- ", f8.3)
  return

end subroutine testguidedn

subroutine refer() 

  use benchdata
  use benchstats
  implicit none

  integer :: i, j, k, dl
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing reference time ") 

! SEQUENTIAL REFERENCE TIME 

  dl = delaylength 

  do k=0,outerreps
    start  = getclock()
    do j=1,innerreps_sched
      do i=1,itersperthr
        call delay(dl)
      end do
    end do
    time(k) = (getclock() - start)* 1.0e6 / real (innerreps_sched) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  reftime = meantime 
  refsd = sd 
  write (6,1000) reftime, conf95*refsd
  1000 format("Reference_time =                      ",f10.2,&
  " microseconds +/- ", f8.3)
  write (6,*) 
 return

end subroutine refer

subroutine getdelay() 

  use benchdata
  use benchstats
  implicit none

  integer :: i, reps, dl  
  real (kind = dp) :: actualtime, targettime
  real (kind = dp) :: start
  real (kind = dp) :: getclock   

! CHOOSE delaylength SO THAT call delay(delaylength) 
! TAKES APPROXIMATELY 100 CPU CLOCK CYCLES
 
  dl = 0
  reps = itersperthr * innerreps_sched 
  actualtime = 0. 
  targettime = 100.0 / mhz * 1e-06 

  call delay(dl) 

  do while (actualtime .lt. targettime) 
    dl = dl * 1.1 + 1 
    start = getclock()
      do i = 1, reps  
        call delay(dl) 
      end do 
      actualtime  = (getclock() - start) / dble  (reps)
  end do
  
  start = getclock()
  do i = 1, reps  
    call delay(dl) 
  end do 
  actualtime  = (getclock() - start) / dble (reps)
  delaylength = dl 
  print *, "Assumed clock rate = ",mhz," MHz "
  print *, "Delay length = ", dl
  print *, "Delay time  = ", actualtime * mhz * 1e6, "  cycles"
  return

end subroutine getdelay
