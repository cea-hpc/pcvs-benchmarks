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

program syncbench

  use benchdata
  implicit none

! GET NUMBER OF THREADS

!$OMP PARALLEL 
!$OMP MASTER
      nthreads=omp_get_num_threads()
!$OMP END MASTER 
!$OMP END PARALLEL 
  print *,"Running OpenMP benchmark on ",nthreads," thread(s)"

! GENERATE REFERENCE TIME 

  delaylength = 500
  innerreps = 10000
  call refer() 

! TEST  PARALLEL REGION

  innerreps = 1000
  call testpr() 

! TEST DO

  call testdo() 

! TEST PARALLEL DO

  call testpdo() 

! TEST BARRIER

  call testbar() 

! TEST SINGLE

  call testsing() 

! TEST CRITICAL

  innerreps = 100000  
  call testcrit()     

! TEST LOCK/UNLOCK

  call testlock() 

! TEST ORDERED

  innerreps = 1000
  call testorder() 

! GENERATE NEW REFERENCE TIME 

  innerreps = 100000
  call referatom() 

! TEST ATOMIC

  innerreps = 100000 
  call testatom() 

! GENERATE NEW REFERENCE TIME 

  innerreps = 10000
  call referred() 

! TEST REDUCTION 

  innerreps = 1000
  call testred() 

! OMP 2.0 features inside this ifdef statement 
#ifdef OMPVER2    

! GENERATE NEW REFERENCE TIME 

    innerreps = 10000
    call referws() 

! TEST WORKSHARE

    innerreps = 1000
    call testws() 

! TEST PARALLEL WORKSHARE

    call testpws() 

#endif    
! End of OMP 2.0 features

  stop
end program syncbench

!************ External subroutines below **************

subroutine testpr()

  use benchdata
  use benchstats
  implicit none

  integer :: j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) ::  meantime, sd 
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing PARALLEL time") 
      
  dl = delaylength 
  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
    do j=1,innerreps 
!$OMP PARALLEL 
      call delay(dl)
!$OMP END PARALLEL 
    end do
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("PARALLEL time =                       ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("PARALLEL overhead =                   ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testpr

subroutine testdo()

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
  1100 format("Computing DO time") 
      
  dl = delaylength 

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL PRIVATE(J) 
    do j=1,innerreps
!$OMP DO 
      do i=1,nthreads
        call delay(dl)
      enddo
!$OMP END DO 
    end do
!$OMP END PARALLEL 
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("DO time =                             ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("DO overhead =                         ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testdo

subroutine testpdo()

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
  1100 format("Computing PARALLEL DO time") 

  dl = delaylength 

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
    do j=1,innerreps
!$OMP PARALLEL DO 
      do i=1,nthreads
        call delay(dl)
      enddo
!$OMP END PARALLEL DO 
    end do
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 

  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("PARALLEL_DO time =                    ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("PARALLEL_DO overhead =                ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testpdo

subroutine testbar()

  use benchdata
  use benchstats
  implicit none

  integer :: j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing BARRIER time") 
      
  dl = delaylength 

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL  PRIVATE(J) 
      do j=1,innerreps 
        call delay(dl)
!$OMP BARRIER
      end do
!$OMP END PARALLEL 

      time(k) = (getclock() - start) * 1.0e6 / dble (innerreps)

  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("BARRIER time =                        ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("BARRIER overhead =                    ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testbar

subroutine testsing()

  use benchdata
  use benchstats
  implicit none

  integer :: j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing SINGLE time") 

  dl = delaylength 

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL  PRIVATE(J) 
    do j=1,innerreps 
!$OMP SINGLE 
      call delay(dl)
!$OMP END SINGLE
    end do
!$OMP END PARALLEL 
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("SINGLE time =                         ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("SINGLE overhead =                     ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testsing

subroutine testred()

  use benchdata
  use benchstats
  implicit none

  integer :: j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock 
  real (kind = dp) :: bbbb

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing REDUCTION time") 
      
  dl = delaylength 

  do k=0,outerreps

!     PARALLEL TIME 

    bbbb = 0.0 
    start  = getclock()
    do j=1,innerreps 
!$OMP PARALLEL REDUCTION(+:bbbb)
      call delay(dl)
      bbbb = bbbb + 1.0 
!$OMP END PARALLEL 
    end do
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
    if (bbbb < 0.0) print *, bbbb
  end do

!     PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("REDUCTION time =                ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("REDUCTION overhead =            ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testred

! OMP 2.0 features inside this ifdef statement 
#ifdef OMPVER2    
subroutine testws()

  use benchdata
  use benchstats
  implicit none

  integer :: i, j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp), dimension(:), allocatable :: a
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing WORKSHARE time") 
       
  allocate (a(nthreads))
  a = 0.5

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL PRIVATE(J) 
    do j=1,innerreps
!$OMP WORKSHARE
      a = a + cos(a) - sin(a) 
!$OMP END WORKSHARE
    end do
!$OMP END PARALLEL 
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("WORKSHARE time =                             ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("WORKSHARE overhead =                         ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testws

subroutine testpws()

  use benchdata
  use benchstats
  implicit none

  integer :: i, j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp), dimension(:), allocatable :: a
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing PARALLEL WORKSHARE time") 
      
  allocate (a(nthreads))
  a = 0.5

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
    do j=1,innerreps
!$OMP PARALLEL WORKSHARE
      a = a + cos(a) - sin(a)  
!$OMP END PARALLEL WORKSHARE
    end do
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("PARALLEL_WORKSHARE time =                     ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("PARALLEL_WORKSHARE overhead =                 ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testpws
#endif    
! End of OMP 2.0 features

subroutine testcrit()

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
  1100 format("Computing CRITICAL time") 
      
  dl = delaylength 

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL  PRIVATE(J) 
    do j=1,innerreps/nthreads
!$OMP CRITICAL 
      call delay(dl)
!$OMP END CRITICAL 
    end do
!$OMP END PARALLEL 
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 

  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("CRITICAL time =                       ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("CRITICAL overhead =                   ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testcrit

subroutine testlock()

  use benchdata
  use benchstats
  implicit none

  integer :: j, k, dl
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock
  integer (kind=omp_lock_kind) :: lock 

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing LOCK/UNLOCK time") 

  call omp_init_lock(lock)
  dl = delaylength   
      
  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL  PRIVATE(J) 
    do j=1,innerreps/nthreads 
      call omp_set_lock(lock)
      call delay(dl)
      call omp_unset_lock(lock)
    end do
!$OMP END PARALLEL 
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("LOCK/UNLOCK time =                    ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("LOCK/UNLOCK overhead =                ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testlock

subroutine testorder()

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
  1100 format("Computing ORDERED time") 
      
  dl = delaylength 

  do k=0,outerreps

! PARALLEL TIME 

    start  = getclock()
!$OMP PARALLEL DO ORDERED SCHEDULE(STATIC,1) 
    do j=1,innerreps
!$OMP ORDERED
      call delay(dl)
!$OMP END ORDERED 
    end do 
!$OMP END PARALLEL DO  
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("ORDERED time =                       ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("ORDERED overhead =                   ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testorder

subroutine testatom()

  use benchdata
  use benchstats
  implicit none

  integer :: j, k 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock
  real (kind = dp) :: aaaa

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing ATOMIC time") 

  do k=0,outerreps

! PARALLEL TIME 

    aaaa = 0.0 
    start  = getclock()
!$OMP PARALLEL  PRIVATE(J) 
    do j=1,innerreps/nthreads 
!$OMP ATOMIC 
      aaaa = aaaa + 1.0  
    end do
!$OMP END PARALLEL 
    time(k) =  (getclock() - start) * 1.0e6 / dble (innerreps)
    if (aaaa < 0.0) print *, aaaa 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("ATOMIC time =                         ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("ATOMIC overhead =                     ",f8.2,&
  " microseconds +/- ", f12.3)
  return

end subroutine testatom

subroutine refer() 

  use benchdata
  use benchstats
  implicit none

  integer :: j, k, dl
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd 
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing reference time 1") 

! SEQUENTIAL REFERENCE TIME 

  dl = delaylength 

  do k=0,outerreps
    start  = getclock()
    do j=1,innerreps
      call delay(dl)
    end do
    time(k) = (getclock() - start)* 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  reftime = meantime 
  refsd = sd 
  write (6,1000) reftime, conf95*refsd
  1000 format("Reference_time_1 =                    ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  return

end subroutine refer

subroutine referatom() 

  use benchdata
  use benchstats
  implicit none

  integer :: j, k 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd 
  real (kind = dp) :: getclock 
  real (kind = dp) :: aaaa

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing reference time 2") 

! SEQUENTIAL REFERENCE TIME 

  do k=0,outerreps
         
    aaaa = 0.0 
    start  = getclock()
    do j=1,innerreps
      aaaa = aaaa + 1.0 
    end do
    time(k) = (getclock() - start)* 1.0e6 / dble (innerreps) 
    if (aaaa < 0.) print *, aaaa

  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  reftime = meantime 
  refsd = sd 
  write (6,1000) reftime, conf95*refsd
  1000 format("Reference_time_2 =                    ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  return

end subroutine referatom

subroutine referred() 

  use benchdata
  use benchstats
  implicit none

  integer :: j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock 
  real (kind = dp) :: bbbb

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing reference time 3") 

! SEQUENTIAL REFERENCE TIME 

  dl = delaylength 

  do k=0,outerreps
    bbbb = 0.0 
    start  = getclock()
    do j=1,innerreps
      call delay(dl)
      bbbb = bbbb + 1.0 
    end do
    if (bbbb < 0.0) print *, bbbb
    time(k) = (getclock() - start)* 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  reftime = meantime 
  refsd = sd 
  write (6,1000) reftime, conf95*refsd
  1000 format("Reference_time_3 =                    ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  return

end subroutine referred

! OMP 2.0 features inside this ifdef statement 
#ifdef OMPVER2    
subroutine referws() 

  use benchdata
  use benchstats
  implicit none

  integer :: j, k, dl
  real (kind = dp) :: start
  real (kind = dp), dimension(:), allocatable :: a
  real (kind = dp) :: meantime, sd
  real (kind = dp) :: getclock 
  allocate(a(1))

  write(6,*) 
  write(6,1010)
  write(6,1100)
  1010 format("--------------------------------------------------------")
  1100 format("Computing reference time 4") 

! SEQUENTIAL REFERENCE TIME 

  dl = delaylength 
  a = 0.5 

  do k=0,outerreps
    start  = getclock()
      do j=1,innerreps
        a = a + cos(a) - sin(a)  
      end do
      time(k) = (getclock() - start)* 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  reftime = meantime 
  refsd = sd 
  write (6,1000) reftime, conf95*refsd
  1000 format("Reference_time_4 =                    ",f8.2,&
  " microseconds +/- ", f12.3)
  write (6,*) 
  return

end subroutine referws
#endif    
! End of OMP 2.0 features
