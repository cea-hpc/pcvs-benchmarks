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

program arraybench

  use arraydata 
  use benchdata

  implicit none
  integer :: i
  real (kind = dp), dimension(sz) :: atemp
  real (kind = dp), dimension(1) :: aref

! GET NUMBER OF THREADS

!$OMP PARALLEL 
!$OMP MASTER
  nthreads=omp_get_num_threads()
!$OMP END MASTER 
!$OMP END PARALLEL 

  write(6,*) "Running OpenMP benchmark on ",nthreads," thread(s)"

! GENERATE REFERENCE TIME 

  delaylength = 500
  innerreps = 100
  aref(1) = 1.0
  call refer(aref)

! TEST  PRIVATE CLAUSE 
 
  delaylength = 500
  innerreps = 100
  atemp = 1.0
  call testprivate(atemp) 

! TEST  FIRSTPRIVATE CLAUSE

  delaylength = 500
  innerreps = 100
  atemp = 1.0 
  call testfirstpriv(atemp)
 
! TEST  COPYPRIVATE CLAUSE

  delaylength = 500
  innerreps = 100
#ifdef OMPVER2
  call testcopyprivnew()
#endif

! TEST COPYIN CLAUSE 

  delaylength = 500
  innerreps = 100
  call testcopyinnew()

! TEST REDUCTION CLAUSE

  delaylength = 500
  innerreps = 100
#ifdef OMPVER2
  call testreducnew() 
#endif

end program arraybench

subroutine testprivate(a)

  use arraydata
  use benchdata
  use benchstats

  integer :: j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd 
  real (kind = dp) :: getclock 
  real (kind = dp), dimension(sz) :: a

  write(6,*) 
  write(6,1010)
  write(6,1100) sz
  1010 format("--------------------------------------------------------")
  1100 format("Computing PRIVATE",i7," time") 
      
  dl = delaylength 
  do k=0,outerreps

! PRIVATE TIME 

    start  = getclock()
    do j=1,innerreps 
!$OMP PARALLEL PRIVATE(a)
      call arraydelay(dl,a)
!$OMP END PARALLEL 
    end do
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("PRIVATE time =                       ",f10.2,&
      " microseconds +/- ", f9.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("PRIVATE overhead =                   ",f10.2,&
      " microseconds +/- ", f9.3)
  return

end subroutine testprivate

subroutine testfirstpriv(a)

  use arraydata
  use benchdata
  use benchstats

  integer :: j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd 
  real (kind = dp) :: getclock 
  real (kind = dp), dimension(sz) :: a

  write(6,*) 
  write(6,1010)
  write(6,1100) sz
  1010 format("--------------------------------------------------------")
  1100 format("Computing FIRSTPRIVATE",i7," time") 
      
  dl = delaylength 
  do k=0,outerreps

! PRIVATE TIME 

    start  = getclock()
    do j=1,innerreps 
!$OMP PARALLEL FIRSTPRIVATE(a)
      call arraydelay(dl,a)
!$OMP END PARALLEL 
    end do
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  1000 format("FIRSTPRIVATE time =                       ",f10.2,&
      " microseconds +/- ", f9.3)
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  1001 format("FIRSTPRIVATE overhead =                   ",f10.2,&
      " microseconds +/- ", f9.3)
  return

end subroutine testfirstpriv

subroutine refer(a) 

  use arraydata
  use benchdata
  use benchstats

  real (kind = dp), dimension(1) :: a 
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
      call arraydelay(dl, a)
    end do
    time(k) = (getclock() - start)* 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  reftime = meantime 
  refsd = sd 
  write (6,1000) reftime, conf95*refsd
  1000 format("Reference_time_1 =                    ",f10.2,&
  " microseconds +/- ", f9.3)
  write (6,*) 
  return

end subroutine refer

#ifdef OMPVER2
  subroutine testcopyprivnew()

    use arraydata
    use benchdata 
    use benchstats

    integer :: j, k, dl 
    real (kind = dp) :: start
    real (kind = dp) :: meantime, sd 
    real (kind = dp) :: getclock 

    write(6,*) 
    write(6,1010)
    write(6,1100) sz
    1010 format("--------------------------------------------------------")
    1100 format("Computing COPYPRIVATE",i7," time") 
      
    dl = delaylength 
    do k=0,outerreps

! COPYPRIVATE TIME 

      start  = getclock()
!$OMP PARALLEL 
      do j=1,innerreps 
!$OMP SINGLE 
        call arraydelay(dl,thrtest)
!$OMP END SINGLE COPYPRIVATE(thrtest)
      end do
!$OMP END PARALLEL 
      time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
    end do

! PROCESS TIMING DATA 

    call stats (meantime, sd) 

    write (6,1000) meantime, conf95*sd
    1000 format("COPYPRIVATE time =                       ",f10.2,&
      " microseconds +/- ", f9.3)
    write (6,*) 
    write (6,1001) meantime-reftime, conf95*(sd+refsd)
    1001 format("COPYPRIVATE overhead =                   ",f10.2,&
      " microseconds +/- ", f9.3)
    return

  end subroutine testcopyprivnew
#endif

subroutine testcopyinnew()

  use arraydata
  use benchdata 
  use benchstats

  integer :: j, k, dl 
  real (kind = dp) :: start
  real (kind = dp) :: meantime, sd 
  real (kind = dp) :: getclock 

  write(6,*) 
  write(6,1010)
  write(6,1100) sz
      
  thrtest = 1.0 
  dl = delaylength 
  do k=0,outerreps

! COPYIN TIME 

    start  = getclock()
    do j=1,innerreps 
!$OMP PARALLEL COPYIN(thrtest)
      call arraydelay(dl,thrtest)
!$OMP END PARALLEL 
    end do
    time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
  end do

! PROCESS TIMING DATA 

  call stats (meantime, sd) 

  write (6,1000) meantime, conf95*sd
  write (6,*) 
  write (6,1001) meantime-reftime, conf95*(sd+refsd)
  write(6,*)

  1010 format("--------------------------------------------------------")
  1100 format("Computing COPYIN",i7," time") 
  1000 format("COPYIN time =                       ",f10.2,&
  " microseconds +/- ", f9.3)
  1001 format("COPYIN overhead =                   ",f10.2,&
  " microseconds +/- ", f9.3)
  return

end subroutine testcopyinnew

#ifdef OMPVER2
  subroutine referrednew() 

    use arraydata
    use benchdata 
    use benchstats

    integer :: i, j, k, dl
    real (kind = dp) :: start
    real (kind = dp) :: meantime, sd 
    real (kind = dp) :: getclock 

    write(6,*)
    write(6,1010)
    write(6,1100)sz
    1010 format("--------------------------------------------------------")
    1100 format("Computing reference time red", i7) 

! SEQUENTIAL REFERENCE TIME 

    dl = delaylength 
    redtest = 3.0

    do k=0,outerreps
      start  = getclock()
      do j=1,innerreps
        do i=1,sz
          redtest(i) = redtest(i) + 1.0
        end do 
        call arraydelay(dl, redtest)
      end do
      time(k) = (getclock() - start)* 1.0e6 / dble (innerreps) 
    end do

! PROCESS TIMING DATA 

    call stats (meantime, sd) 

    reftime = meantime 
    refsd = sd 
    write (6,1000) reftime, conf95*refsd
    1000 format("Reference_time_red =                    ",f10.2,&
    " microseconds +/- ", f9.3)
    write (6,*) 
    return

  end subroutine referrednew

  subroutine testreducnew()

    use arraydata
    use benchdata
    use benchstats

    integer :: j, k, dl 
    real (kind = dp) :: start
    real (kind = dp) :: meantime, sd 
    real (kind = dp) :: getclock 

    call referrednew()

    write(6,*) 
    write(6,1010)
    write(6,1100) sz

    1010 format("--------------------------------------------------------")
    1100 format("Computing REDUCTION",i7," time") 
      
    dl = delaylength 
    redtest = 3.0 

    do k=0,outerreps

! PRIVATE TIME 

      start  = getclock()
      do j=1,innerreps 
!$OMP PARALLEL REDUCTION(+:redtest)
        do i=1,sz
          redtest(i) = redtest(i) + 1.0
        end do 
        call arraydelay(dl,redtest)
!$OMP END PARALLEL 
      end do
      time(k) = (getclock() - start) * 1.0e6 / dble (innerreps) 
    end do

! PROCESS TIMING DATA 

    call stats (meantime, sd) 

    write (6,1000) meantime, conf95*sd
    1000 format("REDUCTION time =                       ",f10.2,&
    " microseconds +/- ", f9.3)
    write (6,*) 
    write (6,1001) meantime-reftime, conf95*(sd+refsd)
    1001 format("REDUCTION overhead =                   ",f10.2,&
    " microseconds +/- ", f9.3)
    return

  end subroutine testreducnew
#endif
