!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.3         !
!                                                                         !
!          O p e n M P    M U L T I - Z O N E    V E R S I O N            !
!                                                                         !
!                           L U - M Z - O M P                             !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is an OpenMP version of the NPB LU code.              !
!    Refer to NAS Technical Reports 95-020 for details.                   !
!                                                                         !
!    Permission to use, copy, distribute and modify this software         !
!    for any purpose with or without fee is hereby granted.  We           !
!    request, however, that all derived work reference the NAS            !
!    Parallel Benchmarks 3.3. This software is provided "as is"           !
!    without express or implied warranty.                                 !
!                                                                         !
!    Information on NPB 3.3, including the technical report, the          !
!    original specifications, source code, results and information        !
!    on how to submit new results, is available at:                       !
!                                                                         !
!           http://www.nas.nasa.gov/Software/NPB/                         !
!                                                                         !
!    Send comments or suggestions to  npb@nas.nasa.gov                    !
!                                                                         !
!          NAS Parallel Benchmarks Group                                  !
!          NASA Ames Research Center                                      !
!          Mail Stop: T27A-1                                              !
!          Moffett Field, CA   94035-1000                                 !
!                                                                         !
!          E-mail:  npb@nas.nasa.gov                                      !
!          Fax:     (650) 604-3957                                        !
!                                                                         !
!-------------------------------------------------------------------------!

c---------------------------------------------------------------------
c
c Authors: S. Weeratunga
c          V. Venkatakrishnan
c          E. Barszcz
c          M. Yarrow
C          R.F. Van der Wijngaart
C          H. Jin
c
c---------------------------------------------------------------------

c---------------------------------------------------------------------
      program applu
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   driver for the performance evaluation of the solver for
c   five coupled parabolic/elliptic partial differential equations.
c
c---------------------------------------------------------------------

      implicit none

      include 'header.h'
      include 'omp_stuff.h'

      integer num_zones
      parameter (num_zones=x_zones*y_zones)

      integer   nx(num_zones), nxmax(num_zones), ny(num_zones), 
     $          nz(num_zones),
     &          proc_zone_id(num_zones), proc_num_zones

c---------------------------------------------------------------------
c   Define all field arrays as one-dimenional arrays, to be reshaped
c---------------------------------------------------------------------

      double precision u     (proc_max_size5),
     >                 rsd   (proc_max_size5),
     >                 frct  (proc_max_size5),
     >                 qs    (proc_max_size ),
     >                 rho_i (proc_max_size ),
     >                 qbc   (proc_max_bcsize)

      common/cvar/ u, rsd, frct, qs, rho_i, qbc

c---------------------------------------------------------------------
c   2D auxiliary arrays are dimensioned to accommodate the largest
c   zone cross section
c---------------------------------------------------------------------

      double precision a (25*problem_size*problem_size), 
     $                 b (25*problem_size*problem_size), 
     $                 c (25*problem_size*problem_size), 
     $                 d (25*problem_size*problem_size),
     $                 au(25*problem_size*problem_size), 
     $                 bu(25*problem_size*problem_size), 
     $                 cu(25*problem_size*problem_size), 
     $                 du(25*problem_size*problem_size),
     $                 tv( 5*problem_size*problem_size),
     $                 phi1 (problem_size*problem_size),
     $                 phi2 (problem_size*problem_size)

!!      common/cjac/ a, b, c, d, au, bu, cu, du, tv, phi1, phi2

      integer isync(0:problem_size)

      logical verified
      double precision mflops, nsur, navg, n3

      integer i, zone, step, iz, tot_threads, nthreads
      double precision t, tmax, timer_read, trecs(t_last),
     $                 rsdnm(5), rsdnm_aux(5), errnm(5), errnm_aux(5),
     $                 frc, frc_aux
      external timer_read
      character t_names(t_last)*8


c---------------------------------------------------------------------
c   read input data
c---------------------------------------------------------------------
      call read_input(tot_threads)

      if (timeron) then
         t_names(t_total)  = 'total'
         t_names(t_rhsx)   = 'rhsx'
         t_names(t_rhsy)   = 'rhsy'
         t_names(t_rhsz)   = 'rhsz'
         t_names(t_rhs)    = 'rhs'
         t_names(t_jacld)  = 'jacld'
         t_names(t_blts)   = 'blts'
         t_names(t_jacu)   = 'jacu'
         t_names(t_buts)   = 'buts'
         t_names(t_add)    = 'add'
         t_names(t_l2norm) = 'l2norm'
         t_names(t_rdis1)  = 'qbc_copy'
         t_names(t_rdis2)  = 'qbc_comm'
      endif

c---------------------------------------------------------------------
c   set up domain sizes
c---------------------------------------------------------------------
      call zone_setup(nx, nxmax, ny, nz)

      call omp_setup(num_zones, nx, ny, nz, tot_threads)
      call zone_starts(num_zones, nx, nxmax, ny, nz)

c---------------------------------------------------------------------
c   set up coefficients
c---------------------------------------------------------------------
      call setcoeff()


!$omp parallel private(iz,i,zone,step,t,tmax,trecs,nthreads,isync,
!$omp&  a,b,c,d,au,bu,cu,du,tv,phi1,phi2,errnm_aux,rsdnm_aux,frc_aux,
!$omp&  proc_num_zones,proc_zone_id)
!$omp&  if(nested.ne.2)

      call omp_init(num_zones, proc_zone_id, proc_num_zones)

      nthreads = proc_num_threads(myid+1)

      do i = 1, t_last
         call timer_clear(i)
      end do

!$omp parallel private(iz,zone) num_threads(nthreads)
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)

c---------------------------------------------------------------------
c   set the boundary values for dependent variables
c---------------------------------------------------------------------
        call setbv(u(start5(zone)),
     $             nx(zone), nxmax(zone), ny(zone), nz(zone))

c---------------------------------------------------------------------
c   set the initial values for dependent variables
c---------------------------------------------------------------------
        call setiv(u(start5(zone)),
     $             nx(zone), nxmax(zone), ny(zone), nz(zone))

c---------------------------------------------------------------------
c   compute the forcing term based on prescribed exact solution
c---------------------------------------------------------------------
        call erhs(frct(start5(zone)), rsd(start5(zone)),
     $            nx(zone), nxmax(zone), ny(zone), nz(zone))

c---------------------------------------------------------------------
c   compute the steady-state residuals
c---------------------------------------------------------------------
        call rhs(u(start5(zone)), rsd(start5(zone)), 
     $           frct(start5(zone)), qs(start1(zone)), 
     $           rho_i(start1(zone)), 
     $           nx(zone), nxmax(zone), ny(zone), nz(zone))

      end do

c---------------------------------------------------------------------
c   initialize a,b,c,d to zero (guarantees that page tables have been
c   formed, if applicable on given architecture, before timestepping).
c   extra working arrays au, bu, cu, du are used in the OpenMP version
c   to align/touch data pages properly in the upper triangular solver.
c---------------------------------------------------------------------
      zone = proc_zone_id(1)
      call init_workarray(nx(zone), nxmax(zone), ny(zone),
     $                    a, b, c, d, au, bu, cu, du, tv)
!$omp end parallel

c---------------------------------------------------------------------
c   perform one SSOR iteration to touch all data pages
c---------------------------------------------------------------------
      call exch_qbc(u, qbc, nx, nxmax, ny, nz, 
     &              proc_zone_id, proc_num_zones)

!$omp parallel private(iz,zone) num_threads(nthreads)
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)
        call ssor(u(start5(zone)), rsd(start5(zone)), 
     $            frct(start5(zone)), qs(start1(zone)), 
     $            rho_i(start1(zone)), tv, 
     $            a, b, c, d, au, bu, cu, du, 
     $            nx(zone), nxmax(zone), ny(zone), nz(zone),
     &            isync)
      end do
!$omp end parallel

c---------------------------------------------------------------------
c   reset the boundary and initial values
c---------------------------------------------------------------------
!$omp parallel private(iz,zone) num_threads(nthreads)
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)

        call setbv(u(start5(zone)),
     $             nx(zone), nxmax(zone), ny(zone), nz(zone))

        call setiv(u(start5(zone)),
     $             nx(zone), nxmax(zone), ny(zone), nz(zone))

c---------------------------------------------------------------------
c   compute the steady-state residuals
c---------------------------------------------------------------------
        call rhs(u(start5(zone)), rsd(start5(zone)), 
     $           frct(start5(zone)), qs(start1(zone)), 
     $           rho_i(start1(zone)), 
     $           nx(zone), nxmax(zone), ny(zone), nz(zone))

      end do
!$omp end parallel

c---------------------------------------------------------------------
c   begin pseudo-time stepping iterations
c---------------------------------------------------------------------

      do i = 1, t_last
         call timer_clear(i)
      end do

!$omp barrier
      call timer_start(1)

c---------------------------------------------------------------------
c   the timestep loop
c---------------------------------------------------------------------
      do step = 1, itmax

        if (mod(step,20) .eq. 0 .or. step .eq. 1 .or.
     >        step .eq. itmax) then
!$omp master
           write( *, 200) step
 200       format(' Time step ', i4)
!$omp end master
        endif

        call exch_qbc(u, qbc, nx, nxmax, ny, nz, 
     &                proc_zone_id, proc_num_zones)

c---------------------------------------------------------------------
c   perform the SSOR iterations
c---------------------------------------------------------------------

!$omp parallel private(iz,zone) num_threads(nthreads)
        do iz = 1, proc_num_zones
          zone = proc_zone_id(iz)
          call ssor(u(start5(zone)), rsd(start5(zone)), 
     $              frct(start5(zone)), qs(start1(zone)), 
     $              rho_i(start1(zone)), tv, 
     $              a, b, c, d, au, bu, cu, du, 
     $              nx(zone), nxmax(zone), ny(zone), nz(zone),
     &              isync)
        end do
!$omp end parallel

      end do

!$omp master
      do i = 1, 5
         rsdnm(i) = 0.d0
         errnm(i) = 0.d0
      end do
      frc = 0.d0
!$omp end master
!$omp barrier

c---------------------------------------------------------------------
c   compute the max-norms of newton iteration residuals
c---------------------------------------------------------------------
      if (timeron) call timer_start(t_l2norm)
!$omp parallel private(iz,zone,i) num_threads(nthreads)
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)
        call l2norm(rsd(start5(zone)), rsdnm_aux, 
     $              nx(zone), nxmax(zone), ny(zone), nz(zone))
!$omp master
        do i = 1, 5
!$omp atomic
          rsdnm(i) = rsdnm(i) + rsdnm_aux(i)
        end do
!$omp end master
      end do
!$omp end parallel

      if (timeron) call timer_stop(t_l2norm)

!$omp barrier
      call timer_stop(1)
      tmax = timer_read(1)

c---------------------------------------------------------------------
c   compute the solution error and surface integral
c---------------------------------------------------------------------
!$omp parallel private(iz,zone,i) num_threads(nthreads)
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)
        call error(u(start5(zone)), errnm_aux,
     $             nx(zone), nxmax(zone), ny(zone), nz(zone))
        call pintgr(u(start5(zone)), phi1, phi2, frc_aux,
     $              nx(zone), nxmax(zone), ny(zone), nz(zone))
!$omp master
        do i = 1, 5
!$omp atomic
          errnm(i) = errnm(i) + errnm_aux(i)
        end do
!$omp atomic
        frc = frc + frc_aux
!$omp end master
      end do
!$omp end parallel


c---------------------------------------------------------------------
c   verification test
c---------------------------------------------------------------------
!$omp barrier
!$omp master
      call verify ( rsdnm, errnm, frc, verified )


      maxtime = tmax
      mflops = 0.d0

      if (maxtime .ne. 0.d0) then
        do zone = 1, num_zones
          n3 = dble(nx(zone))*ny(zone)*nz(zone)
          navg = (nx(zone) + ny(zone) + nz(zone))/3.d0
          nsur = (nx(zone)*ny(zone) + nx(zone)*nz(zone) +
     >            ny(zone)*nz(zone))/3.d0
          mflops = mflops + float(itmax)*1.0d-6 *
     >       (1984.77d0 * n3 - 10923.3d0 * nsur
     >         + 27770.9d0 * navg - 144010.d0)
     >       / maxtime
        end do
      endif

      call print_results('LU-MZ', class, gx_size, gy_size, gz_size, 
     >  itmax, maxtime, mflops, num_othreads, tot_threads,
     >  '          floating point', verified, 
     >  npbversion, compiletime, cs1, cs2, cs3, cs4, cs5, cs6, 
     >  '(none)')

!$omp end master
!$omp barrier

c---------------------------------------------------------------------
c      More timers
c---------------------------------------------------------------------
      if (.not.timeron) goto 999

      do i=1, t_last
         trecs(i) = timer_read(i)
      end do
      tmax = maxtime
      if (tmax .eq. 0.0) tmax = 1.0

!$omp critical (ptime)
      write(*,800) myid, nthreads
 800  format(' Myid =',i5,'   num_threads =',i4/
     >       '  SECTION   Time (secs)')
      do i=1, t_last
         write(*,810) t_names(i), trecs(i), trecs(i)*100./tmax
         if (i.eq.t_rhs) then
            t = trecs(t_rhsx) + trecs(t_rhsy) + trecs(t_rhsz)
            write(*,820) 'sub-rhs', t, t*100./tmax
            t = trecs(i) - t
            write(*,820) 'rest-rhs', t, t*100./tmax
         elseif (i.eq.t_rdis2) then
            t = trecs(t_rdis1) + trecs(t_rdis2)
            write(*,820) 'exch_qbc', t, t*100./tmax
         endif
 810     format(2x,a8,':',f9.3,'  (',f6.2,'%)')
 820     format(5x,'--> total ',a8,':',f9.3,'  (',f6.2,'%)')
      end do
      write(*,*)
!$omp end critical (ptime)

 999  continue

!$omp end parallel

      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine init_workarray(nx, nxmax, ny, a, b, c, d, 
     $                          au, bu, cu, du, tv)
      implicit none

c---------------------------------------------------------------------
c   initialize a,b,c,d to zero (guarantees that page tables have been
c   formed, if applicable on given architecture, before timestepping).
c   extra working arrays au, bu, cu, du are used in the OpenMP version
c   to align/touch data pages properly in the upper triangular solver.
c---------------------------------------------------------------------

      integer nx, nxmax, ny
      double precision a (25,2:nxmax-1,ny), b (25,2:nxmax-1,ny),
     $                 c (25,2:nxmax-1,ny), d (25,2:nxmax-1,ny),
     $                 au(25,2:nxmax-1,ny), bu(25,2:nxmax-1,ny),
     $                 cu(25,2:nxmax-1,ny), du(25,2:nxmax-1,ny),
     $                 tv( 5,2:nxmax-1,ny)

      integer i, j, m

!$OMP DO SCHEDULE(STATIC)
      do j = 2, ny-1
        do i = 2, nx-1
          do m = 1, 25
            a(m,i,j) = 0.d0
            b(m,i,j) = 0.d0
            c(m,i,j) = 0.d0
            d(m,i,j) = 0.d0
          end do
        end do
      end do
!$OMP END DO nowait
!$OMP DO SCHEDULE(STATIC)
      do j = ny-1, 2, -1
        do i = nx-1, 2, -1
          do m = 1, 25
            au(m,i,j) = 0.d0
            bu(m,i,j) = 0.d0
            cu(m,i,j) = 0.d0
            du(m,i,j) = 0.d0
          end do
          do m = 1, 5
            tv(m,i,j) = 0.d0
          end do
        end do
      end do
!$OMP END DO nowait

      return
      end
