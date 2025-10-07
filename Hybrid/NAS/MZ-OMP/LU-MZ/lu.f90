!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         !
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
!    Parallel Benchmarks 3.4. This software is provided "as is"           !
!    without express or implied warranty.                                 !
!                                                                         !
!    Information on NPB 3.4, including the technical report, the          !
!    original specifications, source code, results and information        !
!    on how to submit new results, is available at:                       !
!                                                                         !
!           http://www.nas.nasa.gov/Software/NPB/                         !
!                                                                         !
!    Send comments or suggestions to  npb@nas.nasa.gov                    !
!                                                                         !
!          NAS Parallel Benchmarks Group                                  !
!          NASA Ames Research Center                                      !
!          Moffett Field, CA   94035-1000                                 !
!                                                                         !
!-------------------------------------------------------------------------!

!---------------------------------------------------------------------
!
! Authors: S. Weeratunga
!          V. Venkatakrishnan
!          E. Barszcz
!          M. Yarrow
!          R.F. Van der Wijngaart
!          H. Jin
!
!---------------------------------------------------------------------

!---------------------------------------------------------------------
      program LU_MZ
!---------------------------------------------------------------------

!---------------------------------------------------------------------
!
!   driver for the performance evaluation of the solver for
!   five coupled parabolic/elliptic partial differential equations.
!
!---------------------------------------------------------------------

      use lu_data
      use lu_fields
      use ompnpb

      implicit none

      integer num_zones

      logical verified
      double precision mflops, nsur, navg, n3

      integer i, zone, step, iz, tot_threads, itimer, nthreads
      double precision t, tmax, timer_read, trecs(t_last),  &
     &                 tsum(t_last), tming(t_last), tmaxg(t_last),  &
     &                 rsdnm(5), rsdnm_aux(5), errnm(5), errnm_aux(5),  &
     &                 frc, frc_aux
      external timer_read
      character t_names(t_last)*8


!---------------------------------------------------------------------
!   read input data
!---------------------------------------------------------------------
      call read_input(tot_threads, itimer)

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

!---------------------------------------------------------------------
!   set up domain sizes
!---------------------------------------------------------------------
      call zone_setup(nx, nxmax, ny, nz)

      num_zones = max_zones
      call setup_omp(num_zones, nx, ny, nz, tot_threads)
      call zone_starts(num_zones, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!      allocate space for field arrays
!---------------------------------------------------------------------
       call alloc_field_space

!---------------------------------------------------------------------
!   set up coefficients
!---------------------------------------------------------------------
      call setcoeff()


      if (timeron) then
         do i = 1, t_last
            tsum(i) = 0.d0
            tming(i) = huge(0.d0)
            tmaxg(i) = 0.d0
         end do
      endif

!---------------------------------------------------------------------
!   start of the outer parallel region
!---------------------------------------------------------------------
!$omp parallel private(iz,i,zone,step,t,tmax,trecs,nthreads,isync,  &
!$omp&  a,b,c,d,au,bu,cu,du,tv,phi1,phi2,errnm_aux,rsdnm_aux,frc_aux,  &
!$omp&  proc_num_zones,proc_zone_id)  &
!$omp&  if(nested.ne.2)

      call init_omp(num_zones, proc_zone_id, proc_num_zones)

      nthreads = proc_num_threads(myid+1)
!$    call omp_set_num_threads(nthreads)

      do i = 1, t_last
         call timer_clear(i)
      end do

      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)

!---------------------------------------------------------------------
!   set the boundary values for dependent variables
!---------------------------------------------------------------------
        call setbv(u(start5(zone)),  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))

!---------------------------------------------------------------------
!   set the initial values for dependent variables
!---------------------------------------------------------------------
        call setiv(u(start5(zone)),  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))

!---------------------------------------------------------------------
!   compute the forcing term based on prescribed exact solution
!---------------------------------------------------------------------
        call erhs(frct(start5(zone)), rsd(start5(zone)),  &
     &            nx(zone), nxmax(zone), ny(zone), nz(zone))

!---------------------------------------------------------------------
!   compute the steady-state residuals
!---------------------------------------------------------------------
        call rhs(u(start5(zone)), rsd(start5(zone)),  &
     &           frct(start5(zone)), qs(start1(zone)),  &
     &           rho_i(start1(zone)),  &
     &           nx(zone), nxmax(zone), ny(zone), nz(zone))

      end do

!---------------------------------------------------------------------
!   initialize a,b,c,d to zero (guarantees that page tables have been
!   formed, if applicable on given architecture, before timestepping).
!   extra working arrays au, bu, cu, du are used in the OpenMP version
!   to align/touch data pages properly in the upper triangular solver.
!---------------------------------------------------------------------
      zone = proc_zone_id(1)
      call init_workarray(nx(zone), nxmax(zone), ny(zone),  &
     &                    a, b, c, d, au, bu, cu, du, tv)

!---------------------------------------------------------------------
!   perform one SSOR iteration to touch all data pages
!---------------------------------------------------------------------
      call exch_qbc(u, qbc, nx, nxmax, ny, nz,  &
     &              proc_zone_id, proc_num_zones)

      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)
        call ssor(u(start5(zone)), rsd(start5(zone)),  &
     &            frct(start5(zone)), qs(start1(zone)),  &
     &            rho_i(start1(zone)), tv,  &
     &            a, b, c, d, au, bu, cu, du,  &
     &            nx(zone), nxmax(zone), ny(zone), nz(zone),  &
     &            isync)
      end do

!---------------------------------------------------------------------
!   reset the boundary and initial values
!---------------------------------------------------------------------
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)

        call setbv(u(start5(zone)),  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))

        call setiv(u(start5(zone)),  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))

!---------------------------------------------------------------------
!   compute the steady-state residuals
!---------------------------------------------------------------------
        call rhs(u(start5(zone)), rsd(start5(zone)),  &
     &           frct(start5(zone)), qs(start1(zone)),  &
     &           rho_i(start1(zone)),  &
     &           nx(zone), nxmax(zone), ny(zone), nz(zone))

      end do

!---------------------------------------------------------------------
!   begin pseudo-time stepping iterations
!---------------------------------------------------------------------

      do i = 1, t_last
         call timer_clear(i)
      end do

!$omp barrier
      call timer_start(1)

!---------------------------------------------------------------------
!   the timestep loop
!---------------------------------------------------------------------
      do step = 1, itmax

!$omp master
        if (mod(step,20) .eq. 0 .or. step .eq. 1 .or.  &
     &        step .eq. itmax) then
           write( *, 200) step
 200       format(' Time step ', i4)
        endif
!$omp end master

        call exch_qbc(u, qbc, nx, nxmax, ny, nz,  &
     &                proc_zone_id, proc_num_zones)

!---------------------------------------------------------------------
!   perform the SSOR iterations
!---------------------------------------------------------------------

        do iz = 1, proc_num_zones
          zone = proc_zone_id(iz)
          call ssor(u(start5(zone)), rsd(start5(zone)),  &
     &              frct(start5(zone)), qs(start1(zone)),  &
     &              rho_i(start1(zone)), tv,  &
     &              a, b, c, d, au, bu, cu, du,  &
     &              nx(zone), nxmax(zone), ny(zone), nz(zone),  &
     &              isync)
        end do

      end do

!$omp master
      do i = 1, 5
         rsdnm(i) = 0.d0
         errnm(i) = 0.d0
      end do
      frc = 0.d0
!$omp end master
!$omp barrier

!---------------------------------------------------------------------
!   compute the max-norms of newton iteration residuals
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_l2norm)
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)
        call l2norm(rsd(start5(zone)), rsdnm_aux,  &
     &              nx(zone), nxmax(zone), ny(zone), nz(zone))
        do i = 1, 5
!$omp atomic
          rsdnm(i) = rsdnm(i) + rsdnm_aux(i)
        end do
      end do

      if (timeron) call timer_stop(t_l2norm)

!$omp barrier
      call timer_stop(1)
      tmax = timer_read(1)

!---------------------------------------------------------------------
!   compute the solution error and surface integral
!---------------------------------------------------------------------
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)
        call error(u(start5(zone)), errnm_aux,  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))
        call pintgr(u(start5(zone)), phi1, phi2, frc_aux,  &
     &              nx(zone), nxmax(zone), ny(zone), nz(zone))
        do i = 1, 5
!$omp atomic
          errnm(i) = errnm(i) + errnm_aux(i)
        end do
!$omp atomic
        frc = frc + frc_aux
      end do


!---------------------------------------------------------------------
!   verification test
!---------------------------------------------------------------------
!$omp barrier
!$omp master
      call verify ( rsdnm, errnm, frc, verified )


      maxtime = tmax
      mflops = 0.d0

      if (maxtime .ne. 0.d0) then
        do zone = 1, num_zones
          n3 = dble(nx(zone))*ny(zone)*nz(zone)
          navg = (nx(zone) + ny(zone) + nz(zone))/3.d0
          nsur = (nx(zone)*ny(zone) + nx(zone)*nz(zone) +  &
     &            ny(zone)*nz(zone))/3.d0
          mflops = mflops + float(itmax)*1.0d-6 *  &
     &       (1984.77d0 * n3 - 10923.3d0 * nsur  &
     &         + 27770.9d0 * navg - 144010.d0)  &
     &       / maxtime
        end do
      endif

      call print_results('LU-MZ', class, gx_size, gy_size, gz_size,  &
     &  itmax, maxtime, mflops, num_othreads, tot_threads,  &
     &  '          floating point', verified,  &
     &  npbversion, compiletime, cs1, cs2, cs3, cs4, cs5, cs6,  &
     &  '(none)')

!$omp end master
!$omp barrier

!---------------------------------------------------------------------
!      More timers
!---------------------------------------------------------------------
      if (.not.timeron) goto 999

      do i=1, t_last
         trecs(i) = timer_read(i)
      end do
      tmax = maxtime
      if (tmax .eq. 0.0) tmax = 1.0

      do i=1, t_last
!$omp atomic
         tsum(i) = tsum(i) + trecs(i)
!$omp atomic
         tming(i) = min(tming(i), trecs(i))
!$omp atomic
         tmaxg(i) = max(tmaxg(i), trecs(i))
      end do
!$omp barrier

!$omp master
!$    write(*, 700) num_othreads
!$    do i = 1, t_last
!$       tsum(i) = tsum(i) / num_othreads
!$       write(*, 710) i, t_names(i), tming(i), tmaxg(i), tsum(i)
!$    end do
!$omp end master
 700  format(' #othrs =', i6, 11x, 'minimum', 5x, 'maximum',  &
     &       5x, 'average')
 710  format(' timer ', i2, '(', A8, ') :', 3(2x,f10.4))

!$    if (itimer .lt. 2) goto 999

!$omp barrier
!$omp critical (ptime)
!$    write(*,800) myid, nthreads
 800  format(/' myid =',i5,'   num_ithreads =',i4)
      write(*,805)
 805  format('  SECTION   Time (secs)')
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
!$omp end critical (ptime)

 999  continue

!$omp end parallel

      call free_space

      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine init_workarray(nx, nxmax, ny, a, b, c, d,  &
     &                          au, bu, cu, du, tv)
      implicit none

!---------------------------------------------------------------------
!   initialize a,b,c,d to zero (guarantees that page tables have been
!   formed, if applicable on given architecture, before timestepping).
!   extra working arrays au, bu, cu, du are used in the OpenMP version
!   to align/touch data pages properly in the upper triangular solver.
!---------------------------------------------------------------------

      integer nx, nxmax, ny
      double precision a (25,2:nxmax-1,ny), b (25,2:nxmax-1,ny),  &
     &                 c (25,2:nxmax-1,ny), d (25,2:nxmax-1,ny),  &
     &                 au(25,2:nxmax-1,ny), bu(25,2:nxmax-1,ny),  &
     &                 cu(25,2:nxmax-1,ny), du(25,2:nxmax-1,ny),  &
     &                 tv( 5,2:nxmax-1,ny)

      integer i, j, m

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(m,i,j)
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
!$OMP END PARALLEL

      return
      end
