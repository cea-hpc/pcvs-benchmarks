!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         !
!                                                                         !
!             M P I    M U L T I - Z O N E    V E R S I O N               !
!                                                                         !
!                           L U - M Z - M P I                             !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is an MPI+OpenMP version of the NPB LU code.          !
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
      use mpinpb

      implicit none

      integer num_zones

      logical verified
      double precision mflops, nsur, navg, n3

      integer i, zone, step, iz, ip, tot_threads, itimer, ixmax, iymax
      double precision t, tmax, timer_read, trecs(t_last),  &
     &                 tsum(t_last), tming(t_last), tmaxg(t_last),  &
     &                 rsdnm(5), rsdnm_aux(5), errnm(5), errnm_aux(5),  &
     &                 frc, frc_aux
      external timer_read
      character t_names(t_last)*8


      call setup_mpi
      if (.not. active) goto 999

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
         t_names(t_exch)   = 'qx_exch'
      endif

!---------------------------------------------------------------------
!   set up domain sizes
!---------------------------------------------------------------------
      call zone_setup(nx, nxmax, ny, nz, nx1)

      num_zones = max_zones
      call map_zones(num_zones, nx1, ny, nz, tot_threads)
      call zone_starts(num_zones, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!   allocate space for fields
!---------------------------------------------------------------------
      ixmax = 1
      iymax = 1
      do iz = 1, proc_num_zones
         zone = proc_zone_id(iz)
         ixmax = max(ixmax, nxmax(zone))
         iymax = max(iymax, ny(zone))
      end do

      call alloc_field_space( ixmax, iymax )

!---------------------------------------------------------------------
!   set up coefficients
!---------------------------------------------------------------------
      call setcoeff()

      do i = 1, t_last
         call timer_clear(i)
      end do

      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)

!---------------------------------------------------------------------
!   set the boundary values for dependent variables
!---------------------------------------------------------------------
        call setbv(u(start5(iz)),  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))

!---------------------------------------------------------------------
!   set the initial values for dependent variables
!---------------------------------------------------------------------
        call setiv(u(start5(iz)),  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))

!---------------------------------------------------------------------
!   compute the forcing term based on prescribed exact solution
!---------------------------------------------------------------------
        call erhs(frct(start5(iz)), rsd(start5(iz)),  &
     &            nx(zone), nxmax(zone), ny(zone), nz(zone))

!---------------------------------------------------------------------
!   compute the steady-state residuals
!---------------------------------------------------------------------
        call rhs(u(start5(iz)), rsd(start5(iz)),  &
     &           frct(start5(iz)), qs(start1(iz)),  &
     &           rho_i(start1(iz)),  &
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
     &                    a, b, c, d, au, bu, cu, du)

!---------------------------------------------------------------------
!   perform one SSOR iteration to touch all data pages
!---------------------------------------------------------------------
      call exch_qbc(u, qbc_ou, qbc_in, nx, nxmax, ny, nz,  &
     &              npb_verbose)

      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)
        call ssor(u(start5(iz)), rsd(start5(iz)),  &
     &            frct(start5(iz)), qs(start1(iz)),  &
     &            rho_i(start1(iz)),  &
     &            a, b, c, d, au, bu, cu, du,  &
     &            nx(zone), nxmax(zone), ny(zone), nz(zone))
      end do

!---------------------------------------------------------------------
!   reset the boundary and initial values
!---------------------------------------------------------------------
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)

        call setbv(u(start5(iz)),  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))

        call setiv(u(start5(iz)),  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))

!---------------------------------------------------------------------
!   compute the steady-state residuals
!---------------------------------------------------------------------
        call rhs(u(start5(iz)), rsd(start5(iz)),  &
     &           frct(start5(iz)), qs(start1(iz)),  &
     &           rho_i(start1(iz)),  &
     &           nx(zone), nxmax(zone), ny(zone), nz(zone))

      end do

!---------------------------------------------------------------------
!   begin pseudo-time stepping iterations
!---------------------------------------------------------------------

      do i = 1, t_last
         call timer_clear(i)
      end do
      call mpi_barrier(comm_setup, ierror)

      call timer_start(1)

!---------------------------------------------------------------------
!   the timestep loop
!---------------------------------------------------------------------
      do step = 1, itmax

        if (mod(step,20) .eq. 0 .or. step .eq. 1 .or.  &
     &        step .eq. itmax) then
           if (myid .eq. root) write( *, 200) step
 200       format(' Time step ', i4)
        endif

        call exch_qbc(u, qbc_ou, qbc_in, nx, nxmax, ny, nz, 0)

!---------------------------------------------------------------------
!   perform the SSOR iterations
!---------------------------------------------------------------------

        do iz = 1, proc_num_zones
          zone = proc_zone_id(iz)
          call ssor(u(start5(iz)), rsd(start5(iz)),  &
     &              frct(start5(iz)), qs(start1(iz)),  &
     &              rho_i(start1(iz)),  &
     &              a, b, c, d, au, bu, cu, du,  &
     &              nx(zone), nxmax(zone), ny(zone), nz(zone))
        end do

      end do
 
      do i = 1, 5
         rsdnm(i) = 0.d0
         errnm(i) = 0.d0
      end do
      frc = 0.d0

!---------------------------------------------------------------------
!   compute the max-norms of newton iteration residuals
!---------------------------------------------------------------------
      if (timeron) call timer_start(t_l2norm)
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)
        call l2norm(rsd(start5(iz)), rsdnm_aux, nx1(zone),  &
     &              nx(zone), nxmax(zone), ny(zone), nz(zone))
        do i = 1, 5
          rsdnm(i) = rsdnm(i) + rsdnm_aux(i)
        end do
      end do

      if (timeron) call timer_stop(t_l2norm)

      call timer_stop(1)
      maxtime= timer_read(1)

!---------------------------------------------------------------------
!   compute the solution error and surface integral
!---------------------------------------------------------------------
      do iz = 1, proc_num_zones
        zone = proc_zone_id(iz)
        call error(u(start5(iz)), errnm_aux, nx1(zone),  &
     &             nx(zone), nxmax(zone), ny(zone), nz(zone))
        call pintgr(u(start5(iz)), phi1, phi2, frc_aux,  &
     &              nx(zone), nxmax(zone), ny(zone), nz(zone))
        do i = 1, 5
          errnm(i) = errnm(i) + errnm_aux(i)
        end do
        frc = frc + frc_aux
      end do

      if (row.eq.0) then
        do i = 1, 5
          rsdnm_aux(i) = rsdnm(i)
          errnm_aux(i) = errnm(i)
        end do

        call mpi_reduce(rsdnm_aux, rsdnm, 5, dp_type, MPI_SUM,  &
     &                  root, comm_zpart, ierror)
        call mpi_reduce(errnm_aux, errnm, 5, dp_type, MPI_SUM,  &
     &                  root, comm_zpart, ierror)

        frc_aux = frc
        call mpi_reduce(frc_aux, frc, 1, dp_type, MPI_SUM,  &
     &                  root, comm_zpart, ierror)
      endif

!---------------------------------------------------------------------
!   verification test
!---------------------------------------------------------------------
      if (myid .eq. root) then
        call verify ( rsdnm, errnm, frc, verified )
      endif

      call free_space

      t = maxtime
      call mpi_reduce(t, maxtime, 1, dp_type, MPI_MAX,  &
     &                root, comm_setup, ierror)

      if (myid .ne. root) goto 900

      mflops = 0.d0

      if (maxtime .ne. 0.d0) then
        do zone = 1, num_zones
          n3 = dble(nx1(zone))*ny(zone)*nz(zone)
          navg = (nx1(zone) + ny(zone) + nz(zone))/3.d0
          nsur = (nx1(zone)*ny(zone) + nx1(zone)*nz(zone) +  &
     &            ny(zone)*nz(zone))/3.d0
          mflops = mflops + float(itmax)*1.0d-6 *  &
     &       (1984.77d0 * n3 - 10923.3d0 * nsur  &
     &         + 27770.9d0 * navg - 144010.d0)  &
     &       / maxtime
        end do
      endif

      call print_results('LU-MZ', class, gx_size, gy_size, gz_size,  &
     &  itmax, maxtime, mflops, zdim*xdim, tot_threads*xdim,  &
     &  '          floating point', verified,  &
     &  npbversion, compiletime, cs1, cs2, cs3, cs4, cs5, cs6,  &
     &  '(none)')

!---------------------------------------------------------------------
!      More timers
!---------------------------------------------------------------------
 900  if (.not.timeron) goto 999

      do i=1, t_last
         trecs(i) = timer_read(i)
      end do

      call MPI_Reduce(trecs, tsum,  t_last, dp_type, MPI_SUM,  &
     &                0, comm_setup, ierror)
      call MPI_Reduce(trecs, tming, t_last, dp_type, MPI_MIN,  &
     &                0, comm_setup, ierror)
      call MPI_Reduce(trecs, tmaxg, t_last, dp_type, MPI_MAX,  &
     &                0, comm_setup, ierror)

      if (myid .eq. 0) then
         write(*, 700) num_procs
         do i = 1, t_last
            tsum(i) = tsum(i) / num_procs
            write(*, 710) i, t_names(i), tming(i), tmaxg(i), tsum(i)
         end do
      endif
 700  format(' nprocs =', i6, 11x, 'minimum', 5x, 'maximum',  &
     &       5x, 'average')
 710  format(' timer ', i2, '(', A8, ') :', 3(2x,f10.4))

      if (itimer .lt. 2) goto 999

      if (myid .gt. 0) then
         call mpi_recv(i, 1, MPI_INTEGER, 0, 1000,  &
     &                 comm_setup, MPI_STATUS_IGNORE, ierror)
         call mpi_send(trecs, t_last, dp_type, 0, 1001,  &
     &                 comm_setup, ierror)
         goto 999
      endif

      ip = 0
      tmax = maxtime
      if (tmax .eq. 0.0) tmax = 1.0
 910  write(*,800) ip, proc_num_threads(ip/xdim+1)
 800  format(/' Myid =',i5,'   num_threads =',i4/  &
     &        '  SECTION   Time (secs)')
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

      ip = ip + 1
      if (ip .lt. num_procs) then
         call mpi_send(myid, 1, MPI_INTEGER, ip, 1000,  &
     &                 comm_setup, ierror)
         call mpi_recv(trecs, t_last, dp_type, ip, 1001,  &
     &                 comm_setup, MPI_STATUS_IGNORE, ierror)
         goto 910
      endif

 999  continue
      call mpi_barrier(MPI_COMM_WORLD, ierror)
      call mpi_finalize(ierror)
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine init_workarray(nx, nxmax, ny, a, b, c, d,  &
     &                          au, bu, cu, du)
      implicit none

!---------------------------------------------------------------------
!   initialize a,b,c,d to zero (guarantees that page tables have been
!   formed, if applicable on given architecture, before timestepping).
!   extra working arrays au, bu, cu, du are used in the OpenMP version
!   to align/touch data pages properly in the upper triangular solver.
!---------------------------------------------------------------------

      integer nx, nxmax, ny
      double precision a (25,nxmax,ny), b (25,nxmax,ny),  &
     &                 c (25,nxmax,ny), d (25,nxmax,ny),  &
     &                 au(25,nxmax,ny), bu(25,nxmax,ny),  &
     &                 cu(25,nxmax,ny), du(25,nxmax,ny)

      integer i, j, m

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(i,j,m)
!$OMP DO SCHEDULE(STATIC)
      do j = 2, ny-1
        do i = 1, nx
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
        do i = nx, 1, -1
          do m = 1, 25
            au(m,i,j) = 0.d0
            bu(m,i,j) = 0.d0
            cu(m,i,j) = 0.d0
            du(m,i,j) = 0.d0
          end do
        end do
      end do
!$OMP END DO nowait
!$OMP END PARALLEL

      return
      end
