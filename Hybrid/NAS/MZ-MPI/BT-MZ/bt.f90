!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.4         !
!                                                                         !
!             M P I    M U L T I - Z O N E    V E R S I O N               !
!                                                                         !
!                           B T - M Z - M P I                             !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is an MPI+OpenMP version of the NPB BT code.          !
!    Refer to NAS Technical Reports 95-020 and 99-011 for details.        !
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
! Authors: R. Van der Wijngaart
!          T. Harris
!          M. Yarrow
!          H. Jin
!
!---------------------------------------------------------------------

!---------------------------------------------------------------------
       program BT_MZ
!---------------------------------------------------------------------

       use bt_data
       use bt_fields
       use mpinpb

       implicit none
      
!---------------------------------------------------------------------
!      local variables
!---------------------------------------------------------------------
       integer          num_zones
       integer          i, niter, step, fstatus, zone,  &
     &                  iz, ip, tot_threads, itimer
       double precision navg, mflops, nsur, n3

       external         timer_read
       double precision tmax, timer_read, t, trecs(t_last),  &
     &                  tsum(t_last), tming(t_last), tmaxg(t_last)
       logical          verified
       character        t_names(t_last)*8


       call setup_mpi
       if (.not. active) goto 999

!---------------------------------------------------------------------
!      Root node reads input file (if it exists) else takes
!      defaults from parameters
!---------------------------------------------------------------------
       if (myid .eq. root) then

         write(*, 1000)

         call check_timer_flag( itimer )

         open (unit=2,file='inputbt-mz.data',status='old',  &
     &         iostat=fstatus)

         if (fstatus .eq. 0) then
           write(*,*) 'Reading from input file inputbt-mz.data'
           read (2,*) niter
           read (2,*) dt
           read (2,*,err=20,end=20) itimer
   20      close(2)

           if (niter .eq. 0)  niter = niter_default
           if (dt .eq. 0.d0)  dt    = dt_default

         else
           niter = niter_default
           dt    = dt_default
         endif

         write(*, 1001) x_zones, y_zones
         write(*, 1002) gx_size, gy_size, gz_size
         write(*, 1003) niter, dt
         write(*, 1004) num_procs
       endif
 1000  format(//, ' NAS Parallel Benchmarks (NPB3.4-MZ MPI+OpenMP)',  &
     &            ' - BT-MZ Benchmark', /)
 1001  format(' Number of zones: ', i3, ' x ', i3)
 1002  format(' Total mesh size: ', i5, ' x ', i5, ' x ', i3)
 1003  format(' Iterations: ', i3, '    dt: ', F10.6)
 1004  format(' Number of active processes: ', i6/)

       call mpi_bcast(niter, 1, MPI_INTEGER,  &
     &                root, comm_setup, ierror)
       call mpi_bcast(dt, 1, dp_type,  &
     &                root, comm_setup, ierror)
       call mpi_bcast(itimer, 1, MPI_INTEGER,  &
     &                root, comm_setup, ierror)
       timeron = (itimer .gt. 0)

       if (timeron) then
         t_names(t_total) = 'total'
         t_names(t_rhsx) = 'rhsx'
         t_names(t_rhsy) = 'rhsy'
         t_names(t_rhsz) = 'rhsz'
         t_names(t_rhs) = 'rhs'
         t_names(t_xsolve) = 'xsolve'
         t_names(t_ysolve) = 'ysolve'
         t_names(t_zsolve) = 'zsolve'
         t_names(t_rdis1) = 'qbc_copy'
         t_names(t_rdis2) = 'qbc_comm'
         t_names(t_add) = 'add'
       endif

!---------------------------------------------------------------------
!      allocate space for working arrays
!---------------------------------------------------------------------
       call alloc_proc_space

       call env_setup(tot_threads)

       call zone_setup(nx, nxmax, ny, nz)

       num_zones = max_zones
       call map_zones(num_zones, nx, ny, nz, tot_threads)
       call zone_starts(num_zones, nx, nxmax, ny, nz)

!---------------------------------------------------------------------
!      allocate space for field arrays
!---------------------------------------------------------------------
       call alloc_field_space

       call set_constants

       do iz = 1, proc_num_zones
         zone = proc_zone_id(iz)

         call initialize(u(start5(iz)),  &
     &                   nx(zone), nxmax(zone), ny(zone), nz(zone))
         call exact_rhs(forcing(start5(iz)),  &
     &                  nx(zone), nxmax(zone), ny(zone), nz(zone))

       end do

       do i = 1, t_last
          call timer_clear(i)
       end do

!---------------------------------------------------------------------
!      do one time step to touch all code, and reinitialize
!---------------------------------------------------------------------

       call exch_qbc(u, qbc_ou, qbc_in, nx, nxmax, ny, nz,  &
     &               npb_verbose)

       do iz = 1, proc_num_zones
         zone = proc_zone_id(iz)
         call adi(rho_i(start1(iz)), us(start1(iz)),  &
     &            vs(start1(iz)), ws(start1(iz)),  &
     &            qs(start1(iz)), square(start1(iz)),  &
     &            rhs(start5(iz)), forcing(start5(iz)),  &
     &            u(start5(iz)),  &
     &            nx(zone), nxmax(zone), ny(zone), nz(zone))
       end do

       do iz = 1, proc_num_zones
         zone = proc_zone_id(iz)
         call initialize(u(start5(iz)),  &
     &                   nx(zone), nxmax(zone), ny(zone), nz(zone))
       end do

       do i = 1, t_last
          call timer_clear(i)
       end do
       call mpi_barrier(comm_setup, ierror)

       call timer_start(1)

!---------------------------------------------------------------------
!      start the benchmark time step loop
!---------------------------------------------------------------------

       do  step = 1, niter

         if (mod(step, 20) .eq. 0 .or. step .eq. 1) then
            if (myid .eq. root) write(*, 200) step
 200        format(' Time step ', i4)
         endif

         call exch_qbc(u, qbc_ou, qbc_in, nx, nxmax, ny, nz,  &
     &                 0)

         do iz = 1, proc_num_zones
           zone = proc_zone_id(iz)
           call adi(rho_i(start1(iz)), us(start1(iz)),  &
     &              vs(start1(iz)), ws(start1(iz)),  &
     &              qs(start1(iz)), square(start1(iz)),  &
     &              rhs(start5(iz)), forcing(start5(iz)),  &
     &              u(start5(iz)),  &
     &              nx(zone), nxmax(zone), ny(zone), nz(zone))
         end do

       end do

       call timer_stop(1)
       tmax = timer_read(1)

!---------------------------------------------------------------------
!      perform verification and print results
!---------------------------------------------------------------------
       
       call verify(niter, verified, num_zones, rho_i, us, vs, ws,  &
     &             qs, square, rhs, forcing, u, nx, nxmax, ny, nz)

       call free_space

       t = tmax
       call mpi_reduce(t, tmax, 1, dp_type, MPI_MAX,  &
     &                 root, comm_setup, ierror)

       if (myid .ne. root) goto 900

       mflops = 0.0d0
       if( tmax .ne. 0. ) then
         do zone = 1, num_zones
           n3 = dble(nx(zone))*ny(zone)*nz(zone)
           navg = (nx(zone) + ny(zone) + nz(zone))/3.0
           nsur = (nx(zone)*ny(zone) + nx(zone)*nz(zone) +  &
     &             ny(zone)*nz(zone))/3.0
           mflops = mflops + 1.0d-6*float(niter) *  &
     &      (3478.8d0 * n3 - 17655.7d0 * nsur + 28023.7d0 * navg)  &
     &      / tmax
         end do
       endif

       call print_results('BT-MZ', class, gx_size, gy_size, gz_size,  &
     &                    niter, tmax, mflops, num_procs, tot_threads,  &
     &                    '          floating point',  &
     &                    verified, npbversion,compiletime, cs1, cs2,  &
     &                    cs3, cs4, cs5, cs6, '(none)')

!---------------------------------------------------------------------
!      More timers
!---------------------------------------------------------------------
 900   if (.not.timeron) goto 999

       do i=1, t_last
          trecs(i) = timer_read(i)
       end do

       call MPI_Reduce(trecs, tsum,  t_last, dp_type, MPI_SUM,  &
     &                 0, comm_setup, ierror)
       call MPI_Reduce(trecs, tming, t_last, dp_type, MPI_MIN,  &
     &                 0, comm_setup, ierror)
       call MPI_Reduce(trecs, tmaxg, t_last, dp_type, MPI_MAX,  &
     &                 0, comm_setup, ierror)

       if (myid .eq. 0) then
          write(*, 700) num_procs
          do i = 1, t_last
             tsum(i) = tsum(i) / num_procs
             write(*, 710) i, t_names(i), tming(i), tmaxg(i), tsum(i)
          end do
       endif
 700   format(' nprocs =', i6, 11x, 'minimum', 5x, 'maximum',  &
     &        5x, 'average')
 710   format(' timer ', i2, '(', A8, ') :', 3(2x,f10.4))

       if (itimer .lt. 2) goto 999

       if (myid .gt. 0) then
          call mpi_recv(i, 1, MPI_INTEGER, 0, 1000,  &
     &                  comm_setup, MPI_STATUS_IGNORE, ierror)
          call mpi_send(trecs, t_last, dp_type, 0, 1001,  &
     &                  comm_setup, ierror)
          goto 999
       endif

       ip = 0
       if (tmax .eq. 0.0) tmax = 1.0
 910   write(*,800) ip, proc_num_threads(ip+1)
 800   format(/' Myid =',i6,'   num_threads =',i4/  &
     &         '  SECTION   Time (secs)')
       do i=1, t_last
          write(*,810) t_names(i), trecs(i), trecs(i)*100./tmax
          if (i.eq.t_rhs) then
             t = trecs(t_rhsx) + trecs(t_rhsy) + trecs(t_rhsz)
             write(*,820) 'sub-rhs', t, t*100./tmax
             t = trecs(t_rhs) - t
             write(*,820) 'rest-rhs', t, t*100./tmax
          elseif (i.eq.t_rdis2) then
             t = trecs(t_rdis1) + trecs(t_rdis2)
             write(*,820) 'exch_qbc', t, t*100./tmax
          endif
 810      format(2x,a8,':',f9.3,'  (',f6.2,'%)')
 820      format('    --> total ',a8,':',f9.3,'  (',f6.2,'%)')
       end do

       ip = ip + 1
       if (ip .lt. num_procs) then
          call mpi_send(myid, 1, MPI_INTEGER, ip, 1000,  &
     &                  comm_setup, ierror)
          call mpi_recv(trecs, t_last, dp_type, ip, 1001,  &
     &                  comm_setup, MPI_STATUS_IGNORE, ierror)
          goto 910
       endif

 999   continue
       call mpi_barrier(MPI_COMM_WORLD, ierror)
       call mpi_finalize(ierror)

       end

