!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.3         !
!                                                                         !
!          S E R I A L    M U L T I - Z O N E    V E R S I O N            !
!                                                                         !
!                              B T - M Z                                  !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is a serial version of the NPB BT code.               !
!    Refer to NAS Technical Reports 95-020 and 99-011 for details.        !
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
c Authors: R. Van der Wijngaart
c          T. Harris
c          M. Yarrow
c          H. Jin
c
c---------------------------------------------------------------------

c---------------------------------------------------------------------
       program BT
c---------------------------------------------------------------------

       include  'header.h'
      
       integer   num_zones
       parameter (num_zones=x_zones*y_zones)

       integer   nx(num_zones), nxmax(num_zones), ny(num_zones), 
     $           nz(num_zones)

c---------------------------------------------------------------------
c   Define all field arrays as one-dimenional arrays, to be reshaped
c---------------------------------------------------------------------
       double precision 
     >   u       (proc_max_size5),
     >   us      (proc_max_size ),
     >   vs      (proc_max_size ),
     >   ws      (proc_max_size ),
     >   qs      (proc_max_size ),
     >   rho_i   (proc_max_size ),
     >   square  (proc_max_size ),
     >   rhs     (proc_max_size5),
     >   forcing (proc_max_size5),
     >   qbc     (proc_max_bcsize)

       common /fields/ u, us, vs, ws, qs, rho_i, square, 
     >                 rhs, forcing, qbc

       integer          i, niter, step, fstatus, zone, itimer
       double precision navg, mflops, nsur, n3

       external         timer_read
       double precision tmax, timer_read, t, trecs(t_last)
       logical          verified
       character        t_names(t_last)*8

c---------------------------------------------------------------------
c      Reads input file (if it exists) else takes
c      defaults from parameters
c---------------------------------------------------------------------

       write(*, 1000)
       open (unit=2,file='inputbt-mz.data',status='old', 
     >       iostat=fstatus)

       timeron = .false.
       if (fstatus .eq. 0) then
         write(*,*) 'Reading from input file inputbt-mz.data'
         read (2,*) niter
         read (2,*) dt
         read (2,*) itimer
         close(2)

         if (niter .eq. 0)  niter = niter_default
         if (dt .eq. 0.d0)  dt    = dt_default
         if (itimer .gt. 0) timeron = .true.

       else
         niter = niter_default
         dt    = dt_default
       endif

       write(*, 1001) x_zones, y_zones
       write(*, 1002) niter, dt
 1000  format(//, ' NAS Parallel Benchmarks (NPB3.3-MZ-SER)',
     >            ' - BT Multi-Zone Serial Benchmark', /)
 1001  format(' Number of zones: ', i3, ' x ', i3)
 1002  format(' Iterations: ', i3, '    dt: ', F10.6/)

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

       call env_setup

       call zone_setup(nx, nxmax, ny, nz)

       call set_constants

       do zone = 1, num_zones

         call initialize(u(start5(zone)),
     $                   nx(zone), nxmax(zone), ny(zone), nz(zone))
         call exact_rhs(forcing(start5(zone)),
     $                  nx(zone), nxmax(zone), ny(zone), nz(zone))

       end do

       do i = 1, t_last
          call timer_clear(i)
       end do

c---------------------------------------------------------------------
c      do one time step to touch all code, and reinitialize
c---------------------------------------------------------------------

       call exch_qbc(u, qbc, nx, nxmax, ny, nz)

       do zone = 1, num_zones
         call adi(rho_i(start1(zone)), us(start1(zone)), 
     $            vs(start1(zone)), ws(start1(zone)), 
     $            qs(start1(zone)), square(start1(zone)), 
     $            rhs(start5(zone)), forcing(start5(zone)), 
     $            u(start5(zone)), 
     $            nx(zone), nxmax(zone), ny(zone), nz(zone))
       end do

       do zone = 1, num_zones
         call initialize(u(start5(zone)), 
     $                   nx(zone), nxmax(zone), ny(zone), nz(zone)) 
       end do

       do i = 1, t_last
          call timer_clear(i)
       end do
       call timer_start(1)

c---------------------------------------------------------------------
c      start the benchmark time step loop
c---------------------------------------------------------------------

       do  step = 1, niter

         if (mod(step, 20) .eq. 0 .or. step .eq. 1) then
            write(*, 200) step
 200        format(' Time step ', i4)
         endif

         call exch_qbc(u, qbc, nx, nxmax, ny, nz)

         do zone = 1, num_zones
           call adi(rho_i(start1(zone)), us(start1(zone)), 
     $              vs(start1(zone)), ws(start1(zone)), 
     $              qs(start1(zone)), square(start1(zone)), 
     $              rhs(start5(zone)), forcing(start5(zone)), 
     $              u(start5(zone)), 
     $              nx(zone), nxmax(zone), ny(zone), nz(zone))
         end do

       end do

       call timer_stop(1)
       tmax = timer_read(1)

c---------------------------------------------------------------------
c      perform verification and print results
c---------------------------------------------------------------------

       call verify(niter, verified, num_zones, rho_i, us, vs, ws, 
     $             qs, square, rhs, forcing, u, nx, nxmax, ny, nz)

       mflops = 0.0d0
       if( tmax .ne. 0. ) then
         do zone = 1, num_zones
           n3 = dble(nx(zone))*ny(zone)*nz(zone)
           navg = (nx(zone) + ny(zone) + nz(zone))/3.0
           nsur = (nx(zone)*ny(zone) + nx(zone)*nz(zone) +
     >             ny(zone)*nz(zone))/3.0
           mflops = mflops + 1.0d-6*float(niter) *
     >      (3478.8d0 * n3 - 17655.7d0 * nsur + 28023.7d0 * navg)
     >      / tmax
         end do
       endif

       call print_results('BT-MZ', class, gx_size, gy_size, gz_size, 
     >                    niter, tmax, mflops, 
     >                    '          floating point', 
     >                    verified, npbversion,compiletime, cs1, cs2, 
     >                    cs3, cs4, cs5, cs6, '(none)')

c---------------------------------------------------------------------
c      More timers
c---------------------------------------------------------------------
       if (.not.timeron) goto 999

       do i=1, t_last
          trecs(i) = timer_read(i)
       end do
       if (tmax .eq. 0.0) tmax = 1.0

       write(*,800)
 800   format('  SECTION   Time (secs)')
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

 999   continue

       end

