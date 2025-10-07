
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine read_input(tot_threads, itimer)

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

      integer tot_threads, itimer

      integer fstatus, ios, iprov
      character(len=10) mt_type

      if (myid .eq. root) then
        write(*, 1000) 

        call check_timer_flag( itimer )

        open (unit=2,file='inputlu-mz.data',status='old',  &
     &        access='sequential',form='formatted', iostat=fstatus)

        if (fstatus .eq. 0) then

           write(*,*) 'Reading from input file inputlu-mz.data'

           read (2,*)
           read (2,*)
           read (2,*) ipr, inorm
           read (2,*)
           read (2,*)
           read (2,*) itmax
           read (2,*)
           read (2,*)
           read (2,*) dt
           read (2,*)
           read (2,*)
           read (2,*) omega
           read (2,*)
           read (2,*)
           read (2,*) tolrsd(1),tolrsd(2),tolrsd(3),tolrsd(4),tolrsd(5)
           read (2,*,err=20,end=20)
           read (2,*,err=20,end=20)
           read (2,*,err=20,end=20) itimer
   20      close(2)

           if (itmax .eq. 0)  itmax = itmax_default
           if (dt .eq. 0.d0)  dt    = dt_default

        else
           ipr   = ipr_default
           inorm = inorm_default
           itmax = itmax_default
           dt    = dt_default
           omega = omega_default
           tolrsd(1) = tolrsd1_def
           tolrsd(2) = tolrsd2_def
           tolrsd(3) = tolrsd3_def
           tolrsd(4) = tolrsd4_def
           tolrsd(5) = tolrsd5_def
        endif

        write(*, 1001) x_zones, y_zones
        write(*, 1002) gx_size, gy_size, gz_size
        write(*, 1003) itmax, dt
        write(*, 1004) num_procs
      endif

 1000 format(//,' NAS Parallel Benchmarks (NPB3.4-MZ MPI+OpenMP)',  &
     &          ' - LU-MZ Benchmark', /)
 1001 format(' Number of zones: ', i3, ' x ',  i3)
 1002 format(' Total mesh size: ', i5, ' x ', i5, ' x ', i3)
 1003 format(' Iterations: ', i3, '    dt: ', F10.6)
 1004 format(' Number of active processes: ', i6/)

      call mpi_bcast(ipr,    1, MPI_INTEGER,  &
     &               root, comm_setup, ierror)
      call mpi_bcast(inorm,  1, MPI_INTEGER,  &
     &               root, comm_setup, ierror)
      call mpi_bcast(itmax,  1, MPI_INTEGER,  &
     &               root, comm_setup, ierror)
      call mpi_bcast(dt,     1, dp_type,  &
     &               root, comm_setup, ierror)
      call mpi_bcast(omega,  1, dp_type,  &
     &               root, comm_setup, ierror)
      call mpi_bcast(tolrsd, 5, dp_type,  &
     &               root, comm_setup, ierror)
      call mpi_bcast(itimer, 1, MPI_INTEGER,  &
     &               root, comm_setup, ierror)
      timeron = (itimer .gt. 0)

!---------------------------------------------------------------------
!   set up processor grid
!---------------------------------------------------------------------
      call proc_grid()

!---------------------------------------------------------------------
!   determine the neighbors
!---------------------------------------------------------------------
      call neighbors()

!---------------------------------------------------------------------
!   prepare for load balancing of zones
!---------------------------------------------------------------------
      call alloc_proc_space

      call env_setup(tot_threads)

      if (myid.eq.root .and. xdim.gt.1 .and. num_threads.gt.1) then
         call mpi_query_thread(iprov, ierror)
         if (iprov .ne. MPI_THREAD_MULTIPLE) then
            if (iprov .eq. MPI_THREAD_SERIALIZED) then
               mt_type = 'SERIALIZED'
            else if (iprov .eq. MPI_THREAD_FUNNELED) then
               mt_type = 'FUNNELED'
            else if (iprov .eq. MPI_THREAD_SINGLE) then
               mt_type = 'SINGLE'
            else
               mt_type = 'UNKNOWN'
            endif
            write(*, 2100) mt_type
         endif
      endif
 2100 format('Warning: The current MPI environment does not support',  &
     &       ' MPI_THREAD_MULTIPLE'/  &
     &       'The provided level is MPI_THREAD_',a/  &
     &       'The benchmark may fail when multiple threads are used')

      return
      end


