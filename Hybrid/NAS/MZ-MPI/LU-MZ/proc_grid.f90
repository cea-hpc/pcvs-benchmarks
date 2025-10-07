
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine proc_grid

!---------------------------------------------------------------------
!---------------------------------------------------------------------

      use lu_data
      use mpinpb

      implicit none

!---------------------------------------------------------------------
!   local variables
!---------------------------------------------------------------------
      integer nx1, zdim_best, xdim_best, cz_size, cx_size
      integer ios
      double precision t_size, spup, spup_best
      character(len=80) envstr

!---------------------------------------------------------------------
!   NPB_NUM_XPROCS sets the number of processes for
!   the I dimension partition in the process grid
!---------------------------------------------------------------------
      if (myid .eq. root) then
         call get_menv('NPB_NUM_XPROCS', envstr, ios)
         xdim = 0
         if (ios .gt. 0) then
            read(envstr,*,iostat=ios) xdim
            if (ios .eq. 0) xdim = min(xdim, num_procs)
            if (xdim.gt.0 .and. xdim*max_zones.lt.num_procs)  &
     &         xdim = 0
         endif
      endif
      call mpi_bcast(xdim, 1, mpi_integer, root,  &
     &               comm_setup, ierror)

      if (xdim .gt. 0) then
         zdim = num_procs / xdim
         if (xdim*zdim .ne. num_procs .or. zdim .gt. max_zones) then
            if (myid .eq. root) then
               write(*,2000) xdim
               if (zdim .gt. max_zones) then
                  write(*,2010) zdim, max_zones
               else
                  write(*,2020) num_procs, xdim
               endif
            endif
            call error_cond(0, ' ')
         endif
         goto 40
      endif
 2000 format(' Error: cannot determine a proper process grid for'  &
     &       ' NPB_NUM_XPROCS = ',i6)
 2010 format(' zdim ',i6,' exceeds the number of zones ',i6)
 2020 format(' number of processes ',i6,' is not divisible by ',i6)

      nx1 = gx_size / x_zones
      t_size = max_zones * nx1

!---------------------------------------------------------------------
!   set up a two-d grid for processors: column-major ordering of unknowns
!   pick up a processor count that can balance the zone load and
!   maximize the speedup
!---------------------------------------------------------------------
      zdim   = min(max_zones, num_procs)
      spup_best = 0.0d0
      zdim_best = num_procs
      xdim_best = 1

      do while (zdim .ge. 1)
         xdim   = num_procs / zdim
         do while (xdim*zdim .ne. num_procs)
            zdim = zdim - 1
            xdim = num_procs / zdim
         end do
         cz_size = (max_zones + zdim - 1)/zdim
         cx_size = (nx1 + xdim - 1)/xdim
         spup = t_size / cz_size / cx_size
         if (spup .gt. spup_best) then
            zdim_best = zdim
            xdim_best = xdim
            spup_best = spup
         endif
         zdim = zdim - 1
      end do

!---------------------------------------------------------------------
!   zdim is the number of processes for zones
!   xdim is the number of processes for the x dimension
!---------------------------------------------------------------------
      zdim = zdim_best
      xdim = xdim_best

   40 if (myid .eq. root .and. xdim .gt. 1) then
         write(*,2100) xdim, zdim
      endif
 2100 format(' 2D process grid (NXP x NZP) =',i6,' x',i5/)

      row = mod(myid,xdim)
      col = myid/xdim

      call mpi_comm_split(comm_setup,col,myid,comm_ipart,ierror)
      call mpi_comm_split(comm_setup,row,myid,comm_zpart,ierror)

!---------------------------------------------------------------------
!   for coding convenience, "num_zprocs" is the number of procs
!     for zone distribution 
!---------------------------------------------------------------------
      num_zprocs = zdim

      return
      end


