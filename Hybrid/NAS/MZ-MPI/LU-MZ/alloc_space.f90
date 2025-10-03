!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  Allocate space for field arrays
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_field_space( ixmax, iymax )

      use lu_data, only : problem_size, tot_zonesize, tot_zonesize5,  &
     &                    tot_bcsize_in, tot_bcsize_ou
      use lu_fields

      implicit none

      integer ixmax, iymax

      integer ios

      allocate( a (25*ixmax*iymax),  &
     &          b (25*ixmax*iymax),  &
     &          c (25*ixmax*iymax),  &
     &          d (25*ixmax*iymax),  &
     &          au(25*ixmax*iymax),  &
     &          bu(25*ixmax*iymax),  &
     &          cu(25*ixmax*iymax),  &
     &          du(25*ixmax*iymax),  &
     &          phi1((problem_size+1)*problem_size),  &
     &          phi2((problem_size+1)*problem_size), stat=ios )

      if (ios .eq. 0) allocate(  &
     &         u     (tot_zonesize5),  &
     &         rsd   (tot_zonesize5),  &
     &         frct  (tot_zonesize5),  &
     &         qs    (tot_zonesize),  &
     &         rho_i (tot_zonesize),  &
     &         qbc_ou(tot_bcsize_ou),  &
     &         qbc_in(tot_bcsize_in), stat=ios )

      if (ios .ne. 0) call error_cond( 1, 'fields' )

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  Allocate process-based working arrays
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_proc_space

      use mpinpb
      implicit none

      integer ios

      allocate(  &
     &   proc_zone_count (num_zprocs),  &
     &   proc_num_threads(num_zprocs),  &
     &   proc_group      (num_zprocs),  &
     &   proc_zone_size  (num_zprocs),  &
     &   pcomm_group     (num_procs2),  &
     &   qcomm_size      (num_procs), stat=ios )

      if (ios .ne. 0) call error_cond( 1, 'proc arrays' )

      return
      end

!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  Free allocated space
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine free_space

      use lu_fields
      use mpinpb

      implicit none

      integer ios

      deallocate(  &
     &   a, b, c, d, au, bu, cu, du, phi1, phi2,  &
     &   u, rsd, frct, qs, rho_i, qbc_ou, qbc_in,  &
     &   stat=ios )

      deallocate(  &
     &   proc_zone_count, proc_num_threads, proc_group,  &
     &   proc_zone_size, pcomm_group, qcomm_size,  &
     &   stat=ios )

      return
      end

