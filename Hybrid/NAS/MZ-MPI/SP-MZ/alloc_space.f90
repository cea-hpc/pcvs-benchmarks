!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  Allocate space for field arrays
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine alloc_field_space

      use sp_data, only : tot_zonesize, tot_zonesize5,  &
     &                    tot_bcsize_in, tot_bcsize_ou
      use sp_fields

      implicit none

      integer ios

      allocate (  &
     &   u       (tot_zonesize5),  &
     &   us      (tot_zonesize),  &
     &   vs      (tot_zonesize),  &
     &   ws      (tot_zonesize),  &
     &   qs      (tot_zonesize),  &
     &   rho_i   (tot_zonesize),  &
     &   speed   (tot_zonesize),  &
     &   square  (tot_zonesize),  &
     &   rhs     (tot_zonesize5),  &
     &   forcing (tot_zonesize5),  &
     &   qbc_ou  (tot_bcsize_ou),  &
     &   qbc_in  (tot_bcsize_in), stat=ios )

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
     &   proc_zone_count (num_procs ),  &
     &   proc_num_threads(num_procs ),  &
     &   proc_group      (num_procs ),  &
     &   proc_zone_size  (num_procs ),  &
     &   pcomm_group     (num_procs2),  &
     &   qcomm_size      (num_procs ), stat=ios )

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

      use sp_fields
      use mpinpb

      implicit none

      integer ios

      deallocate(  &
     &   u, us, vs, ws, qs, rho_i,  &
     &   speed, square, rhs, forcing, qbc_ou, qbc_in,  &
     &   stat=ios )

      deallocate(  &
     &   proc_zone_count, proc_num_threads, proc_group,  &
     &   proc_zone_size, pcomm_group, qcomm_size,  &
     &   stat=ios )

      return
      end
