!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  ompnpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module ompnpb
!
      use sp_data, only : max_zones
!
      integer   zone_proc_id(max_zones),  &     ! othread_id for each zone
     &          proc_zone_count(max_zones),  &  ! #zones assigned to othread
     &          proc_num_threads(max_zones),  & ! #ithreads for each othread
     &          proc_group(max_zones)           ! group_id for each othread
      double precision proc_zone_size(max_zones)
!
      integer          myid, root, num_othreads, num_threads,  &
     &                 mz_bload, max_threads, nested
!$omp threadprivate(myid, root)

      end module ompnpb

