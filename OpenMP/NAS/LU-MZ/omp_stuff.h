c
      integer   zone_proc_id(max_zones),        ! othread_id for each zone
     &          proc_zone_count(max_zones),     ! #zones assigned to othread
     &          proc_num_threads(max_zones),    ! #ithreads for each othread
     &          proc_group(max_zones)           ! group_id for each othread
      double precision proc_zone_size(max_zones)
      common /omp_cmn1b/ proc_zone_size, zone_proc_id, proc_zone_count, 
     &                   proc_num_threads, proc_group
c
      integer          myid, root, num_othreads, num_threads, 
     &                 mz_bload, max_threads, nested
      common /omp_cmn2a/ myid, root
!$omp threadprivate(/omp_cmn2a/)
      common /omp_cmn2b/ num_othreads, num_threads, mz_bload, 
     &                   max_threads, nested

