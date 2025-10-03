!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

!     use mpi
      use lu_data, only : max_zones, kind2
      include 'mpif.h'
!
!     zone_proc_id(MZ)     - process id each zone assigned to
!     proc_zone_id(MZ)     - list of zones assigned to this process
!     proc_num_zones       - number of zones assigned to this process
!     proc_zone_count(NP)  - number of zones assigned to each process
!     proc_num_threads(NP) - number of threads assigned to each process
!     proc_group(NP)       - group id each process assigned to
!
      integer   zone_proc_id(max_zones), proc_zone_id(max_zones),  &
     &          proc_num_zones
      integer, allocatable :: proc_zone_count(:),  &
     &          proc_num_threads(:), proc_group(:)
      double precision, allocatable :: proc_zone_size(:)
!
      integer   myid, root, comm_setup, ierror, dp_type
      integer   num_threads, max_threads, mz_bload, mz_bload_erank
!
!     num_procs  - total number of processes
!     num_procs2 - power-of-two value, no less than num_procs
!     num_zprocs - number of processes assigned to zone dimension
      integer   num_procs, num_procs2, num_zprocs,  &
     &          comm_zpart, comm_ipart
      logical   active
!
! ... Two adjustable parameters for MPI communication
!     max_reqs  -- max. number of async message requests
!     MSG_SIZE  -- optimal message size (in words) for communication
      integer   max_reqs, MSG_SIZE
      parameter (max_reqs=32, MSG_SIZE=400000)
!
      integer   requests(max_reqs), statuses(MPI_STATUS_SIZE,max_reqs)
!
      integer, allocatable :: pcomm_group(:)
      integer(kind2), allocatable :: qcomm_size(:)
      integer(kind2) ::  &
     &          qstart2_west (max_zones), qstart2_east (max_zones),  &
     &          qstart2_south(max_zones), qstart2_north(max_zones)

      end module mpinpb

