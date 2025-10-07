!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

!     use mpi
      include 'mpif.h'

      ! for main codes
      integer  node, no_nodes, total_nodes, root
      logical  active
      integer  comm_setup, comm_solve, comm_rhs
      integer  dp_type

      ! for copy_faces
      integer  requests(0:11)
      integer  statuses(MPI_STATUS_SIZE,0:11)

      ! for solvers
      integer  recv_id, send_id
      integer  r_status(MPI_STATUS_SIZE)

      ! for btio
      integer  mstatus(MPI_STATUS_SIZE)
      integer, allocatable :: cell_btype(:), cell_ftype(:)
      integer  info
      integer  combined_btype, combined_ftype, element
      integer  fp

      end module mpinpb

