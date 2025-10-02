!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

      use mpi
!     include 'mpif.h'

      ! for main codes
      integer  node, no_nodes, total_nodes, root
      logical  active
      integer  comm_setup, comm_solve, comm_rhs
      integer  dp_type

      ! for copy_faces
      integer  statuses(MPI_STATUS_SIZE,0:11)
      integer  requests(0:11)

      ! for solvers
      integer  mstatuses(MPI_STATUS_SIZE,2)
      integer  mrequests(2)

      integer  DEFAULT_TAG
      parameter (DEFAULT_TAG = 0)

      end module mpinpb
