!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

      use mpi_f08

      ! for main codes
      integer  node, no_nodes, total_nodes, root
      logical  active
      type(MPI_Comm) :: comm_setup, comm_solve, comm_rhs
      type(MPI_Datatype) :: dp_type

      ! for copy_faces
      type(MPI_Status) :: statuses(0:11)
      type(MPI_Request) :: requests(0:11)

      ! for solvers
      type(MPI_Status) :: mstatuses(2)
      type(MPI_Request) :: mrequests(2)

      integer  DEFAULT_TAG
      parameter (DEFAULT_TAG = 0)

      end module mpinpb
