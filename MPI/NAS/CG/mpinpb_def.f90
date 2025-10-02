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

      integer  me, nprocs, total_nodes, root
      logical  active
      integer  comm_solve
      integer  dp_type

      integer  status(MPI_STATUS_SIZE)
      integer  request

      end module mpinpb

