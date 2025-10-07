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
      integer  comm_solve
      integer  dp_type

      ! for exchange_*
      integer  msgid1, msgid2, msgid3
      integer  status(MPI_STATUS_SIZE)

      end module mpinpb

