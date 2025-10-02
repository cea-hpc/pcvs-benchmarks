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
      type(MPI_Comm) :: comm_solve
      type(MPI_Datatype) :: dp_type

      ! for exchange_*
      type(MPI_Request) :: msgid1, msgid2, msgid3
      type(MPI_Status) :: status

      end module mpinpb

