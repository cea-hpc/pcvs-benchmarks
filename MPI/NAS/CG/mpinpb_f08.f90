!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

      use mpi_f08

      integer  me, nprocs, total_nodes, root
      logical  active
      type(MPI_Comm) :: comm_solve
      type(MPI_Datatype) :: dp_type

      type(MPI_Status) :: status
      type(MPI_Request) :: request

      end module mpinpb

