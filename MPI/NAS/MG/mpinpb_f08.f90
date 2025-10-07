!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

      use mpi_f08

      integer me, nprocs, nprocs_total, root
      logical active
      type(MPI_Datatype) :: dp_type
      type(MPI_Comm) :: comm_work
      type(MPI_Request) :: msg_id(3,-1:1,2)
      type(MPI_Status) :: status

      end module mpinpb

