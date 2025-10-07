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

      integer me, nprocs, nprocs_total, root
      logical active
      integer dp_type
      integer comm_work
      integer msg_id(3,-1:1,2)
      integer status(MPI_STATUS_SIZE)

      end module mpinpb

