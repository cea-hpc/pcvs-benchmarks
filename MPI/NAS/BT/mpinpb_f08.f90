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
      integer :: node, no_nodes, total_nodes, root
      logical :: active
      type(MPI_Comm) :: comm_setup, comm_solve, comm_rhs
      type(MPI_Datatype) :: dp_type

      ! for copy_faces
      type(MPI_Request) :: requests(0:11)
      type(MPI_Status) :: statuses(0:11)

      ! for solvers
      type(MPI_Request) :: recv_id, send_id
      type(MPI_Status) :: r_status

      ! for btio
      type(MPI_Status) :: mstatus
      type(MPI_Datatype), allocatable :: cell_btype(:), cell_ftype(:)
      type(MPI_Info) :: info
      type(MPI_Datatype) :: combined_btype, combined_ftype, element
      type(MPI_File) :: fp

      end module mpinpb

