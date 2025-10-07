!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  mpinpb module
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      module mpinpb

      use mpi_f08

!--------------------------------------------------------------------
! 'np' number of processors, 'np_min' min number of processors
!--------------------------------------------------------------------
      integer np_min, np

! we need a bunch of logic to keep track of how
! arrays are laid out. 
! coords of this processor
      integer me, me1, me2

! need a communicator for row/col in processor grid
      type(MPI_Comm) :: comm_solve, commslice1, commslice2
      logical active

! mpi data types
      type(MPI_Datatype) :: dc_type

      end module mpinpb

