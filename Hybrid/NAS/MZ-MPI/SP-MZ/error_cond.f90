!---------------------------------------------------------------------
!---------------------------------------------------------------------
!
!  Print error message and exit program
!
!---------------------------------------------------------------------
!---------------------------------------------------------------------

      subroutine error_cond( icode, msg )

      use mpinpb
      implicit none

      integer icode
      character(len=*) msg

      integer ierr

      if (icode .eq. 1) then
         write (*,*) 'Error in allocating space for ', msg
      else if (icode .eq. 2) then
         write (*,*) 'Erroneous data designation: ', msg
      endif

      call mpi_abort(MPI_COMM_WORLD, 1, ierr)
      stop

      end
