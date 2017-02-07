!     $Id: iot_err.f,v 1.1 1997/09/25 09:55:30 djl Exp $
!
!     IOT: MPI IO Test Suite
!
!     Copyright Fujitsu Ltd. 1997
!
!     Function:
!     Error handling functions
!     Release 1.0
!
!     Description:
!     Functions to handle errors
!
!     Interface:
!     use iot_err
!
!     External components:
!     iot_defs, iot_string
!-------------------------------------------------------------------

      module iot_err

      contains

!-----------------------------------------------------------------------
!     prtMPIerr
!
!     Print errors from MPI routines
!
!-----------------------------------------------------------------------

      subroutine prtMPIerr(errnum)
      use iot_defs
      use iot_string
      implicit none

      integer :: ierr, class, cnt, errnum
      character(len=MPI_MAX_ERROR_STRING) :: str
      call mpi_error_string(errnum, str, cnt, ierr)
      write (fout,"(""MPI Error "", i3, "" : "", 60a)") errnum, str(1:cnt)
      call mpi_error_class(errnum, class, ierr)
      call mpi_error_string(class, str, cnt, ierr)
      write (fout,"(""MPI Class "", i3, "" : "", 60a)") class, str(1:cnt)

      end subroutine prtMPIerr


!-----------------------------------------------------------------------
!     iotMPIerr
!
!     Handle errors from MPI routines
!
!-----------------------------------------------------------------------

      subroutine iotMPIerr(node, ierr, errmessage)
      use iot_defs
      use iot_string
      implicit none

      integer :: ierr
      integer :: node
      character(len=*) :: errmessage

      call prtMPIerr(ierr)
      write (fout,*) errmessage, "node (MPI) ",node
      ierr = IOT_ERR_MPI

      end subroutine iotMPIerr

!-----------------------------------------------------------------------

      end module iot_err



