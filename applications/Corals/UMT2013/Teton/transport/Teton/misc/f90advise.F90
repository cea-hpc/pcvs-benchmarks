subroutine f90advise(message)

!=======================================================================
!                       Version 1.0: 12/98, MRZ
!-----------------------------------------------------------------------
! f90advise
!   This is the Fortran90 error handler for advise (warning) messages.
! Currently, it issues an error message and exits.  When TETON runs as
! a physics package, this routine should issue the error to the host
! code's error handler.
!
!-----------------------------------------------------------------------
! v1.0: Original implementation
!=======================================================================

use io_mod

!  variable declarations
   implicit none

!  passed variables
   character(*) :: message

!-----------------------------------------------------------------------

!  issue the advise message and continue execution

   write(nout,500) message

!-----------------------------------------------------------------------
!  format statements
!-----------------------------------------------------------------------
500 format(/1x,"The following advise message has occurred:"/3x,a)
!-----------------------------------------------------------------------

   return
end subroutine f90advise
