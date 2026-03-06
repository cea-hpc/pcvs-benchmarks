subroutine f90fatal(message)

!=======================================================================
!                       Version 1.0: 12/98, MRZ
!-----------------------------------------------------------------------
! f90fatal
!   This is the Fortran90 error handler for fatal errors.  Currently, it
! issues an error message and exits.  When TETON runs as a physics
! pacakge, this routine should issue the error to the host code's
! error handler
!
! message   error message printed upon fatal error
!-----------------------------------------------------------------------
! v1.0: Original implementation
!=======================================================================

use io_mod

!  variable declarations
   implicit none

!  passed variables
   character(*) :: message

!-----------------------------------------------------------------------

!  issue the fatal error message and exit

   write(nout,500) message

   call exit(1)

!-----------------------------------------------------------------------
!  format statements
!-----------------------------------------------------------------------
500 format(/1x,"The following fatal error has occurred:"/3x,a)
!-----------------------------------------------------------------------

   return
end subroutine f90fatal
