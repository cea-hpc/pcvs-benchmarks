# 1 "misc/assert.F90"
# 1 "<interne>"
# 1 "<ligne-de-commande>"
# 1 "misc/assert.F90"
subroutine assert(bool,routine,line,message)

!=======================================================================
!                       Version 1: 09/98, MRZ
!-----------------------------------------------------------------------
! assert
!   This routine performs assertion checking.
!
! bool      scalar logical (boolean) assertion
! routine   routine in which assertion is written (provided by cpp)
! line      line number at which assertion is written (provided by cpp)
! message   error message printed upon failed assertion
!-----------------------------------------------------------------------

   use io_mod

!  variable declarations
   implicit none

!  passed variables
   logical,      intent(in) :: bool
   character(*), intent(in) :: routine, message
   integer,      intent(in) :: line

!-----------------------------------------------------------------------

# 53 "misc/assert.F90"

!-----------------------------------------------------------------------
! format statements
!-----------------------------------------------------------------------
501 format(/1x,"Assertion failed: '",a,"' in file `",a,"' at line ",i1)
502 format(/1x,"Assertion failed: '",a,"' in file `",a,"' at line ",i2)
503 format(/1x,"Assertion failed: '",a,"' in file `",a,"' at line ",i3)
504 format(/1x,"Assertion failed: '",a,"' in file `",a,"' at line ",i4)
505 format(/1x,"Assertion failed: '",a,"' in file `",a,"' at line ",i8)
!-----------------------------------------------------------------------

!-----------------------------------------------------------------------
   return
end subroutine assert
