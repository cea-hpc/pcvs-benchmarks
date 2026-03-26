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

#ifdef ASSERT

   if ( .not. bool ) then

!     if the assertion is not true, then issue an error message...

      select case(line)
      case (1:9)
         write(nout,501) message,routine,line
      case (10:99)
         write(nout,502) message,routine,line
      case (100:999)
         write(nout,503) message,routine,line
      case (1000:9999)
         write(nout,504) message,routine,line
      case default
         write(nout,505) message,routine,line
      end select

!     ...and die a horrible death

      call exit(1)

   endif

#endif

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
