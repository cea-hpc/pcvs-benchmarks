!***********************************************************************
!                        Version 0:  02/02, MKN                        *
!                                                                      *
!   CINTERFACE  -   Wrapper for modules that can be called from C++    * 
!                   used to get IterControls pointer                   *
!                                                                      *
!***********************************************************************


   subroutine ConstructEditor

!  Include

   use kind_mod
   use Editor_mod


   implicit none


!  Construct Problem Edits 

   allocate (RadEdit)

   call construct(RadEdit)


   return
   end subroutine ConstructEditor

