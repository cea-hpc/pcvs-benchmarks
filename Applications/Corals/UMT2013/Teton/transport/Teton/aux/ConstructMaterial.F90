!***********************************************************************
!                        Version 0:  02/02, MKN                        *
!                                                                      *
!   CINTERFACE  -   Wrapper for modules that can be called from C++    * 
!                   used to get IterControls pointer                   *
!                                                                      *
!***********************************************************************


   subroutine ConstructMaterial

!  Include

   use kind_mod
   use Material_mod


   implicit none

!  Construct Material Module 

   allocate (Mat)

   call construct(Mat)


   return
   end subroutine ConstructMaterial

