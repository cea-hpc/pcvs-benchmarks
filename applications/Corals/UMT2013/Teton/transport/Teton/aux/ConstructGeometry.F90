!***********************************************************************
!                        Version 0:  02/02, MKN                        *
!                                                                      *
!   CINTERFACE  -   Wrapper for modules that can be called from C++    * 
!                   used to get IterControls pointer                   *
!                                                                      *
!***********************************************************************


   subroutine ConstructGeometry

!  Include

   use kind_mod
   use Geometry_mod


   implicit none


!  Construct the Geometry Module 

   allocate (Geom)

   call construct(Geom)


   return
   end subroutine ConstructGeometry

