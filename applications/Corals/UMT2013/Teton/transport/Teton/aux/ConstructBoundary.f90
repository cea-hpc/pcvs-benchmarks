# 1 "aux/ConstructBoundary.F90"
# 1 "<interne>"
# 1 "<ligne-de-commande>"
# 1 "aux/ConstructBoundary.F90"
!***********************************************************************
!                        Version 0:  03/08, PFN                        *
!                                                                      *
!   ConstructBoundary - Constructor for external and shared            * 
!                       boundaries (called from C++).                  * 
!                                                                      *
!***********************************************************************


   subroutine ConstructBoundary(NumReflecting, NumVacuum,   &
                                NumSource, NumShared)

!  Include

   use kind_mod
   use BoundaryList_mod


   implicit none

!  Arguments

   integer, intent(in)          :: NumReflecting
   integer, intent(in)          :: NumVacuum
   integer, intent(in)          :: NumSource
   integer, intent(in)          :: NumShared

!  Construct the Boundary Module 

   allocate (RadBoundary)
   call construct(RadBoundary,  NumReflecting, &
                                NumVacuum,     &
                                NumSource,     &
                                NumShared) 


   return
   end subroutine ConstructBoundary

