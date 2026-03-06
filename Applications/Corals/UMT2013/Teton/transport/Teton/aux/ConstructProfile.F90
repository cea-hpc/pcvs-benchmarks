!***********************************************************************
!                        Version 0:  02/02, MKN                        *
!                                                                      *
!   ConstructProfile - Constructor for source profiles called          * 
!                      from C++.                                       * 
!                                                                      *
!***********************************************************************


   subroutine ConstructProfile(maxprof)

!  Include

   use kind_mod
   use ProfileList_mod


   implicit none

!  Arguments

   integer, intent(in)          :: maxprof

!  Construct the Source Profile Module 

   allocate (SourceProfiles)
   call construct(SourceProfiles, maxprof)



   return
   end subroutine ConstructProfile

