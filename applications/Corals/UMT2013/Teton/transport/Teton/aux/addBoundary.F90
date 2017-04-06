!***********************************************************************
!                         Version 0: 03/08 PFN                         *
!                                                                      *
!    AddProfile   -  Called from Teton.cc to add a boundary            *
!                    to the boundary list.                             *
!                                                                      *
!***********************************************************************
   subroutine addBoundary(numBCTotal, Type, NumBdyElem, BdyElem1,  & 
                          ProfileID, NeighborID, EditID) 


   use kind_mod
   use BoundaryList_mod
   use Boundary_mod

   implicit none


!  Arguments

   integer, intent(in)          :: numBCTotal
   integer, intent(in)          :: NumBdyElem(numBCTotal)
   integer, intent(in)          :: BdyElem1(numBCTotal)
   integer, intent(in)          :: ProfileID(numBCTotal) 
   integer, intent(in)          :: NeighborID(numBCTotal) 
   integer, intent(in)          :: EditID(numBCTotal)
              
   character(len=8), intent(in) :: Type(numBCTotal)

!  Local

   integer  :: BdyID
   integer  :: nReflecting
              
!  Add this profile to the list 

   do BdyID=1,numBCTotal

     call setBoundary(RadBoundary,       &
                      BdyID,             &
                      Type(BdyID),       &
                      NumBdyElem(BdyID), &
                      BdyElem1(BdyID),   &
                      ProfileID(BdyID),  &
                      NeighborID(BdyID), &
                      EditID(BdyID)  )

   enddo

!  Allocate space for reflecting boundary data

   nReflecting = getNumberOfReflecting(RadBoundary)

   do BdyID=1,nReflecting
     Bdy => getReflecting(RadBoundary,BdyID)

     call constructReflectedAngle(Bdy)
   enddo

   

   return
   end subroutine addBoundary 

