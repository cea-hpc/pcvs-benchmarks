!***********************************************************************
!                        Version 1:  12/94, PFN                        *
!                                                                      *
!   SETBDY - Sets boundary fluxes PSIB to specified incident boundary  *
!            fluxes.                                                   *
!                                                                      *
!   Output:  PSIB  - boundary intensities                      (E/A/t) *
!                                                                      *
!***********************************************************************

   subroutine setbdy(psir, PSIB) 

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use ProfileList_mod
   use BoundaryList_mod
   use Boundary_mod

   implicit none

!  Arguments

   real(adqt), intent(in)    :: psir(Size%ngr,Size%ncornr,Size%nangSN) 

   real(adqt), intent(inout) :: psib(Size%ngr,Size%nbelem,Size%nangSN)

!  Local

   integer :: i,ia,ib,ic,nangSN
   integer :: nReflecting, nVacuum, nSource, nShared 
   integer :: nBdyElem, b0, b1, b2, profID

!  Constants

   nangSN      = Size%nangSN
   nSource     = getNumberOfSource(RadBoundary)
   nReflecting = getNumberOfReflecting(RadBoundary)
   nVacuum     = getNumberOfVacuum(RadBoundary)
   nShared     = getNumberOfShared(RadBoundary)

!  Reflecting

   do i=1,nReflecting
     Bdy      => getReflecting(RadBoundary, i)
     nBdyElem =  getNumberOfBdyElements(Bdy)
     b0       =  getFirstBdyElement(Bdy) - 1
 
     do ib=1,nBdyElem
       ic = Bdy% BdyToC(ib)
       do ia=1,nangSN
         psib(:,b0+ib,ia) = psir(:,ic,ia)
       enddo
     enddo
   enddo

!  Vacuum

   do i=1,nVacuum
     Bdy      => getVacuum(RadBoundary, i)
     nBdyElem =  getNumberOfBdyElements(Bdy)
     b1       =  getFirstBdyElement(Bdy)
     b2       =  b1 + nBdyElem - 1
 
     do ib=b1,b2
       do ia=1,nangSN
          psib(:,ib,ia) = zero
       enddo
     enddo
   enddo

!  Sources
                                                                                                    
   do i=1,nSource
     Bdy => getSource(RadBoundary, i)
     nBdyElem = getNumberOfBdyElements(Bdy)
     b1       = getFirstBdyElement(Bdy)
     b2       = b1 + nBdyElem - 1
     profID   = getProfileID(Bdy)

     do ia=1,nangSN
       do ib=b1,b2
         psib(:,ib,ia) = SourceProfiles% Psi_Inc(:,ia,profID)
       enddo
     enddo
   enddo

!  Shared
                                                                                                    
   do i=1,nShared
     Bdy      => getShared(RadBoundary, i)
     nBdyElem =  getNumberOfBdyElements(Bdy)
     b0       =  getFirstBdyElement(Bdy) - 1

     do ib=1,nBdyElem
       ic = Bdy% BdyToC(ib)
       do ia=1,nangSN
         psib(:,b0+ib,ia) = psir(:,ic,ia)
       enddo
     enddo
   enddo



   return
   end subroutine setbdy

