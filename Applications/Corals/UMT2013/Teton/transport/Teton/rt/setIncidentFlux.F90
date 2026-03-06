!***********************************************************************
!                        Version 1:  03/2009, PFN                      *
!                                                                      *
!   setIncidentFlux - Calculates the incident flux on shared           *
!                     boundaries.                                      *
!                                                                      *
!***********************************************************************
   subroutine setIncidentFlux(psib) 

   use kind_mod
   use constant_mod
   use Size_mod
   use QuadratureList_mod
   use Quadrature_mod
   use Communicator_mod
   use BoundaryList_mod
   use Boundary_mod

   implicit none

!  Arguments

   real(adqt), intent(in) :: psib(Size%ngr,Size%nbelem,Size%nangSN) 

!  Local

   integer    :: bin,i,ia,ib,ig
   integer    :: b0,ishared,nShared,Groups,nrecv,nrecv0,Angle,NangBin

   real(adqt) :: dot
   real(adqt) :: sum

!  Constants

   nShared =  getNumberOfShared(RadBoundary)
   Groups  =  QuadSet% Groups

!  Edit

   QuadSet% IncFluxOld(:) = QuadSet% IncFlux(:)
   QuadSet% IncFlux(:)    = zero
   QuadSet% Flux(:,:)     = zero

   SharedBoundary: do ishared=1,nShared
     Bdy => getShared(RadBoundary, ishared)
     b0  =  getFirstBdyElement(Bdy) - 1

     AngleBin: do bin=1,QuadSet% NumBin0
       Comm => getMessage(QuadSet, bin, ishared)
       NangBin = QuadSet% NangBinList(bin)

       sum = zero
       if (Comm% lenrecv > 0) then

         nrecv0 = 0
         do ia=1,NangBin
           Angle = QuadSet% AngleOrder(ia,bin)
           nrecv = Comm% nrecv(ia)

           do i=1,nrecv
             ib = Comm% ListRecv(nrecv0+i)
             dot = DOT_PRODUCT( QuadSet%omega(:,Angle),Bdy%A_bdy(:,ib-b0) )
             do ig=1,Groups
               sum = sum - dot*psib(ig,ib,Angle)
             enddo
           enddo
           nrecv0 = nrecv0 + nrecv
         enddo

       endif

       QuadSet% IncFlux(bin)      = sum + QuadSet% IncFlux(bin) 
       QuadSet% Flux(bin,ishared) = sum 

     enddo AngleBin

   enddo SharedBoundary 


 
   return
   end subroutine setIncidentFlux 

