!***********************************************************************
!                        Version 1:  02/06, PFN                        *
!                                                                      *
!   setGeometry - This routine sets mesh connectivity in the geometry  *
!                 module.                                              *
!                                                                      *
!***********************************************************************

   subroutine setGeometry(CToZone,CToPoint,CToFace,nfpc,BdyToC,ZoneToSrc,px)

!  Include

   use kind_mod
   use Size_mod
   use Geometry_mod
   use BoundaryList_mod
   use Boundary_mod
   use ZoneData_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: BdyToC(Size%nbelem),              & 
                                CToZone(Size%ncornr),             &
                                CToPoint(Size%ncornr),            &
                                CToFace(Size%maxcf,Size%ncornr),  &
                                ZoneToSrc(Size%nzones),           &
                                nfpc(Size%ncornr)

   real(adqt), target, intent(in) :: px(Size%ndim,Size%npnts)

!  Local 

   integer :: i, ib, nBoundary, nBdyElem, b0

!  Constants 

   nBoundary = getNumberOfBoundaries(RadBoundary)

!  Set the corner-based connectivity

   Geom% CToFace(:,:)   = CToFace(:,:)
   Geom% CToZone(:)     = CToZone(:)
   Geom% CToPoint(:)    = CToPoint(:)
   Geom% nfpc(:)        = nfpc(:)

   Geom% ZoneToSrc(1:Size%nzones) = ZoneToSrc(:)

   Geom%px => px

!  Create local boundary-element to global corner ID

   do i=1,nBoundary
     Bdy      => getBoundary(RadBoundary, i)
     nBdyElem =  getNumberOfBdyElements(Bdy)
     b0       =  getFirstBdyElement(Bdy) - 1

     do ib=1,nBdyElem
       Bdy% BdyToC(ib) = BdyToC(b0+ib)
     enddo

   enddo



   return
   end subroutine setGeometry 


