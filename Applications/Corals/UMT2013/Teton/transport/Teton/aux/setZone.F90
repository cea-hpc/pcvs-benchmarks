!***********************************************************************
!                        Version 1:  02/06, PFN                        *
!                                                                      *
!   setZone     - This routine sets geometry information in a zone     *
!                 data structure .                                     *
!                                                                      *
!***********************************************************************

   subroutine setZone(zoneID, corner0, numFaces, numCorner, Connect, nfpc)

!  Include

   use kind_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: zoneID
   integer,    intent(in)    :: corner0
   integer,    intent(in)    :: numFaces
   integer,    intent(in)    :: numCorner(Size%nzones)
   integer,    intent(in)    :: Connect(3,Size%maxcf,Size%maxCorner)
   integer,    intent(in)    :: nfpc(Size%ncornr)

!  Local 

   integer :: nCorner, ncfaces

!  Set the zone data structures

   Z => getZoneData(Geom, zoneID)

   nCorner = numCorner(zoneID)
   ncfaces = nfpc(corner0+1)

   call constructZone( Z, nCorner, ncfaces, corner0, numFaces, Connect )


   return
   end subroutine setZone 


