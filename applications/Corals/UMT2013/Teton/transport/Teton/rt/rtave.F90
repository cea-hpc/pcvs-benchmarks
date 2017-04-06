!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTAVE - Calculates the volume-weighted average of a corner         *
!           variable.                                                  *
!                                                                      *
!   Input:  xc   - corner variable                                     *
!                                                                      *
!   Output: AVE  - zone-average value of the corner variable           *
!                                                                      *
!***********************************************************************
 
   subroutine rtave(xc, AVE)

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(in)    :: xc(Size%ncornr)

   real(adqt), intent(inout) :: ave(Size%nzones)

!  Local

   integer    :: c, c0, nCorner, zone 

   real(adqt) :: sumZ 

!  Compute zone-average value from corner values
 
   ZoneLoop: do zone=1,Size%nzones

     Z       => getZoneData(Geom, zone)
     nCorner =  Z% nCorner
     c0      =  Z% c0
     sumZ    =  zero

     CornerLoop: do c=1,nCorner
       sumZ = sumZ + Z% Volume(c)*xc(c0+c)/Z% VolumeZone
     enddo CornerLoop

!     ave(zone) = sumZ/Z% VolumeZone
     ave(zone) = sumZ

   enddo ZoneLoop

 
   return
   end subroutine rtave

