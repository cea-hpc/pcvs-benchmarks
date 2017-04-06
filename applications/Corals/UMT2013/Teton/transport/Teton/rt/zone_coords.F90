!***********************************************************************
!                        Version 1:  06/94, PFN                        *
!                                                                      *
!   ZONE_COORDS  - This routine computes the zone center in an         *
!                  arbitrary grid with 1, 2 or 3 dimensions.           *
!                                                                      *
!                                                                      *
!***********************************************************************

   subroutine zone_coords(ZX)

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(inout) :: zx(Size%ndim)
 
!  Local

   integer :: c, c0, nCorner, point

!  Accumulate sum of coordinates:

   zx(:)   = zero

   nCorner = Z% nCorner
   c0      = Z% c0

   do c=1,nCorner
     point = Geom%CToPoint(c0+c)
     zx(:) = zx(:) + Geom%px(:,point)
   enddo 

!  Divide by number of corners to get average coordinate:

   zx(:) = zx(:)/real(nCorner,adqt)


   return
   end subroutine zone_coords 


