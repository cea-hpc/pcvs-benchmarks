!***********************************************************************
!                        Version 1:  02/06, PFN                        *
!                                                                      *
!   getGeometry - This routine gets mesh volumes and areas.            *
!                                                                      *
!***********************************************************************

   subroutine getGeometry

!  Include

   use kind_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Local

   integer zone

!  Save the "old" corner volumes

   do zone=1,Size%nzones
     Z => getZoneData(Geom, zone)
     Z% VolumeOld(:) = Z% Volume(:)
   enddo

!  Call the appropriate function based on the spatial dimensionality

   if (Size% ndim == 3) then
     call rtgeom3
   endif 



   return
   end subroutine getGeometry 


