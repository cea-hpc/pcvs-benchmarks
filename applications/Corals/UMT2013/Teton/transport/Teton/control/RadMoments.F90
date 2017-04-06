!***********************************************************************
!                        Version 1:  03/02, PFN                        *
!                                                                      *
!   RadMoments - Calculate the radiative flux and pressure tensor for  *
!                each group and each zone.  These quantities will be   *
!                remapped and will be used to construct new corner     *
!                intensties after remap.                               *
!                                                                      *
!   Input:       psir   - radiation specific energy density   (E/m)    *
!                                                                      *
!   Output:      RadFlux        - radiative flux                       *
!                PressureTensor - radiation pressure tensor            *
!                                                                      *
!***********************************************************************
 
   subroutine RadMoments(Phi, RadEnergyDensity) 

   use kind_mod
   use constant_mod
   use radconstant_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none 

!  Arguments

   real(adqt), intent(in)    :: Phi(Size%ngr,Size%ncornr)

   real(adqt), intent(inout) :: RadEnergyDensity(Size%nzones,Size%ngr)

!  Local

   integer    :: c, c0, ig, ngr, nCorner, zone 

   real(adqt) :: factor, VolFrac

!***********************************************************************
!  Update Radiation Energy Density                                     * 
!***********************************************************************
 
   ngr                   = Size%ngr
   factor                = one/speed_light
   RadEnergyDensity(:,:) = zero

   ZoneLoop: do zone=1,Size%nzones

     Z => getZoneData(Geom, zone)

     nCorner = Z% nCorner
     c0      = Z% c0

     do c=1,nCorner

       VolFrac = factor*Z% Volume(c)/Z% VolumeZone
       
       do ig=1,ngr
         RadEnergyDensity(zone,ig) = RadEnergyDensity(zone,ig) +  &
                                     VolFrac*Phi(ig,c0+c)
       enddo

     enddo

   enddo ZoneLoop



   return
   end subroutine RadMoments 


