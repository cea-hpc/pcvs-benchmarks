!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTINIT - Calculates an initial radiation distribution.  The        *
!            energy density is a*T^4 where T is the initial radiation  *
!            temperature.  The energy is placed in a Planck spectrum   *
!            with an isotropic angular distribution.  Electron energy, *
!            electron temperature and ion energy are also set.         *
!                                                                      *
!   Input:   tez    - zone-average electron temperature   (T)          *
!            tiz    - zone-average ion temperature        (T)          *
!            trz    - zone-average radiation temperature  (T)          *
!                                                                      *
!   Output:  PSIR   - corner angular photon intensity     (E/A/t)      *
!            TEC    - corner electron temperatures        (T)          *
!            TIC    - corner ion temperatures             (T)          *
!                                                                      *
!   Local:   a      - radiation constant    (E/V/T^4)                  *
!                                                                      *
!***********************************************************************
 
   subroutine rtinit(erad, PSIR)

   use kind_mod
   use mpi_param_mod
   use mpif90_mod
   use constant_mod
   use radconstant_mod
   use Size_mod
   use Geometry_mod
   use Material_mod
   use QuadratureList_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(inout) :: erad

   real(adqt), intent(inout) :: psir(Size%ngr,Size%ncornr,Size%nangSN)

!  Local

   integer    :: ia, ig, ngrp, nangles, ngr, nzones
   integer    :: c, c0, nCorner, zone

   real(adqt) :: floor, wtiso, temp, t4

!  Stack Arrays

   real(adqt) :: gnu(Size%ngr+1)
   real(adqt) :: frac(Size%ngr+1)
   real(adqt) :: x(Size%ngr+1)
   real(adqt) :: planck(Size%ngr)

!  Constants

   floor   = zero
   erad    = zero
   ngr     = Size% ngr
   nzones  = Size% nzones
   nangles = Size% nangSN
   wtiso   = Size% wtiso

   ngrp    = ngr + 1

   gnu(:)  = getEnergyGroups(Quad,ngr)

!  Compute the fraction of the total emission in each energy group
!  The input for RTPLNK is (h*nu)/(k*Te).
 
   ZoneLoop: do zone=1,nzones

     Z       => getZoneData(Geom, zone)

     nCorner = Z% nCorner
     c0      = Z% c0

!  T4 has units of energy/area/time 

     temp = Mat%trz(zone)
     t4   = speed_light*rad_constant*temp*temp*temp*temp
 
!  Compute hnu/kt at upper energy boundary

     do ig=1,ngr+1
       x(ig) = gnu(ig)/temp
     enddo
 
     call rtplnk(ngrp,x,FRAC)

!  Use a lower bound of zero in calculating the Planckian

     frac(1)     = zero
     frac(ngr+1) = one
 
!  Compute the Planck function for all groups
 
     do ig=1,ngr
       planck(ig) = t4*(frac(ig+1) - frac(ig))
     enddo

!  Loop over all angles in group

     do ia=1,nangles
       do c=1,nCorner
         psir(:,c0+c,ia) = max(wtiso*planck(:),floor)
       enddo
     enddo

     do ig=1,ngr
       erad = erad + Z% VolumeZone*max(planck(ig),floor)
     enddo

!  Set corner temperatures

     do c=1,nCorner
       Mat%tec(c0+c) = Mat%tez(zone)
     enddo
 
   enddo ZoneLoop
 

   if (Size%igeom == 'rz') then
     erad = two*pi*erad/speed_light
   else
     erad = erad/speed_light
   endif


   call MPIAllReduceT(ERAD, "sum", MPI_COMM_WORLD)
 
 
   return
   end subroutine rtinit

