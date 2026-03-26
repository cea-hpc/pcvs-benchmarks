!***********************************************************************
!                        Version 1:  11/98, PFN                        *
!                                                                      *
!   RTCOMPTON  - Performs the energy exchange between electrons and    *
!                photons due to Compton scattering.                    *
!                                                                      *
!   Input:   phinc - specified incident boundary intensities           *
!                                                                      *
!   Output:  DECOMPTON - energy change rate due to Compton             *
!                        scattering   (E/m/t)                          *
!                                                                      *
!***********************************************************************

   subroutine rtcompton(Phi)

   use kind_mod
   use constant_mod
   use radconstant_mod
   use Size_mod
   use Geometry_mod
   use Material_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(in)    :: Phi(Size%ngr,Size%ncornr) 

!  Local

   integer    :: ic,ig,iz,ncornr,ngr

   real(adqt) :: ac,trad,sumrad,sumSigsPhi,tr4min,tfloor

!  Constants:

   tfloor = Size%tfloor
   ac     = rad_constant*speed_light 
   tr4min = tfloor*tfloor*tfloor*tfloor
   ncornr = Size%ncornr
   ngr    = Size%ngr


   if (ngr == 1) then

     Mat%decompton(:) = zero

!     ZoneLoop: do zone=1,Size%nzones
                                                                                                 
!       Z       => getZoneData(Geom, zone)
                                                                                                 
!       nCorner = Z% nCorner
!       c0      = Z% c0
                                                                                                 
!       do c=1,nCorner
                                                                                                 
!         sumrad     = zero
!         sumSigsPhi = zero
                                                                                                 
!         do ig=1,ngr
!           sumrad     = sumrad     + Phi(ig,c0+c)
!           sumSigsPhi = sumSigsPhi + Phi(ig,c0+c)*Mat%sigs(ig,zone) 
!         enddo

!        trad = sqrt( sqrt( max(sumrad/ac,tr4min) ) )
!        Mat%decompton(c0+c) = (Mat%tec(c0+c) - trad)*sumSigsPhi/  &
!                            ( pi*electronMass*Mat%rho(zone) )
                                                                                                 
!       enddo
                                                                                                 
!     enddo ZoneLoop

   else

     Mat%decompton(:) = zero

   endif



   return
   end subroutine rtcompton


