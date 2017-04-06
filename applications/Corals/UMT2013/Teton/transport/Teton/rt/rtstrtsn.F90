!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTSTRTSN - Computes an angle-dependent source using the previous   *
!              time step angular intensity and saves copies of the     *
!              corner temperatures.                                    *
!                                                                      *
!   Input:   psir,tec                                                  *
!                                                                      *
!   Output:  ASRCC,TEZOLD,TECN                                         *
!                                                                      *
!***********************************************************************
 
   subroutine rtstrtsn(psir, Phi, PSIB)

   use kind_mod
   use constant_mod
   use Size_mod
   use Material_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(in)    :: psir(Size%ngr,Size%ncornr,Size%nangSN),  &
                                Phi(Size%ngr,Size%ncornr)

   real(adqt), intent(inout) :: psib(Size%ngr,Size%nbelem,Size%nangSN)

!  Local Variables

   integer    :: ia, ic, ig, ncornr, nzones, ngr
   integer    :: c, c0, nCorner, zone

   real(adqt) :: tau, sum, quadwt

!  Mesh Constants

   ncornr = Size%ncornr
   nzones = Size%nzones
   ngr    = Size%ngr
   tau    = Size%tau

!  If this is a time-dependent problem compute the angle-dependent
!  source using the old time-step intensity

   if (Size%itimsrc == 'exact') then
     ZoneLoop: do zone=1,nzones
       Z => getZoneData(Geom, zone)
       nCorner = Z% nCorner
       c0      = Z% c0 
       do ia=1,Size%nangSN
         do c=1,nCorner
           Z% STime(:,c,ia) = tau*psir(:,c0+c,ia)
         enddo
       enddo
     enddo ZoneLoop
   else
     do zone=1,nzones
       Z => getZoneData(Geom, zone)
       Z% STime(:,:,:) = zero 
     enddo
   endif

!  Initialize arrays

   do ic=1,ncornr
     Mat%tecn(ic)  = Mat%tec(ic)
     Mat%denec(ic) = zero
   enddo

!  Initialize the boundary flux array (PSIB)

   call setbdy(psir, PSIB)

!  Calculate zone-average energy density for convergence test

   do zone=1,nzones
     Z => getZoneData(Geom, zone)

     nCorner             = Z% nCorner
     c0                  = Z% c0
     Z% EnergyDensityOld = zero

     do c=1,nCorner
       sum = zero
       do ig=1,ngr
         sum = sum + Phi(ig,c0+c)
       enddo
       Z% EnergyDensityOld = Z% EnergyDensityOld + Z%Volume(c)*sum
     enddo
     Z% EnergyDensityOld = Z% EnergyDensityOld/Z%VolumeZone
   enddo

!  Compute zone-average temperature for convergence test

   call rtave(Mat%tec, Mat%TEZOLD)


   return
   end subroutine rtstrtsn
 
