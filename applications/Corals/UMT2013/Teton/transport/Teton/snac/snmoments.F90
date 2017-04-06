!***********************************************************************
!                        Version 1:  05/95, PFN                        *
!                                                                      *
!   SNMOMENTS - This routine, called by SNFLWRZA and SNFLWXYZ          *
!               calculates the required spherical harmonic moments     *
!               [phic] of the angular flux [psic]. It uses the array   *
!               ynm(n,m), whose definition is:                         * 
!                                                                      *
!               ynm(n,m) = real part of (l,k)th spherical harmonic,    *
!                          evaluated at the mth direction, where       *
!                                                                      *
!                             n = 1 + l*(l+1)/2 + k                    *
!                                                                      *
!            This routine is designed to accumulate moments as an      *
!            angle is calculated and does not require storage of the   *
!            full angular flux array.  It is assumed that the moment   *
!            array has been initialized before the loop over angles    *
!            in SNFLWRZA or SNFLWXYZ.                                  *
!                                                                      *
!                                                                      *
!   Input:   psic     - angular flux                   (E/A/t/ster)    *
!            quadwt   - quadrature weights                      (0)    *
!            ynm      - spherical harmonics                     (0)    *
!                                                                      *
!   Output:  PHIC     - flux moments                        (E/A/t)    *
!                                                                      *
!***********************************************************************

   subroutine snmoments(psic, PHI)

   use kind_mod
   use constant_mod
   use Quadrature_mod
   use Size_mod

   implicit none

!  Arguments

   real(adqt), intent(in)    :: psic(QuadSet%Groups,Size%ncornr,QuadSet%NumAngles) 

   real(adqt), intent(inout) :: Phi(QuadSet%NumMoments*QuadSet%Groups,Size%ncornr)

!  Local

   integer    :: ic, ig, Angle, Groups, ncornr

   real(adqt) :: quadwt 

!  Add this angles contribution to the flux moments

   Groups = QuadSet% Groups
   ncornr = Size% ncornr

   Phi(:,:) = zero

   AngleLoop: do Angle=1,QuadSet%NumAngles

     quadwt = QuadSet% Weight(Angle)

     if (quadwt /= zero) then

       do ic=1,ncornr
         do ig=1,Groups
           Phi(ig,ic) = Phi(ig,ic) + quadwt*psic(ig,ic,Angle)
         enddo
       enddo

     endif

   enddo AngleLoop

 
   return
   end subroutine snmoments


