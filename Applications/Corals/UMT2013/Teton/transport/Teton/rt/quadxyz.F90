!***********************************************************************
!                        Version 1:  01/93, PFN                        *
!                                                                      *
!   QUADXYZ - Calculates group dependent quadrature sets for Sn        *
!             radiation transport in XYZ geometry.                     *
!                                                                      *
!   Input:   norder - quadrature order                                 *
!            nang   - number of angles                                 *
!                                                                      *
!   Output:  OMEGA  - group dependent direction cosines (mu,eta,xi)    *
!            QUADWT - group dependent quadrature weights               *
!                                                                      *
!   Allowed values of "n" are:  2, 4, 6, 8, 10, 12, 14, 16, 18, 20     *
!                                                                      *
!   Directions per Octant:                                             *
!                                                                      *
!                             N   N(N+2)/8                             *
!                             2       1                                *
!                             4       3                                *
!                             6       6                                *
!                             8      10                                *
!                            10      15                                *
!                            12      21                                *
!                            14      28                                *
!                            16      36                                *
!                            18      45                                *
!                            20      55                                *
!                                                                      *
!***********************************************************************

   subroutine quadxyz(self)

   use kind_mod
   use constant_mod
   use Quadrature_mod
   use QuadratureData_mod

   implicit none

!  Arguments

   type(Quadrature) :: self

!  Local

   integer    :: i,ns,nangoct,jcos,norder

   real(adqt) :: halfpi

   real(adqt) :: amu,aeta,axi,awgt

!  Angular weights sum to pi/2 in each octant 

   halfpi = half*pi

!  Set the direction cosines and weights; note that the
!  angles are numbered consecutively in an octant.
!  NANGOCT is the number of angles per octant and NS is
!  an offset to the first angle and weight for the set.

   norder  = self% order
   nangoct = norder*(norder + 2)/8
   ns      = iang(norder) - 1
   jcos    = icoff(norder)

   do i=1,nangoct

     amu  = dircos( imu(ns+i)  + jcos )
     aeta = dircos( ieta(ns+i) + jcos )
     axi  = dircos( ixi(ns+i)  + jcos )
     awgt = weight( iwgt(ns+i) + jcos )

!  Octant 1  mu>0, eta>0, xsi>0

     self% omega(1,i) = amu
     self% omega(2,i) = aeta
     self% omega(3,i) = axi
     self% weight(i)  = halfpi*awgt

!  Octant 2  mu<0, eta>0, xsi>0

     self% omega(1,nangoct+i)   = -amu
     self% omega(2,nangoct+i)   =  aeta
     self% omega(3,nangoct+i)   =  axi
     self% weight(nangoct+i)    =  halfpi*awgt

!  Octant 3  mu<0, eta<0, xsi>0

     self% omega(1,2*nangoct+i) = -amu
     self% omega(2,2*nangoct+i) = -aeta
     self% omega(3,2*nangoct+i) =  axi
     self% weight(2*nangoct+i)  =  halfpi*awgt

!  Octant 4  mu>0, eta<0, xsi>0

     self% omega(1,3*nangoct+i) =  amu
     self% omega(2,3*nangoct+i) = -aeta
     self% omega(3,3*nangoct+i) =  axi
     self% weight(3*nangoct+i)  =  halfpi*awgt

!  Octant 5  mu>0, eta>0, xsi<0

     self% omega(1,4*nangoct+i) =  amu
     self% omega(2,4*nangoct+i) =  aeta
     self% omega(3,4*nangoct+i) = -axi
     self% weight(4*nangoct+i)  =  halfpi*awgt

!  Octant 6  mu<0, eta>0, xsi<0

     self% omega(1,5*nangoct+i) = -amu
     self% omega(2,5*nangoct+i) =  aeta
     self% omega(3,5*nangoct+i) = -axi
     self% weight(5*nangoct+i)  =  halfpi*awgt

!  Octant 7  mu<0, eta<0, xsi<0

     self% omega(1,6*nangoct+i) = -amu
     self% omega(2,6*nangoct+i) = -aeta
     self% omega(3,6*nangoct+i) = -axi
     self% weight(6*nangoct+i)  =  halfpi*awgt

!  Octant 8  mu>0, eta<0, xsi<0

     self% omega(1,7*nangoct+i) =  amu
     self% omega(2,7*nangoct+i) = -aeta
     self% omega(3,7*nangoct+i) = -axi
     self% weight(7*nangoct+i)  =  halfpi*awgt

   enddo


   return
   end subroutine quadxyz


