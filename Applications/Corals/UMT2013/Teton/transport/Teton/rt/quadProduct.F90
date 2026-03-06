!***********************************************************************
!                        Version 1:  07/2003, PFN                      *
!                                                                      *
!   QUADPRODUCT - Computes a product quadrature set based on the       *
!                 number of polar and azimuthal directions and the     *
!                 specified polar axis.                                *
!                                                                      *
!   Input:   nang       - number of angles                             *
!            npolar     - number of polar angles                       *
!            nazimuthal - number of azimuthal angles                   *
!            polaraxis  - polar axis                                   *
!                                                                      *
!   Output:  OMEGA  - group dependent direction cosines (mu,eta,xi)    *
!            QUADWT - group dependent quadrature weights               *
!                                                                      *
!   Allowed values:  1 <= npolar <= 18, 1 <= nazimuthal <= 22          *
!                                                                      *
!                                                                      *
!***********************************************************************

   subroutine quadProduct(self)

   use kind_mod
   use constant_mod
   use Quadrature_mod
   use QuadratureData_mod

   implicit none

!  Arguments

   type(Quadrature)          :: self

!  Local

   integer    :: i,iPhi,jTheta,m,Phi1,Phi2,Theta1,Theta2,nangoct

   integer    :: npolar,nazimuthal,polaraxis

   real(adqt) :: cosineTheta
   real(adqt) :: sineTheta
   real(adqt) :: cosinePhi

   real(adqt) :: omegaX(self% npolar*self% nazimuthal)
   real(adqt) :: omegaY(self% npolar*self% nazimuthal)
   real(adqt) :: omegaZ(self% npolar*self% nazimuthal)
   real(adqt) :: qweight(self% npolar*self% nazimuthal)

!  Constants

   npolar     = self% npolar
   nazimuthal = self% nazimuthal
   polaraxis  = self% polaraxis

!  Check for Errors

   if (npolar < 1 .or. npolar > 18) then
     call f90fatal("ERROR: npolar must be in range 1 <= npolar <= 18")
   endif

   if (nazimuthal < 1 .or. nazimuthal > 22) then
     call f90fatal("ERROR: nazimuthal must be in range 1 <= nazimuthal <= 22")
   endif

   if (polaraxis < 1 .or. polaraxis > 3) then
     call f90fatal("ERROR: polaraxis must be in range 1 <= polaraxis <= 3")
   endif

!  Set omegaX, omegaY, omegaZ based on the choice of polar axis
!  using tabulated values

   Phi1    = first(nazimuthal) 
   Phi2    =  last(nazimuthal) 
   Theta1  = first(npolar) 
   Theta2  =  last(npolar) 

   nangoct = npolar*nazimuthal

   m = 0
   do iPhi=Phi1,Phi2

     cosinePhi = cosPhiXYZ(iPhi)

!  Note that we have replaced sineTheta*sinePhi below with a square-root
!  of the other two components. This gives greater accuracy in the
!  magnitude of the ordinate (which should equal one)

     do jTheta=Theta2,Theta1,-1

       cosineTheta = cosTheta(jTheta)
       sineTheta   = sqrt( one - cosineTheta*cosineTheta )
       m           = m + 1

       if (polaraxis == 1) then
         omegaX(m) = cosineTheta
         omegaY(m) = sineTheta*cosinePhi
         omegaZ(m) = sqrt( one - omegaX(m)*omegaX(m) - omegaY(m)*omegaY(m) )
       elseif (polaraxis == 2) then
         omegaY(m) = cosineTheta
         omegaZ(m) = sineTheta*cosinePhi
         omegaX(m) = sqrt( one - omegaY(m)*omegaY(m) - omegaZ(m)*omegaZ(m) )
       elseif (polaraxis == 3) then
         omegaZ(m) = cosineTheta
         omegaX(m) = sineTheta*cosinePhi
         omegaY(m) = sqrt( one - omegaX(m)*omegaX(m) - omegaZ(m)*omegaZ(m) )
       endif

       qweight(m) = weightTheta(jTheta)*weightPhiXYZ(iPhi)

     enddo
   enddo

!  Set the direction cosines and weights; note that the
!  angles are numbered consecutively in an octant.
!  NANGOCT is the number of angles per octant.


   do i=1,nangoct

!  Octant 1  mu>0, eta>0, xsi>0

     self% omega(1,i) = omegaX(i) 
     self% omega(2,i) = omegaY(i) 
     self% omega(3,i) = omegaZ(i) 
     self% weight(i)  = qweight(i) 

!  Octant 2  mu<0, eta>0, xsi>0

     self% omega(1,nangoct+i)   = -omegaX(i)
     self% omega(2,nangoct+i)   =  omegaY(i) 
     self% omega(3,nangoct+i)   =  omegaZ(i) 
     self% weight(nangoct+i)    =  qweight(i) 

!  Octant 3  mu<0, eta<0, xsi>0

     self% omega(1,2*nangoct+i) = -omegaX(i)
     self% omega(2,2*nangoct+i) = -omegaY(i)
     self% omega(3,2*nangoct+i) =  omegaZ(i) 
     self% weight(2*nangoct+i)  =  qweight(i) 

!  Octant 4  mu>0, eta<0, xsi>0

     self% omega(1,3*nangoct+i) =  omegaX(i) 
     self% omega(2,3*nangoct+i) = -omegaY(i)
     self% omega(3,3*nangoct+i) =  omegaZ(i) 
     self% weight(3*nangoct+i)  =  qweight(i) 

!  Octant 5  mu>0, eta>0, xsi<0

     self% omega(1,4*nangoct+i) =  omegaX(i) 
     self% omega(2,4*nangoct+i) =  omegaY(i) 
     self% omega(3,4*nangoct+i) = -omegaZ(i)
     self% weight(4*nangoct+i)  =  qweight(i) 

!  Octant 6  mu<0, eta>0, xsi<0

     self% omega(1,5*nangoct+i) = -omegaX(i)
     self% omega(2,5*nangoct+i) =  omegaY(i) 
     self% omega(3,5*nangoct+i) = -omegaZ(i)
     self% weight(5*nangoct+i)  =  qweight(i) 

!  Octant 7  mu<0, eta<0, xsi<0

     self% omega(1,6*nangoct+i) = -omegaX(i)
     self% omega(2,6*nangoct+i) = -omegaY(i)
     self% omega(3,6*nangoct+i) = -omegaZ(i)
     self% weight(6*nangoct+i)  =  qweight(i) 

!  Octant 8  mu>0, eta<0, xsi<0

     self% omega(1,7*nangoct+i) =  omegaX(i) 
     self% omega(2,7*nangoct+i) = -omegaY(i)
     self% omega(3,7*nangoct+i) = -omegaZ(i)
     self% weight(7*nangoct+i)  =  qweight(i) 

   enddo


   return
   end subroutine quadProduct


