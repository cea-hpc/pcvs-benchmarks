!***********************************************************************
!                        Version 1:  07/2003, PFN                      *
!                                                                      *
!   QUADLOBATTO - Driver for computing Lobatto quadrature sets.        *
!                 The polar angles are the roots of the Lobatto        *
!                 polynomial and the azimuthal directions are evenly   *
!                 spaced.                                              *
!                                                                      *
!   Input:   nang       - number of angles                             *
!            npolar     - number of polar angles                       *
!            nazimuthal - number of azimuthal angles                   *
!            polaraxis  - polar axis                                   *
!                                                                      *
!   Output:  OMEGA  - group dependent direction cosines (mu,eta,xi)    *
!            QUADWT - group dependent quadrature weights               *
!                                                                      *
!                                                                      *
!***********************************************************************

   subroutine quadLobatto(self)

   use kind_mod
   use constant_mod
   use Quadrature_mod

   implicit none

!  Arguments

   type(Quadrature) :: self

!  Local

   integer    :: i,iPhi,jTheta,m,nang,nangoct

   integer    :: npolar,nazimuthal,polaraxis

   real(adqt) :: cosTheta, sineTheta, Phi, deltaPhi, sinePhi

   real(adqt) :: omegaX(self% npolar*self% nazimuthal),  &
                 omegaY(self% npolar*self% nazimuthal),  &
                 omegaZ(self% npolar*self% nazimuthal),  &
                 weight(self% npolar*self% nazimuthal)

   real(adqt) :: xJac(0:self% npolar),wgtJac(0:self% npolar)

   real(adqt) :: wgtTheta, wgtPhi

!  Constants

   npolar     = self% npolar
   nazimuthal = self% nazimuthal
   polaraxis  = self% polaraxis
   nang       = self% NumAngles

!  Find the cosines of the polar angle using the Lobatto quadrature rule
!  and their associated weights

   call JacobiLobattoPts(npolar,xJac)
   call GaussLegendreLobattoWgts(npolar,xJac,wgtJac)


   nangoct = npolar*nazimuthal

!  Set omegaX, omegaY, omegaZ based on the choice of polar axis

   Phi      = zero
   deltaPhi = pi/(two*real(nazimuthal+1,adqt))
   wgtPhi   = one/real(nazimuthal,adqt)

   m = 0

   do iPhi=1,nazimuthal
     Phi = Phi + deltaPhi
     sinePhi = sin(Phi)
     do jTheta=1,npolar

       cosTheta  = xJac(jTheta) 
       sineTheta = sqrt( one - cosTheta*cosTheta )
       wgtTheta  = wgtJac(jTheta)
       m = m + 1

       if (polaraxis == 1) then
         omegaX(m) = cosTheta
         omegaZ(m) = sineTheta*sinePhi
         omegaY(m) = sqrt( one - omegaZ(m)*omegaZ(m) - omegaX(m)*omegaX(m) )
       elseif (polaraxis == 2) then
         omegaY(m) = cosTheta
         omegaX(m) = sineTheta*sinePhi
         omegaZ(m) = sqrt( one - omegaX(m)*omegaX(m) - omegaY(m)*omegaY(m) )
       elseif (polaraxis == 3) then
         omegaZ(m) = cosTheta
         omegaY(m) = sineTheta*sinePhi
         omegaX(m) = sqrt( one - omegaZ(m)*omegaZ(m) - omegaY(m)*omegaY(m) )
       endif

       weight(m) = wgtTheta*wgtPhi

     enddo
   enddo

!  Set the two ordinates on the pole

   self% omega(:,nang-1)         = zero
   self% omega(:,nang)           = zero

   self% omega(polaraxis,nang-1) =  xJac(0)
   self% omega(polaraxis,nang)   = -xJac(0)

   self% weight(nang-1)          = wgtJac(0)
   self% weight(nang)            = wgtJac(0)

!  Set the direction cosines and weights; note that the
!  angles are numbered consecutively in an octant.
!  NANGOCT is the number of angles per octant.


   do i=1,nangoct

!  Octant 1  mu>0, eta>0, xsi>0

     self% omega(1,i) = omegaX(i) 
     self% omega(2,i) = omegaY(i) 
     self% omega(3,i) = omegaZ(i) 
     self% weight(i)  = weight(i) 

!  Octant 2  mu<0, eta>0, xsi>0

     self% omega(1,nangoct+i)   = -omegaX(i)
     self% omega(2,nangoct+i)   =  omegaY(i) 
     self% omega(3,nangoct+i)   =  omegaZ(i) 
     self% weight(nangoct+i)    =  weight(i) 

!  Octant 3  mu<0, eta<0, xsi>0

     self% omega(1,2*nangoct+i) = -omegaX(i)
     self% omega(2,2*nangoct+i) = -omegaY(i)
     self% omega(3,2*nangoct+i) =  omegaZ(i) 
     self% weight(2*nangoct+i)  =  weight(i) 

!  Octant 4  mu>0, eta<0, xsi>0

     self% omega(1,3*nangoct+i) =  omegaX(i) 
     self% omega(2,3*nangoct+i) = -omegaY(i)
     self% omega(3,3*nangoct+i) =  omegaZ(i) 
     self% weight(3*nangoct+i)  =  weight(i) 

!  Octant 5  mu>0, eta>0, xsi<0

     self% omega(1,4*nangoct+i) =  omegaX(i) 
     self% omega(2,4*nangoct+i) =  omegaY(i) 
     self% omega(3,4*nangoct+i) = -omegaZ(i)
     self% weight(4*nangoct+i)  =  weight(i) 

!  Octant 6  mu<0, eta>0, xsi<0

     self% omega(1,5*nangoct+i) = -omegaX(i)
     self% omega(2,5*nangoct+i) =  omegaY(i) 
     self% omega(3,5*nangoct+i) = -omegaZ(i)
     self% weight(5*nangoct+i)  =  weight(i) 

!  Octant 7  mu<0, eta<0, xsi<0

     self% omega(1,6*nangoct+i) = -omegaX(i)
     self% omega(2,6*nangoct+i) = -omegaY(i)
     self% omega(3,6*nangoct+i) = -omegaZ(i)
     self% weight(6*nangoct+i)  =  weight(i) 

!  Octant 8  mu>0, eta<0, xsi<0

     self% omega(1,7*nangoct+i) =  omegaX(i) 
     self% omega(2,7*nangoct+i) = -omegaY(i)
     self% omega(3,7*nangoct+i) = -omegaZ(i)
     self% weight(7*nangoct+i)  =  weight(i) 

   enddo


   return
   end subroutine quadLobatto


