!***********************************************************************
!                        Version 1:  07/03, PFN                        *
!                                                                      *
!   GaussLegendreLobattoWgts - Given the points (xJac) of the          *
!                              Gauss-Legendre-Lobatto quadrature it    *  
!                              returns the weights.                    *
!                                                                      *
!        -1 = xJac[0] < xJac[1] < ... < xJac[N-1] < xJac[N] = 1        *
!                                                                      *
!                                                                      *
!   Input:      n             Order of the Jacobi polynomial           *
!               alpha, beta   Degrees of the Jacobi polynomial         *
!                                                                      *
!   Output:     xJac          Roots of the derivative of the Jacobi    *
!                             polynomial                               *
!                                                                      *
!***********************************************************************

   subroutine GaussLegendreLobattoWgts(n,xJac,wgtJac)

   use kind_mod
   use constant_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: n

   real(adqt), intent(in)    :: xJac(0:n) 

   real(adqt), intent(inout) :: wgtJac(0:n) 

!  Local Variables

   integer    :: l,ll,nn

   real(adqt) :: weight0 

   real(adqt) :: Plm(0:2*n+1) 

!  Weight for the pole 

   weight0   = two/(real(2*n+1,adqt)*real(2*n+2,adqt))
   wgtJac(0) = weight0

   nn        = 2*n+1

   do l=1,n

!  Find the Legendre polynomial of degree n

     Plm(0) = one 
     Plm(1) = xJac(l) 

     if (n > 1) then

       do ll=2,nn
         Plm(ll) = ( real(2*ll-1,adqt)*xJac(l)*Plm(ll-1) - & 
                     real(ll-1,adqt)*Plm(ll-2) )/real(ll,adqt)
       enddo

     endif 

     wgtJac(l) = weight0/( Plm(nn)*Plm(nn) )

   enddo
 
 
   return
   end subroutine GaussLegendreLobattoWgts 

