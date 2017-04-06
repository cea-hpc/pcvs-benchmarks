!***********************************************************************
!                        Version 1:  07/03, PFN                        *
!                                                                      *
!   Jacobi - Returns the n, n-1, and n-2 Jacobi polynomials and        *
!            their derivatives.                                        *
!                                                                      *
!                                                                      *
!   Input:      n             Order of the Jacobi polynomial           *
!               alpha, beta   Degrees of the Jacobi polynomial         *
!                                                                      *
!   Output:     xJac          Roots of the derivative of the Jacobi    *
!                             polynomial                               *
!                                                                      *
!***********************************************************************

   subroutine Jacobi(n,alp,bet,rv,x,   &
                     poly,pder,polym1,pderm1,polym2,pderm2)

   use kind_mod
   use constant_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: n

   real(adqt), intent(in)    :: alp,bet,rv,x 

   real(adqt), intent(inout) :: poly,pder,polym1,pderm1,polym2,pderm2 

!  Local Variables

   integer    :: k

   real(adqt) :: kreal, apb, a1 , a2 , a3 , a4 , b3 
   real(adqt) :: polyn , pdern , psave , pdsave , polylst , pderlst 

!  Constants

   apb = alp + bet


   poly = one 
   pder = zero 

   if ( n == 0 ) return

   polylst = poly
   pderlst = pder
   poly    = rv*x
   pder    = rv   

   if ( n == 1 ) return

   do k=2,n

     kreal = real(k,adqt)

     a1 =   two*kreal*( kreal + apb )*( two*kreal + apb - two)
     a2 = ( two*kreal + apb - one )*( alp*alp - bet*bet )
     b3 =   two*kreal + apb - two 
     a3 =   b3 *( b3 + one )*( b3 + two )
     a4 =   two*( kreal + alp - one )*( kreal + bet - one )*( two*kreal + apb )

     polyn = ( ( a2 + a3*x )*poly - a4*polylst )/a1
     pdern = ( ( a2 + a3*x )*pder - a4*pderlst + a3*poly )/a1

     psave   = polylst
     pdsave  = pderlst
     polylst = poly
     poly    = polyn
     pderlst = pder
     pder    = pdern

   enddo

   polym1 = polylst
   pderm1 = pderlst
   polym2 = psave
   pderm2 = pdsave

 
 
   return
   end subroutine Jacobi 

