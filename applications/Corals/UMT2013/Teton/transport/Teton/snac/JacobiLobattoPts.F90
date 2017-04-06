!***********************************************************************
!                        Version 1:  07/03, PFN                        *
!                                                                      *
!   JacobiLobattoPts - Computes the roots of the derivative of the     *
!                      Jacobi polynomial of order N and degrees alpha  *
!                      and beta.  The N+2 points ( N roots and the     *
!                      two endpoints x=-1, x=1 ) are returned in xJac  *
!                      in increasing order.                            *
!                                                                      *
!        -1 = xJac[0] < xJac[1] < ... < xJac[N] < xJac[N+1] = 1        *
!                                                                      *
!                                                                      *
!   Input:      n             Order of the Jacobi polynomial           *
!               alpha, beta   Degrees of the Jacobi polynomial         *
!                                                                      *
!   Output:     xJac          Roots of the derivative of the Jacobi    *
!                             polynomial                               *
!                                                                      *
!***********************************************************************

   subroutine JacobiLobattoPts(n,xJac)

   use kind_mod
   use constant_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: n

   real(adqt), intent(inout) :: xJac(0:n) 

!  Local Variables

   integer    :: i, j, k, np, jm, kstop

   real(adqt) :: alpha,beta
   real(adqt) :: alp, bet, rv, det, rp, rm, a, b, epsilon
   real(adqt) :: pnp1p, pdnp1p, pnp, pdnp, pnm1p, pdnm1
   real(adqt) :: pnp1m, pdnp1m, pnm, pdnm, pnm1m
   real(adqt) :: pnp1, pdnp1, pn , pdn, pnm1
   real(adqt) :: dth, cd, sd, cs, ss, cssave
   real(adqt) :: poly, pder, recsum, delx, x 

!  Constants

   parameter (kstop=10)
   parameter (epsilon=1.d-12)

!  Alpha and beta specify the degree

   alpha = zero
   beta  = zero

   alp = alpha
   bet = beta
   rv  = one + alp

   np  = 2*(n + 1)

   call Jacobi(np,alp,bet,rv, one,pnp1p,pdnp1p,pnp,pdnp,pnm1p,pdnp1)
   call Jacobi(np,alp,bet,rv,-one,pnp1m,pdnp1m,pnm,pdnm,pnm1m,pdnm1)

   det =  pnp*pnm1m - pnm*pnm1p 
   rp  = -pnp1p 
   rm  = -pnp1m 
   a   = ( rp*pnm1m - rm*pnm1p )/det 
   b   = ( rm*pnp   - rp*pnm   )/det 

   xJac(0) = one 

   dth = pi/( two*real(2*n,adqt) + one ) 
   cd  = cos( two*dth )
   sd  = sin( two*dth )
   cs  = cos( dth )     
   ss  = sin( dth )     

   do j=1,n

     x    = cs
     k    = 0
     delx = two 

     do while( k < kstop  .and.  abs(delx) > epsilon  )
       call Jacobi(np,alp,bet,rv,x,pnp1,pdnp1,pn,pdn,pnm1,pdnm1)
       poly   = pnp1  + a*pn  + b*pnm1
       pder   = pdnp1 + a*pdn + b*pdnm1
       recsum = zero 
       jm     = j - 1

       do i=0,jm
         recsum = recsum + one/( x - xJac(i) )
       enddo

       delx = -poly/( pder - recsum*poly )
       x    = x + delx
       k    = k + 1
     enddo

     xJac(j) = x
     cssave  = cs*cd - ss*sd
     ss      = cs*sd + ss*cd
     cs      = cssave

   enddo

 
 
   return
   end subroutine JacobiLobattoPts 

