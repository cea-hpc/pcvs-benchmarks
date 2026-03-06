!***********************************************************************
!                                                                      *
!                        Version 1: 09/1991 by MLA                     *
!                        Version 2: 01/2000 by PFN                     *
!                                                                      *
!   SNMREF -                                                           *
!                                                                      *
!                                                                      *
!     This routine, called by SNREFLECT, computes the                  *
!     angle (mref) that reflects onto angle mm at a given side.        *
!                                                                      *
!     The reflection algorithm works with angular currents instead     *
!     of angular fluxes.  (We believe currents are the important       *
!     quantities at boundaries.)  For a given incoming angle, the      *
!     algorithm picks one outgoing angle for its partner, and          *
!     requires that the net current caused by the pair vanish:         *
!                                                                      *
!      w(mm)*|cosine(mm)|*psi(mm) = w(mref)*|cosine(mref)|*psi(mref)   *
!                                                                      *
!     This routine returns mref and the ratio:                         *
!                                                                      *
!        cosrat  =  (w(mref)*|cosine(mref)|) / (w(mm)*|cosine(mm)|)    *
!                                                                      *
!     The main 'flaw' in our algorithm is that an isotropic angular    *
!     flux will not be isotropically reflected.  (The net current will *
!     always be zero, however.)                                        *
!                                                                      *
!     Input:                                                           *
!       Minc    index of the angle whose partner we must find,         *
!       Area    area vector,                                           *
!       omega   array of quadrature direction cosines,                 *
!                                                                      *
!     Output:                                                          *
!       mref    index of the partner angle,                            *
!       cosrat  ratio of cosines;  see above                           *
!                                                                      *
!***********************************************************************

   subroutine snmref(ndim,Minc,NREFLECT,REFLANGLE,  &
                     Area,COSRATIO)

   use kind_mod
   use constant_mod
   use Quadrature_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: ndim,Minc

   integer,    intent(inout) :: nreflect

   integer,    intent(inout) :: ReflAngle(QuadSet% NumAngles) 

   real(adqt), intent(in)    :: Area(ndim)

   real(adqt), intent(inout) :: cosratio(QuadSet% NumAngles) 

!  Local

   integer    :: i1,i2,i3,ia,idim,idim2,m1,m2,  &
                 nzero,nmax,mref,NumAngles

   real(adqt) :: fuz,tol,domega1,domega2,ratio,dot,dotWW,dot_Minc,  &
                 AreaMag,AreaMagInv,wf_sum,eps,wf,wf_m

   real(adqt) :: omega_inc(ndim), omega_refl(ndim,QuadSet% NumAngles)

   character(len=8) :: arbitrary
 
!  Constants

   parameter (fuz=1.d-6)
   parameter (tol=1.d-10)

   NumAngles    = QuadSet% NumAngles
   omega_inc(:) = QuadSet% omega(:,Minc)

!  Most of the time, we'll have reflection in one of the directions
!  of the coordinate system.  The component of OMEGA that changes sign
!  corresponds to the component of the normal to the side (or face)
!  that is non-zero ( e.g. if Area(1).ne.0 then we have reflection in 
!  mu=omega(1)  ).  First we check for the component that changes
!  sign ( omega(idim) ), and then we check to see that the other
!  components are unchanged.

!  90 degree reflection - First check to see that all 
!  components but one are "zero"

   AreaMag = zero 

   do idim=1,ndim
     AreaMag = AreaMag + Area(idim)*Area(idim)
   enddo

   nzero = 0
   nmax  = 0
   i1    = 2
   i2    = ndim - 1 

   do idim=1,ndim
     ratio = abs( Area(idim)*Area(idim)/AreaMag )
     if (ratio < tol) then
       nzero = nzero + 1
       i1    = min(i1,idim)
       i2    = max(i2,idim)
     else
       nmax  = idim
     endif
   enddo

   if ( nzero == (ndim - 1) ) then

     mref = 0

     do ia=1,NumAngles
       if (abs(QuadSet% omega(nmax,ia) + omega_inc(nmax)) < fuz ) then

         domega1 = abs(QuadSet% omega(i1,ia) - omega_inc(i1))
         domega2 = abs(QuadSet% omega(i2,ia) - omega_inc(i2))

         if (domega1 < fuz .and. domega2 < fuz) then
           mref = ia 
         endif

       endif
     enddo

     if (mref == 0) then
       call f90fatal("90 degrees; No relected angle found in SNMREF")
     endif

     nreflect     = 1
     ReflAngle(1) = mref
     cosratio(1)  = one

   else 
 
!  If we make it here, the next easiest case is a 45 degree angle

     arbitrary = "yes"
     do idim=1,ndim
       idim2 = mod(ndim+idim,ndim) + 1
       ratio = abs( (Area(idim) + tol)/(Area(idim2) + tol) )
       if (ratio < one+fuz .and. ratio > one-fuz) then
         i1        = idim
         i2        = idim2
         i3        = mod(ndim+i2,ndim) + 1
         arbitrary = "no"
       endif
     enddo

     if (arbitrary == "no") then
 
       mref = 0
       do ia=1,NumAngles
         if (abs(QuadSet% omega(i1,ia) - omega_inc(i2)) < fuz ) then
           if (abs(QuadSet% omega(i2,ia) - omega_inc(i1)) < fuz ) then
             if (abs(QuadSet% omega(i3,ia) - omega_inc(i3)) < fuz ) then
               mref = ia 
             endif
           endif
         endif
       enddo
 
       if (mref == 0) then
         call f90fatal("45 degrees; No reflected angle found in SNMREF")
       endif

       nreflect     = 1
       ReflAngle(1) = mref
       cosratio(1)  = one
 
     elseif (arbitrary == "yes") then

!------------------------------------------------------------------------------
! THEORY: REFLECTIVE B.C.'s on ARBITRARILY ORIENTED SURFACES
!
! If the surface is not orthogonal then we have to play some tricks to get
! reflected intensities.
!
! The exact reflected direction can be computed as:
!
!                omega_xit .dot. A_fep
!      A-prime = --------------------- A_fep
!                   A_fep .dot A_fep
!
!      Omega_refl = omega_xit - 2*A-prime
!
! Since we know the intensity along this "exact direction" (which is probably
! not in our set of discrete ordinate directions) we must "spread" this
! intensity (psi_w) among nearby d.o. directions.  This must be done in a
! fashion that preserves the net current flow across the boundary.  For a
! reflective surface the net flow should be zero.  We also want all of our
! reflected angular fluxes to be positive.
!
! Given an exiting direction we would compute Omega_refl and then "spread" this
! angular flux among "nearby" discrete ordinate directions.  We choose this
! set to be:
!
!     M = {k :: omega(k) .dot. A_fep < 0}
!               \----------------------/
!                 incoming directions
!
! To preserve the net current flow across A_fep we require (let j be the
! exiting direction of interest):
!
!    ( A .dot. omega_j ) * psi_j * wt_j
!             = - SUM ( A .dot. omega(k) ) * psi(k) * wt(k)
!               [k in {M}]
!
! This leads us to the following form for the j'th component of Psi(k),
! k is in {M}.
!
!           / wt_j * A .dot. omega_j \           delta(k,j)
! psi_k = - | ---------------------- | * psi_j * ----------
!           \ wt_k * A .dot. omega_k /              D(j)
!
! For now we let the weight delta(k,j) be defined as (1/sin(theta_k)).
! Where theta_k is the angle between omega_k and the reflection of omega_j
!
!     Theta_k = 1/2 * acos (omega_k .dot. omega_refl_j)
!
! We also requrie that D(j) = SUM [ delta(k,j) ]
!                           [k in {M}]
!
! For balance to hold     SUM [ delta(k,j) / D(j) ] == 1
!                       [k in {M}]
!
! Note that our angle of interest Omega_m is a member of {M}
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! IMPLEMENTATION:
!
! The theory above assumes that we have an exiting direction and intensity,
! psi(iw,m).  However, in SNAC, we need to build an incoming intensity in a
! specified direction.  To do this we will establish a set of exiting
! directions Omega_j in the set {M-tilde} that can contribute in building
! the reflected direction psi_m (k=m is a member of set {M}).  We define this
! set of exiting directions to be:
!
! {M-tilde}
!      = { j:: omega(j) .dot. A_fep > 0 }
!              \----------------------/
!                 exiting directions
!
! We define omega_refl(j) to be the exact reflected direction (not necessarily
! a discrete ordinate direction) of omega(j).  Omega(k=m) is the direction for
! which we are trying to build psi(m).
!
! It should be noted that the set {M}_j from the Theory section depends on
! omega_refl(j).
!
! To build psi(k=m) we will loop over the directions omega(j) in {M-tilde}.  For
! each of these directions we determine the contribution from psi(omega(j)) to
! psi(omega(k=m)).  Finally, we keep a running sum to build psi(k=m) from each of
! these contributing directions.
!------------------------------------------------------------------------------

! Determine the components for the set {M-tilde}.
! These are the exiting directions (omega_j) that can contribute to psi_m.
! Find the exact reflected direction omega_refl
!------------------------------------------------------------------------------

       dot_Minc   = DOT_PRODUCT( omega_inc(:),Area(:) )
       AreaMagInv = one/AreaMag
       nreflect   = 0

       do ia=1,NumAngles
         dot = DOT_PRODUCT( QuadSet% omega(:,ia),Area(:) )

         if (dot > zero) then
           nreflect               = nreflect + 1
           ReflAngle(nreflect)    = ia
           omega_refl(:,nreflect) = QuadSet% omega(:,ia) - two*dot*AreaMagInv*Area(:)
         endif
       enddo

! Loop over each of the directions j in the set {M_tilde}.  For each j:
!    1. Build the set {M}_j, which is the set of directions that omega_j can
!        contribute to.  Omega(k=m) is a member of {M}.
!    2. Build the "sine sum" as we build {M}.
!    3. With the "sine sum" we can now define the contribution coefficient for
!       psi(M_tilde_set(j)) toward psi_m.  This is the variable "cosratio".
!------------------------------------------------------------------------------

       ReflectAngles: do ia=1,nreflect

         wf_sum = zero
         m2     = ReflAngle(ia)

! If the dot product of the direction of interest (Minc) and an
! exact reflected direction is close to one (i.e. the angle between
! the two vectors is close to zero) it's weight will be very large. 

         dotWW = min( DOT_PRODUCT( omega_inc(:),omega_refl(:,ia) ), one )
         eps   = one - dotWW + tol
         wf_m  = one/(eps*eps*eps*eps)

! Build {M} and compute the sum of the weights (wf_sum).

         do m1=1,NumAngles
           dot = DOT_PRODUCT( QuadSet% omega(:,m1),Area(:) )
           if (dot < zero) then

! Is the angle in the same hemisphere as omega_refl(j)?
! Omega_m1 .dot. Omega_refl(j) > 0?

             dotWW = min( DOT_PRODUCT( QuadSet% omega(:,m1),omega_refl(:,ia) ), one )
             eps   = one - dotWW + tol
             wf    = one/(eps*eps*eps*eps)
             wf_sum = wf_sum + wf

           endif
         enddo

! Find the fraction of psi_j that contributes to the building of psi(k=m).

         dot          = DOT_PRODUCT( QuadSet% omega(:,m2),Area(:) )
         cosratio(ia) = -(QuadSet% weight(m2)  *dot*wf_m)/  &
                         (QuadSet% weight(Minc)*dot_Minc*wf_sum)

       enddo ReflectAngles


     endif

   endif

 

   return
   end subroutine snmref

