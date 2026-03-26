!***********************************************************************

   subroutine snynmset(self, ndim, isctp1)

   use kind_mod
   use constant_mod
   use Quadrature_mod

   implicit none

!  Arguments

   type(Quadrature)          :: self

   integer,    intent(in)    :: ndim, isctp1

!  Local

   integer :: m, idim, test
 
!***********************************************************************
!
!     version 1 1/94 by marvin adams
!
!
!     This routine, called by RSWP3D, sets the ynm array.  ynm(n,m)
!     is the (real part of the) nth spherical harmonic evaluated at
!     the mth quadrature direction.
!
!     If iscat = isctp1-1 is the order of anisotropic scattering, then
!     we need the (l,k)th spherical harmonic for l = 0,...,iscat and
!     k = 0,...,l.
!
!     We actually use the index n instead of l and k;  n is defined:
!
!                  n = 1 + l*(l+1)/2 + k
!
!***********************************************************************

!  Check to make sure that the moment count and scattering order agree

   if (ndim == 1) then
     test = isctp1
   elseif (ndim == 2) then
     test = isctp1*(isctp1+1)/2
   elseif (ndim == 3) then
     test = isctp1*isctp1
   endif

   if (self% NumMoments /= test) then
     call f90fatal("Moment count wrong in SNYNMSET")
   endif
 
!  Loop over quadrature angles:
 
   do m=1,self% NumAngles
 
!    l=k=0   =>  Ylk = 1.0, n=1
 
     self% ynm(1,m) = one 
 
!    l = 1;  k=0 => Y=u, n=2;  k=1 => Y=eta, n=3.
 
     if (isctp1 > 1) then

       do idim=1,ndim
         self% ynm(idim+1,m) = self% omega(idim,m)
       enddo

     endif

     if (isctp1 > 2) then
       call f90fatal("Not ready for scat ord > 1 in SNYNMSET")
     endif

   enddo
 

   return
   end subroutine snynmset


