!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTQUAD - Calls routines to calculate group dependent quadrature    *
!            sets for Sn radiation transport depending on geometry.    *
!                                                                      *
!   Input:   nordr  - group quadrature orders                          *
!            nangr  - total number of angles by group                  *
!            nangt  - sum of angles for all groups (with st+fin dir)   *
!            igeom  - geometry flag                                    *
!                                                                      *
!   Output:  OMEGA  - group dependent direction cosines (mu,eta,xi)    *
!            QUADWT - group dependent quadrature weights               *
!                                                                      *
!   Allowed values of "n" are:  1D:  2, 4, 6, 8, 10, 12, 16, 20, 32    *
!                               2D:  2, 4, 6, 8, 12, 16                *
!                               3D:  2, 4, 6, 8, 12, 16, 18, 20        *
!                                                                      *
!***********************************************************************
   subroutine rtquad(self, ndim, igeom) 


   use kind_mod
   use constant_mod
   use Quadrature_mod

   implicit none

!  Arguments

   type(Quadrature)             :: self 

   integer,          intent(in) :: ndim
   character(len=8), intent(in) :: igeom

!  Local

   integer          :: ia, iang, NumAngles

   real(adqt)       :: fac, sumwgt, wtiso

   character(len=8) :: TypeName

!  Select the appropriate quadrature based on geometry 

   NumAngles = self% NumAngles
   TypeName  = self% TypeName


   select case (igeom)

!  3-D XYZ

     case ('xyz')

       if (TypeName == 'levelsym') then
         call quadxyz(self)
       elseif (TypeName == 'product') then
         call quadProduct(self)
       elseif (TypeName == 'lobatto') then
         call quadLobatto(self)
       endif

   end select
 
!  Make sure that quadrature weights are normalized correctly

   select case (ndim)
     case (1)
       wtiso = half
     case (2)
       wtiso = one/(two*pi)
     case (3)
       wtiso = one/(four*pi)
   end select

   sumwgt = zero

   do ia=1,NumAngles
     sumwgt = sumwgt + self% weight(ia)
   enddo

   fac = one/(wtiso*sumwgt)

   do ia=1,NumAngles
     self% weight(ia) = fac*self% weight(ia)
   enddo

!  Identify starting and finishing directions

   self% StartingDirection(:)  = .FALSE.
   self% FinishingDirection(:) = .FALSE. 

   iang = -1
   do ia=1,NumAngles
     if (self% weight(ia) == zero) then
       if (iang == -1) then
         self% StartingDirection(ia) = .TRUE.
         iang = -iang
       elseif (iang == 1) then
         self% FinishingDirection(ia) = .TRUE.
         iang = -iang
       endif
     endif
   enddo

 
   return
   end subroutine rtquad


