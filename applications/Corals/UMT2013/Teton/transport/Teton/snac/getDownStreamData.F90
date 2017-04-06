!***********************************************************************
!                        Version 1:  01/2009, PFN                      *
!                                                                      *
!   getDownStreamData - Creates a list of downstream corners for the   *
!                       input zone and angle.                          *
!                                                                      *
!***********************************************************************

   subroutine getDownStreamData(zone, omega, NEED, DOWNSTREAMC, nDSC)

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: zone
   integer,    intent(inout) :: need(Size%maxCorner)
   integer,    intent(inout) :: DownStreamC(Size%maxcf,Size%maxCorner)
   integer,    intent(inout) :: nDSC(Size%maxCorner)

   real(adqt), intent(in)    :: omega(Size%ndim)

!  Local Variables

   integer    :: id, izero, ndim
   integer    :: c, cez, nCorner, nCFaces

   real(adqt) :: aezm 

!  Constants

   parameter (izero=0)

   ndim = Size%ndim

!  For incoming corner-faces we increment the need array; for outgoing
!  corner-faces we put the downstream corner number into an index list.

   DownStreamC(:,:) = izero 
   nDSC(:)          = izero
   need(:)          = izero


   Z       => getZoneData(Geom, zone)

   nCorner = Z% nCorner
   nCFaces = Z% nCFaces

   CornerLoop: do c=1,nCorner

     CornerFaceLoop: do id=1,nCFaces
 
!  Get downstream corner number

       cez  = Z%Connect(3,id,c)

!  Omega dot Outward normal - IMPORTANT: the dot product must be
!  coded this way to be compatible with the coding in SNSWP3D and SNSWP2D.
!  Failure to comply results in wrong answers!

!  Corner Face EZ (neighboring corner, same zone)

       if (cez > c) then

         if (ndim == 3) then
                                                                                                 
           aezm = omega(1)*Z%A_ez(1,id,c) +  &
                  omega(2)*Z%A_ez(2,id,c) +  &
                  omega(3)*Z%A_ez(3,id,c)
                                                                                                 
         elseif (ndim == 2) then
                                                                                                 
           aezm = omega(1)*Z%A_ez(1,id,c) +  &
                  omega(2)*Z%A_ez(2,id,c)
                                                                                                 
         endif

         if (aezm < zero) then
           need(c)                    = need(c)   + 1
           nDSC(cez)                  = nDSC(cez) + 1
           DownStreamC(nDSC(cez),cez) = c
         elseif (aezm > zero) then
           need(cez)                  = need(cez) + 1
           nDSC(c)                    = nDSC(c)   + 1
           DownStreamC(nDSC(c),c)     = cez
         endif

       endif

     enddo CornerFaceLoop

   enddo CornerLoop


 
   return
   end subroutine getDownStreamData 

