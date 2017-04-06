!***********************************************************************
!                        Version 2:  01/98, PFN                        *
!                                                                      *
!   findReflectedAngles - This routine perfroms two functions:         *
!                                                                      *
!            1. Checks that all reflecting faces are in the same       *
!               plane (a requirement)                                  *
!            2. For each reflecting boundary, it finds the             *
!               reflected angle for each incident angle                *
!                                                                      *
!***********************************************************************

   subroutine findReflectedAngles 

   use kind_mod
   use constant_mod
   use Size_mod
   use BoundaryList_mod
   use Boundary_mod
   use QuadratureList_mod
   use Quadrature_mod

   implicit none

!  Local Variables

   integer    :: idim, ndim, nreflect 
   integer    :: i, ib, nReflecting, nBdyElem
   integer    :: set, NumQuadSets, mRef, Angle
   integer    :: ReflAngle(Size%nangSN)

   real(adqt) :: eps, tol, A_mag, delta_A
   real(adqt) :: OmegaDotA
   real(adqt) :: A_set(Size%ndim)
   real(adqt) :: Area(Size%ndim)
   real(adqt) :: cosratio(Size%nangSN)

!  Constants

   parameter (tol=1.0d-6)
   parameter (eps=1.d-15)

   ndim        = Size%ndim
   nReflecting = getNumberOfReflecting(RadBoundary)
   NumQuadSets = getNumQuadSets(Quad)

!  Check if all reflecting faces are in the same plane 

   ReflectingLoop: do i=1,nReflecting

     Bdy      => getReflecting(RadBoundary, i)
     nBdyElem =  getNumberOfBdyElements(Bdy)

     A_set(:) =  Bdy% A_bdy(:,1)
     A_mag    =  DOT_PRODUCT( A_set(:),A_set(:) ) 
     A_set(:) =  A_set(:)/sqrt(A_mag)

     do ib=1,nBdyElem

       Area(:) = Bdy% A_bdy(:,ib)
       A_mag   = DOT_PRODUCT( Area(:),Area(:) ) 
       Area(:) = Area(:)/sqrt(A_mag)

       delta_A = zero

       do idim=1,ndim
          delta_A = delta_A + abs(A_set(idim) - Area(idim))
       enddo

       if ( delta_A > tol ) then
         call f90fatal("findReflectedAngles: Not all faces in reflecting plane") 
       endif

     enddo

!  Find the reflected angles

     Area(:) = Bdy% A_bdy(:,1)

     AngleSetLoop: do set=1,NumQuadSets

       QuadSet => getQuadrature(Quad, set)

       AngleLoop: do Angle=1,QuadSet% NumAngles

         OmegaDotA =  DOT_PRODUCT( QuadSet% omega(:,Angle),Area(:) )

         TestIncident: if (OmegaDotA < -eps) then

!  If OmegaDotA<0, Angle is incoming on this set. Here we test OmegaDotA<-eps
!  to account for roundoff errors if the direction is parallel to
!  the reflecting surface (e.g. in Lobatto quadratures). The routine
!  SNMREF computes the angle, MREF, that reflects onto Angle.
!  It also computes a multiplier, cosrat, that makes our
!  reflection algorithm conservative (i.e., net flux=0).
!  AREA contains the components of the outward normal for this boundary set.

           call snmref(ndim,Angle,NREFLECT,REFLANGLE,Area,COSRATIO)

           if (nreflect > 1) then
             call f90fatal("findReflectedAngles: found more than one reflected angle")
           endif

           mRef = ReflAngle(1)

         else

           mRef = -1

         endif TestIncident

         call setReflectedAngle(Bdy, Angle, mRef)

       enddo AngleLoop

!  Debug

!         do Angle=1,QuadSet% NumAngles
!           mRef = getReflectedAngle(Bdy, Angle)
!           write(6,100) Size%my_node, set, Angle, mRef
!         enddo

     enddo AngleSetLoop


   enddo ReflectingLoop

! 100 format("EDIT: node = ",i2,2x,"set = ",i2,2x,"Inc Angle = ",i3,2x,"Refl Angle = ",i3)



   return
   end subroutine findReflectedAngles 


