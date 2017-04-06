!***********************************************************************
!                        Version 1:  09/96, PFN                        *
!                                                                      *
!   SNREFLECT - This routine, called by SNFLWXYZ and SNFLWRZA,         *
!               computes the boundary flux (PSIB) for angle m, on      *
!               reflecting boundaries for which angle m is incoming.   *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!                                                                      *
!***********************************************************************

   subroutine snreflect(Minc, PSIB) 

   use kind_mod
   use constant_mod
   use Size_mod
   use Quadrature_mod
   use BoundaryList_mod
   use Boundary_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: Minc

   real(adqt), intent(inout) :: psib(QuadSet%Groups,Size%nbelem,  &
                                     QuadSet%NumAngles) 

!  Local Variables

   integer    :: i, ib, Mref

   integer    :: nReflecting, nBdyElem, b1, b2

!  Constants

   nReflecting = getNumberOfReflecting(RadBoundary)

!  Loop over reflecting-boundary sets:
 
   ReflectingLoop: do i=1,nReflecting

     Bdy       => getReflecting(RadBoundary, i)
     nBdyElem  =  getNumberOfBdyElements(Bdy)
     b1        =  getFirstBdyElement(Bdy)
     b2        =  b1 + nBdyElem - 1

     Mref      =  getReflectedAngle(Bdy, Minc)

     if (Mref > 0) then

       do ib=b1,b2
         psib(:,ib,Minc) = psib(:,ib,Mref)
       enddo

     endif
 
   enddo ReflectingLoop 

 
   return
   end subroutine snreflect

