!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTSET  - Sets variables needed for dimensioning dynamic arrays     *
!            in RSN.  For 1D spheres and cylinders, starting           *
!            directions are included.  For 2D RZ geometry, starting    *
!            and finishing directions are included.                    *
!                                                                      *
!   Input:   nordr       - Sn order by group (no starting direction)   *
!                                                                      *
!   Output:  NANGTR      - total number of angles for all groups       *
!                          (includes starting direction)               *
!            QuadDef     - quadrature definition by group;             *
!                          entry "ngr+1" is for acceleration           *
!                                                                      *
!                          1 = type                                    *
!                          2 = order                                   *
!                          3 = polar angles                            *
!                          4 = azimuthal angles                        *
!                          5 = polar axis                              *
!                          6 = number of angles in the set (Output)    *
!***********************************************************************
   subroutine setSnOrder(NangSN, QuadDef, gnu)

   use kind_mod
   use Size_mod
   use QuadratureList_mod
   use Quadrature_mod

   implicit none 
 
!  Arguments

   integer,    intent(inout)    :: nangSN

   integer,    intent(inout)    :: QuadDef(6,Size%ngr+1)

   real(adqt), intent(in)       :: gnu(Size%ngr+1)

!  Local Variables

   integer :: i, ig, quadtype, norder, npolar, nazimuthal, nangles
   integer :: NumSnSets
 

!  Quadratures depend on geometry

   GroupLoop: do ig=1,Size%ngr+1

     norder = QuadDef(2,ig)
     select case (Size% igeom)

       case ('cylinder')
         nangles = norder*(norder + 4)/4

       case ('sphere') 
         nangles = norder + 1

       case ('slab')
         nangles = norder

       case ('rz')
         quadtype   = QuadDef(1,ig)
         npolar     = QuadDef(3,ig)
         nazimuthal = QuadDef(4,ig)

         if (quadtype == 1) then
           nangles = norder*(norder + 6)/2
         elseif (quadtype == 2) then
           nangles = 4*npolar*(nazimuthal + 1)
         else
           call f90fatal("Invalid quadrature definition in setSnOrder")
         endif

       case ('xyz')
         quadtype   = QuadDef(1,ig)
         npolar     = QuadDef(3,ig)
         nazimuthal = QuadDef(4,ig)

         if (quadtype == 1) then
           nangles = norder*(norder + 2)
         elseif (quadtype == 2) then
           nangles = 8*npolar*nazimuthal
         elseif (quadtype == 3) then
           nangles = 8*npolar*nazimuthal + 2
         else
           call f90fatal("Invalid quadrature definition in setSnOrder")
         endif

       case default
         call f90fatal("Invalid geometry in setSnOrder")

     end select

     QuadDef(6,ig) = nangles
 
   enddo GroupLoop


!  Set the unique angle sets

   call rtbatch(QuadDef, gnu)


   NumSnSets = getNumSnSets(Quad)
   NangSN    = 0

   do i=1,NumSnSets
     QuadSet => getQuadrature(Quad, i)
     NangSN = NangSN + QuadSet% NumAngles
   enddo

!  Update values in Size module

   Size% nangSN = NangSN
   Size% npsi   = NangSN*Size% ngr


   return
   end subroutine setSnOrder

