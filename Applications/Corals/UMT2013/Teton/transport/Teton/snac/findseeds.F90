!***********************************************************************
!                        Version 1:  04/02, PFN                        *
!                                                                      *
!   FINDSEEDS    - This routine creates a list of starting points or   *
!                  "seeds" for the grid sweep.  The seeds are on the   *
!                  boundary of the grid and require no incident        *
!                  fluxes except from boundary conditions.  There may  *
!                  be situations where no seeds can be found; this     *
!                  will occur if there is a mesh cycle right at the    *
!                  boundary.  In this situation, we are forced to use  *
!                  some old information to get started.                * 
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!                                                                      *
!***********************************************************************
   subroutine findseeds(NSEED, MESHCYCLES, needZ, ZONESEED)

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use BoundaryList_mod
   use Boundary_mod

   implicit none

!  Arguments

   integer,    intent(inout) :: meshcycles,nseed 

   integer,    intent(inout) :: needZ(Size%nzones)
   integer,    intent(inout) :: zoneSeed(Size%nzones) 

!  Local Variables

   integer          :: i, ic, zone, nzones, zoneID, minNeed
   integer          :: nBoundary, nBdyElem, ib

   logical (kind=1) :: noseed 

!  Mesh Constants

   nzones    = Size% nzones
   nBoundary = getNumberOfBoundaries(RadBoundary)

!  Create a list of zone "seeds"

   nseed      = 0
   MeshCycles = 0

   ZoneLoop: do zone=1,nzones
     if (needZ(zone) == 0) then
       nseed           = nseed + 1
       zoneSeed(nseed) = zone 
     endif
   enddo ZoneLoop


   if (nseed == 0) then

!  If no seeds were found, find a zone on the boundary that requires
!  the fewest incident fluxes 

     minNeed = nzones
     zoneID  = 0

     BoundaryLoop: do i=1,nBoundary
       Bdy      => getBoundary(RadBoundary, i)
       nBdyElem =  getNumberOfBdyElements(Bdy)
       do ib=1,nBdyElem
         ic   = Bdy% BdyToC(ib)
         zone = Geom%CToZone(ic)

         if (needZ(zone) < minNeed) then
           zoneID  = zone
           minNeed = needZ(zone)
         endif

       enddo
     enddo BoundaryLoop

     nseed = 1
     zoneSeed(1) = zoneID
     MeshCycles = MeshCycles + needZ(zoneID)
     needZ(zoneID) = 0

   endif

!  Error Check

   if (nseed == 0) then 
     call f90fatal("No seeds found in FINDSEEDS!")
   endif



   return
   end subroutine findseeds 

