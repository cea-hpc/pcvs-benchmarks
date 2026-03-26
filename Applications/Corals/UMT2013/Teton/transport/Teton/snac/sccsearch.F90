!***********************************************************************
!                        Version 1:  03/02, PFN                        *
!                                                                      *
!   SCCSEARCH    - This recursive routine search the dependency graph  *
!                  for strongly-connected components (SCC).            *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!                                                                      *
!***********************************************************************
   recursive subroutine sccsearch(zone,ngraph,count,           &
                                  stackindex,MeshCycles,       &
                                  DownStreamZ,nDSZ,dfnum,      &
                                  lowlink,stack,new,onstack,   &
                                  tempList,LISTCYCLES)

   use kind_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: zone, ngraph

   integer,    intent(inout) :: count, stackindex, MeshCycles 

   integer,    intent(inout) :: dfnum(Size%ncornr)
   integer,    intent(inout) :: lowlink(Size%ncornr)
   integer,    intent(inout) :: stack(ngraph)
   integer,    intent(inout) :: listcycles(ngraph)
   integer,    intent(inout) :: DownStreamZ(2*Size%maxcf,Size%nzones)
   integer,    intent(inout) :: tempList(ngraph)
   integer,    intent(in)    :: nDSZ(Size%nzones)

   logical (kind=1), intent(inout) :: new(Size%ncornr)
   logical (kind=1), intent(inout) :: onstack(Size%ncornr)

!  Local Variables

   integer    :: i,ic2,zone2,id,cyclesize,zoneBreak,lowlinkZ,nzones
   integer    :: c, nCorner, nCFaces

   nzones = Size% nzones

!  Start the search procedure

   count         = count + 1
   dfnum(zone)   = count
   lowlink(zone) = count
   new(zone)     = .FALSE.

!  Put current "zone" on the stack

   stackindex        = stackindex + 1
   stack(stackindex) = zone 
   onstack(zone)     = .TRUE. 

!  Loop over all downstream zones that have not been completed 

   ZoneLoop: do i=1,nDSZ(zone) 

     zone2 = DownStreamZ(i,zone) 

     if (zone2 <= nzones) then

       if ( new(zone2) ) then

         call sccsearch(zone2,ngraph,count,           &
                        stackindex,MeshCycles,        &
                        DownStreamZ,nDSZ,dfnum,       &
                        lowlink,stack,new,onstack,    &
                        tempList,LISTCYCLES)

         if (lowlink(zone2) < lowlink(zone)) then
           lowlink(zone) = lowlink(zone2)
         endif

       else

         if (dfnum(zone2) < dfnum(zone) .and.  &
             onstack(zone2)             .and.  &
             lowlink(zone2) < lowlink(zone)) then

           lowlink(zone) = lowlink(zone2)
         endif
 
       endif

     endif

   enddo ZoneLoop

!  Cycle Check

   CheckCycle: if (lowlink(zone) == dfnum(zone)) then

     zone2          = stack(stackindex)
     stackindex     = stackindex - 1
     onstack(zone2) = .FALSE. 

     DetectCycle: if (zone2 /= zone) then

       cyclesize  = 0

       do while (zone2 /= zone)
         cyclesize           = cyclesize + 1
         tempList(cyclesize) = zone2 

         zone2               = stack(stackindex)
         stackindex          = stackindex - 1
       enddo

       cyclesize             = cyclesize + 1
       tempList(cyclesize)   = zone2
       onstack(tempList(1))  = .TRUE.

!***********************************************************************
!  Now break all connections of zones on the stack to the lowest       *
!  link.                                                               *
!***********************************************************************

       lowlinkZ = tempList(cyclesize)

!  Loop over all neighbors for this zone and find the ones on the stack

       Z => getZoneData(Geom, lowlinkZ)

       nCorner = Z% nCorner
       nCFaces = Z% nCFaces

       do c=1,nCorner

         do id=1,nCFaces

!          FP faces
           ic2 = Z%Connect(1,id,c)

           if ( ic2 /= 0) then

             zoneBreak = Geom%CToZone(ic2)

             if ( onstack(zoneBreak) ) then
               do i=1,nDSZ(zoneBreak)
                 if (DownStreamZ(i,zoneBreak) == lowlinkZ ) then
                   DownStreamZ(i,zoneBreak) = nzones + 1 
                   MeshCycles               = MeshCycles + 1
                   listcycles(MeshCycles)   = lowlinkZ
                 endif
               enddo
             endif

           endif

         enddo

       enddo

!  Reset the stack

       do i=1,cyclesize
         onstack( tempList(i) ) = .FALSE.
       enddo

     endif DetectCycle

   endif CheckCycle



   return
   end subroutine sccsearch 
 
