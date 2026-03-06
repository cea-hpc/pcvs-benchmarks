!***********************************************************************
!                        Version 1:  07/08, PFN                        *
!                                                                      *
!   FIXZONE      - This routine fixes a cycle within a zone.           *
!                                                                      *
!***********************************************************************
   subroutine fixZone(zone, nlast, need, nDSC, list_in_zone)

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: zone
   integer,    intent(inout) :: nlast 

   integer,    intent(inout) :: need(Size%maxCorner)
   integer,    intent(in)    :: nDSC(Size%maxCorner)
   integer,    intent(inout) :: list_in_zone(Size%maxCorner) 

!  Local Variables

   integer    :: cID, minNeed, maxDSC
   integer    :: c, c0, nCorner

!  If we cannot start or complete a zone, pick the corner with the smallest
!  value of need and the largest number of downstream neighbors 

   minNeed  = 100
   maxDSC   = 0

   Z => getZoneData(Geom, zone)

   nCorner = Z% nCorner
   c0      = Z% c0

   CornerLoop: do c=1,nCorner
     if (need(c) /= 0) then
       if ( (need(c) <= minNeed) .and. (nDSC(c) > maxDSC) ) then
         cID     = c
         minNeed = need(c)
         maxDSC  = nDSC(c)
       endif
     endif
   enddo CornerLoop

   need(cID)           = 0
   nlast               = nlast + 1
   list_in_zone(nlast) = cID



   return
   end subroutine fixZone 

