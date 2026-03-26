!***********************************************************************
!                        Version 1:  09/96, PFN                        *
!                                                                      *
!   SNNEXT - This routine builds the sweep ordering array NEXT for a   *
!            single direction.                                         *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!                                                                      *
!***********************************************************************
   subroutine snnext(m) 

   use kind_mod
   use Size_mod
   use Geometry_mod
   use Quadrature_mod
   use ZoneData_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: m

!  Local Variables

   integer    :: i,ic,Cexit,Zexit,ndone,nnext,nlast,   &
                 nseed,newZone,lastZone,nextseed,nextZone, &
                 MeshCycles,ncornr,maxcf,nzones

   integer    :: hotZone,distance1,distanceMin,zone
   integer    :: ii,index,cID,corner1,zone1,ndoneZ,c,c0,nCorner

   integer    :: need(Size%maxCorner)
   integer    :: DownStreamC(Size%maxcf,Size%maxCorner)
   integer    :: nDSC(Size%maxCorner)

   real(adqt) :: omega(Size%ndim)

   logical (kind=1) :: newSeed
   logical (kind=1) :: doneC(Size% maxCorner)

!  Dynamic

   integer, allocatable :: DownStreamZ(:,:)
   integer, allocatable :: nDSZ(:)
   integer, allocatable :: needZ(:)
   integer, allocatable :: listZone(:)
   integer, allocatable :: zoneSeed(:)
   integer, allocatable :: list_in_zone(:)

   logical (kind=1), allocatable :: doneZ(:)

!  Constants

   ncornr   = Size% ncornr
   nzones   = Size% nzones
   maxcf    = Size% maxcf
   omega(:) = QuadSet% Omega(:,m)

!  Allocate arrays

   allocate( DownStreamZ(2*maxcf,nzones) )
   allocate( nDSZ(nzones) )
   allocate( needZ(nzones+1) )
   allocate( listZone(nzones) )
   allocate( zoneSeed(nzones) )
   allocate( list_in_zone(Size% maxCorner) )
   allocate( doneZ(nzones+1) )

   doneZ(:)        = .FALSE.
   doneZ(nzones+1) = .TRUE.

!  Build NEED array by computing Outward_Normal dot Omega(m)

   call snneed(omega, NEEDZ, DOWNSTREAMZ, nDSZ, MESHCYCLES) 

   QuadSet% totcycles = QuadSet% totcycles + MeshCycles

!  Create a list of zone "seeds"

   call findseeds(NSEED, MESHCYCLES, needZ, ZONESEED) 

   QuadSet% totcycles = QuadSet% totcycles + MeshCycles

!  Create the "next" array. 

   nextseed = 1
   ndone    = 0
   ndoneZ   = 0
   nextZone = 0
   lastZone = 0
   newZone  = zoneSeed(1)


   OuterIteration: do

!  Work to complete the zone 

     zone = newZone

     call getDownStreamData(zone, omega, NEED, DOWNSTREAMC, nDSC)

!  Find the corners in this zone with need=0

     Z       => getZoneData(Geom, zone)
     nCorner = Z% nCorner
     c0      = Z% c0

     nlast = 0
     do c=1,nCorner
       if (need(c) == 0) then
         nlast               = nlast + 1
         list_in_zone(nlast) = c 
       endif
     enddo

!  If we cannot start the zone we have a circular dependency within the zone

     if (nlast == 0) then
       call fixZone(zone, nlast, need, nDSC, list_in_zone)
     endif

     nnext                    = 0
     ndoneZ                   = ndoneZ + 1
     QuadSet% nextZ(ndoneZ,m) = zone
     doneZ(zone)              = .TRUE.
     doneC(:)                 = .FALSE.

     ZoneIteration: do

       nnext                        = nnext + 1
       c                            = list_in_zone(nnext)
       QuadSet% next(ndone+nnext,m) = c0 + c
       doneC(c)                     = .TRUE.

!  Decrement the NEED array for corners on exiting faces

!  Exit "EZ" faces 

       do i=1,nDSC(c)

         Cexit = DownStreamC(i,c)

         if ( .not. doneC(Cexit) ) then

           need(Cexit) = need(Cexit) - 1

           if (need(Cexit) == 0) then
             nlast               = nlast + 1
             list_in_zone(nlast) = Cexit 
             doneC(Cexit)        = .TRUE.
           elseif (need(Cexit) < 0) then
             call f90fatal("needC < 0 in SNNEXT!")
           endif

         endif

       enddo

       if (nnext == nlast) then
         if (nnext == nCorner) then
           exit ZoneIteration
         else
           call fixZone(zone, nlast, need, nDSC, list_in_zone)
           cycle ZoneIteration
         endif
       else
         cycle ZoneIteration
       endif

     enddo ZoneIteration

     hotZone = zone 

!  Loop over the down-stream zones for the zone just added
!  to the nextZ list, decrementing the needZ array for these
!  neighboring zones 

     do i=1,nDSZ(zone)

       Zexit = DownStreamZ(i,zone)

       if ( .not. doneZ(Zexit) ) then

         needZ(Zexit) = needZ(Zexit) - 1

         if (needZ(Zexit) == 0) then
           lastZone           = lastZone + 1
           listZone(lastZone) = Zexit 
         elseif (needZ(Zexit) < 0) then
           write(6,100) Zexit, zone
           call f90fatal("needZ < 0 in SNNEXT!")
         endif

       endif

     enddo

 100 format('zone ',i6,' has already been done and is down stream of ',i6)

!  Exit if done or pick a new zone using the following priority:

!    1. Pick the next zone in "listZone"
!    2. Pick the next seed in "zoneSeed"
!    3. Break a cycle to create a new zone seed 

     ndone = ndone + nnext

     if (ndone == ncornr) then

       exit OuterIteration

     else

       newSeed = .FALSE.

       if (nextZone < lastZone) then

         distanceMin = nzones
         do ii=nextZone+1,lastZone
           zone1 = listZone(ii)
           if ( needZ(zone1) == 0 ) then
             distance1 = abs( hotZone - zone1 )
             if (distance1 < distanceMin) then
               index       = ii
               distanceMin = distance1 
             endif
           endif
         enddo

         if (distanceMin /= nzones) then
           newZone         = listZone(index)
           listZone(index) = listZone(lastZone)
           lastZone        = lastZone - 1
         else
           newSeed = .TRUE.
         endif

       else
         nextZone = 0
         lastZone = 0
         newSeed    = .TRUE.
       endif

!  Pick the next seed or break a cycle to create one

       if (newSeed) then
         if (nextseed < nseed) then
           nextseed  = nextseed + 1
           newZone   = zoneSeed(nextseed)
         else
           call cyclebreaker(ndoneZ, MESHCYCLES, NSEED,        &
                             needZ, DownStreamZ, nDSZ, ZONESEED)

           QuadSet% totcycles = QuadSet% totcycles + MeshCycles
           nextseed           = nextseed + 1
           newZone            = zoneSeed(nextseed)
         endif
       endif

       cycle OuterIteration

     endif

!  End of Outer Loop

   enddo OuterIteration


!  Set the extra element of NEXT to 1 (in the sweep routines we
!  need to reference the ncornr+1 entry, but it is not used)

   QuadSet% next(ncornr+1,m) = 1
 
!  Final error check

   if (ndone /= ncornr) then
     call f90fatal("Wrong number of corners in SNNEXT!")
   endif

   if (ndoneZ /= nzones) then
     call f90fatal("Wrong number of zones in SNNEXT!")
   endif

!  Release memory

   deallocate( DownStreamZ )
   deallocate( nDSZ )
   deallocate( needZ )
   deallocate( listZone )
   deallocate( zoneSeed )
   deallocate( list_in_zone )
   deallocate( doneZ )

 
   return
   end subroutine snnext

