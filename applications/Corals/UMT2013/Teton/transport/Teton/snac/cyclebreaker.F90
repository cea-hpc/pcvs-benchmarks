!***********************************************************************
!                        Version 1:  07/01, PFN                        *
!                                                                      *
!   CYCLEBREAKER - This routine breaks cycles in the mesh by selecting *
!                  a corner that will use some old (i.e. previous      *
!                  iterate) incident fluxes.                           *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!                                                                      *
!***********************************************************************
   subroutine cyclebreaker(ndoneZ, MESHCYCLES, NSEED,   &
                           needZ, DownStreamZ, nDSZ, ZONESEED)

   use kind_mod
   use constant_mod
   use Size_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: ndoneZ

   integer,    intent(inout) :: meshcycles,nseed 

   integer,    intent(in)    :: DownStreamZ(2*Size%maxcf,Size%nzones) 
   integer,    intent(in)    :: nDSZ(Size%nzones)

   integer,    intent(inout) :: needZ(Size%nzones+1)
   integer,    intent(inout) :: zoneSeed(Size%nzones)

!  Local Variables

   integer :: i,ic,icds,ngraph,n,nleft,count,stackindex,nzones,zone

!  Dynamic

   integer, allocatable :: listZ(:)
   integer, allocatable :: listcycles(:)
   integer, allocatable :: dfnum(:)
   integer, allocatable :: lowlink(:)
   integer, allocatable :: stack(:)
   integer, allocatable :: tempList(:)

   logical (kind=1), allocatable :: new(:)
   logical (kind=1), allocatable :: onstack(:)

!  Mesh Constants

   nzones = Size%nzones

!  Allocate arrays for the number of zones in the graph (= nzones - ndone)

   ngraph = nzones - ndoneZ

   allocate( listZ(ngraph) )
   allocate( listcycles(ngraph) )
   allocate( dfnum(nzones) )
   allocate( lowlink(nzones) )
   allocate( stack(ngraph) )
   allocate( tempList(ngraph) )

   allocate( new(nzones) )
   allocate( onstack(nzones) )

!  Initialize arrays and counters

   new(:)     = .TRUE. 
   onstack(:) = .FALSE. 

   MeshCycles = 0
   count      = 0
   stackindex = 0

   stack(:)   = 0

!  Make a list of all remaining zones 

   nleft = 0

   do zone=1,nzones 
     if (needZ(zone) == 0) then
       new(zone)    = .FALSE. 
     else
       nleft        = nleft + 1
       listZ(nleft) = zone 
     endif
   enddo

   if (nleft /= ngraph) then
     call f90fatal("Miscount of remaining zones in CYCLEBREAKER")
   endif

!  Loop over the number of zones in the graph

   do i=1,ngraph

     zone = listZ(i)

     if ( new(zone) ) then

       call sccsearch(zone,ngraph,count,          &
                      stackindex,MESHCYCLES,      &
                      DownStreamZ,nDSZ,dfnum,     &
                      lowlink,stack,new,onstack,  &
                      tempList,LISTCYCLES)

     endif

   enddo


   if (MeshCycles == 0) then 

     call f90fatal("Cycle detection failed in CYCLEBREAKER!")

   else

     n = 0
     do i=1,MeshCycles
       zone        = listcycles(i)
       needZ(zone) = needZ(zone) - 1

       if (needZ(zone) == 0) then
         n = n + 1
         zoneSeed(nseed+n) = zone
       endif
     enddo

     nseed = nseed + n

     if (n == 0) then
       call f90fatal("Cycles found, but not broken")
     endif

   endif

!  Release memory

   deallocate( listZ )
   deallocate( listcycles )
   deallocate( dfnum )
   deallocate( lowlink )
   deallocate( stack )
   deallocate( tempList )
   deallocate( new )
   deallocate( onstack )


   return
   end subroutine cyclebreaker
 
