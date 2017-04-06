!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTCONV - Checks convergence by finding the maximum relative error  *
!            between two vectors.  Used for temperature iteration.     *
!                                                                      *
!   Input:   xnew   - new solution      (n+1)                          *
!            xold   - previous solution (n)                            *
!                                                                      *
!   Output:  errmax - maximum relative error over all zones            *
!                                                                      *
!   Local:   relerr - relative errors                                  *
!                                                                      *
!***********************************************************************
 
   subroutine rtconv(maxTempError)

   use kind_mod
   use mpi_param_mod
   use mpif90_mod
   use constant_mod
   use iter_control_list_mod
   use iter_control_mod
   use Size_mod
   use Geometry_mod
   use Material_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(inout) :: maxTempError 

!  Local Variables

   integer    :: my_node, zone, zoneMaxError, nodeID, nzones

   real(adqt) :: relerr, AveTemp, sumTempVol, sumVol, threshold

!  Dynamic Arrays

   real(adqt),  allocatable :: teznew(:)

!  Mesh Constants

   nzones  = Size%nzones
   my_node = Size%my_node

!  Iteration Controls
    
   temperatureControl => getIterationControl(IterControls,"temperature")

!  Compute zone average temperature

   allocate( teznew(nzones) )
 
   call rtave(Mat%tec, TEZNEW)

!  Compute a threshold temperature for checking convergence

   sumTempVol = zero
   sumVol     = zero

   do zone=1,nzones
     Z => getZoneData(Geom, zone)
     sumTempVol = sumTempVol + Z%VolumeZone*teznew(zone)
     sumVol     = sumVol     + Z%VolumeZone
   enddo

   call MPIAllReduceT(sumTempVol, "sum", MPI_COMM_WORLD)
   call MPIAllReduceT(sumVol,     "sum", MPI_COMM_WORLD)

   AveTemp = sumTempVol/sumVol

   threshold = AveTemp/twenty     
 
!  Find maximum relative error in the zonal temperature
 
   zoneMaxError = 0
   maxTempError = zero

   do zone=1,nzones

     if (teznew(zone) > threshold) then

       relerr = abs(one - Mat%tezold(zone)/teznew(zone))

       if (relerr > maxTempError) then

         zoneMaxError = zone 
         maxTempError = relerr

       endif

     endif

   enddo

!  If spatial decomposition is on, find the largest error in the
!  entire mesh

   relerr = maxTempError 
   call MPIAllReduceT(maxTempError, "max", MPI_COMM_WORLD)

   nodeID = -1
   if (relerr == maxTempError) then
     nodeID = my_node
   endif
                                                                                   
   call MPIAllReduceT(nodeID, "max", MPI_COMM_WORLD)

   call setLocalError(temperatureControl,relerr)
   call setGlobalError(temperatureControl,maxTempError)
   call setZoneOfMax(temperatureControl,zoneMaxError)
   call setProcessOfMax(temperatureControl,nodeID)

!  Save latest solution in TEZOLD for next iteration
 
   Mat%tezold(:) = teznew(:)

!  Release Memory

   deallocate( teznew )

 
   return
   end subroutine rtconv

