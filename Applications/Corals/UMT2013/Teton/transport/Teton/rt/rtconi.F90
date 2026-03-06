!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTCONI - Checks convergence by finding the maximum relative error  *
!            between two vectors.  Used for inner (group) iteration.   *
!                                                                      *
!   Input:   phir   - new scalar intensity      (n+1)                  *
!            phiold - previous scalar intensity (n)                    *
!                                                                      *
!   Output:  ERPHMX - maximum relative error in zonal energy density   *
!                                                                      *
!   Local:   relerr - relative errors                                  *
!                                                                      *
!*********************************************************************** 
   subroutine rtconi(maxEnergyDensityError, Phi)

   use kind_mod
   use mpi_param_mod
   use mpif90_mod
   use constant_mod
   use iter_control_list_mod
   use iter_control_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(inout) :: maxEnergyDensityError 

   real(adqt), intent(in)    :: Phi(Size%ngr,Size%ncornr) 

!  Local

   integer, dimension(1) :: zoneEnergyMax 

   integer    :: my_node, ig, zoneMaxError, nodeID, nzones
   integer    :: c, c0, nCorner, zone, ngr, bdyTest

   real(adqt) :: relerr,cutoff,threshold,energy,EnergyMax,sum

!  Dynamic Arrays

   real(adqt),  allocatable :: EnergyDensity(:)

!  Constants

   parameter (cutoff=1.0d-6)

   nzones  = Size%nzones
   ngr     = Size%ngr
   my_node = Size%my_node
 
!  Compute the total energy density in a zone (SOLZ)

   allocate( EnergyDensity(nzones) )

!  Iteration Controls

   intensityControl => getIterationControl(IterControls,"intensity")

!  Find the zone-average energy density 

   do zone=1,nzones
     Z => getZoneData(Geom, zone)

     nCorner             = Z% nCorner
     c0                  = Z% c0
     EnergyDensity(zone) = zero

     do c=1,nCorner
       sum = zero
       do ig=1,ngr
         sum = sum + Phi(ig,c0+c)
       enddo
       EnergyDensity(zone) = EnergyDensity(zone) + Z%Volume(c)*sum
     enddo

     EnergyDensity(zone) = EnergyDensity(zone)/Z%VolumeZone

   enddo

!  Find an energy threshold for convergence tests

   zoneEnergyMax  = maxloc( EnergyDensity(:) )
   EnergyMax      = EnergyDensity(zoneEnergyMax(1))

   call MPIAllReduceT(EnergyMax, "max", MPI_COMM_WORLD)

   threshold = cutoff*EnergyMax

!  Compute relative errors in the total energy density in
!  a zone; eliminate zones from consideration if their zonal
!  energy is less than a threshold 

   zoneMaxError          = 0
   maxEnergyDensityError = zero
 
   do zone=1,nzones

     Z => getZoneData(Geom, zone)

     energy = EnergyDensity(zone)

     if (energy > threshold) then

       relerr = abs( (EnergyDensity(zone) - Z% EnergyDensityOld)/EnergyDensity(zone) )

       if (relerr > maxEnergyDensityError) then

         zoneMaxError          = zone 
         maxEnergyDensityError = relerr

       endif

     endif

     Z% EnergyDensityOld = EnergyDensity(zone)

   enddo

!  If spatial decomposition is on, find the largest error in the
!  entire mesh

   relerr = maxEnergyDensityError 
   call MPIAllReduceT(maxEnergyDensityError, "max", MPI_COMM_WORLD)

   nodeID = -1
   if (relerr == maxEnergyDensityError) then
     nodeID = my_node
   endif

   call MPIAllReduceT(nodeID, "max", MPI_COMM_WORLD)

   call setLocalError(intensityControl,relerr)
   call setGlobalError(intensityControl,maxEnergyDensityError)
   call setZoneOfMax(intensityControl,zoneMaxError)
   call setProcessOfMax(intensityControl,nodeID)
 

!  Release Memory

   deallocate( EnergyDensity )
 

   return
   end subroutine rtconi

