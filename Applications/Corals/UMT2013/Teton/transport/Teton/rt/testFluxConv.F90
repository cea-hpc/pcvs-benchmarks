!***********************************************************************
!                        Version 1:  03/2009, PFN                      *
!                                                                      *
!   testFluxConv - Monitors convergence of the incident current on     *
!                  shared boundaries.                                  *
!                                                                      *
!***********************************************************************
   subroutine testFluxConv(FluxConverged, fluxIter, maxFluxError) 

   use kind_mod
   use constant_mod
   use Size_mod
   use mpi_param_mod
   use mpif90_mod
   use Quadrature_mod
   use iter_control_list_mod
   use iter_control_mod

   implicit none

!  Arguments

   logical (kind=1), intent(inout) :: FluxConverged
   integer,          intent(in)    :: fluxIter
   real(adqt),       intent(inout) :: maxFluxError

!  Local

   integer    :: bin
   integer    :: nConv

   real(adqt) :: fluxTolerance
   real(adqt) :: tempError
   real(adqt) :: minTolerance
   real(adqt) :: totalFlux
   real(adqt) :: weight
   real(adqt) :: threshold
   real(adqt) :: phiError
   real(adqt) :: maxError(QuadSet% NumBin0)

!  Constants

   parameter (minTolerance=0.01d0)
   parameter (threshold=0.001d0)

   incidentFluxControl => getIterationControl(IterControls,"incidentFlux")
   temperatureControl  => getIterationControl(IterControls,"temperature")
   intensityControl    => getIterationControl(IterControls,"intensity")

!  Use the error in the nonlinear iteration as a guide for
!  converging the incident flux iteration

   tempError     = getGlobalError(temperatureControl)
   phiError      = getGlobalError(intensityControl)
   fluxTolerance = getEpsilonPoint(incidentFluxControl)

   fluxTolerance = max(fluxTolerance, tempError/twenty)
   fluxTolerance = min(fluxTolerance, minTolerance)

!  Compute errors

   totalFlux = zero
   do bin=1,QuadSet% NumBin0
     totalFlux = totalFlux + QuadSet% IncFlux(bin)
   enddo

   do bin=1,QuadSet% NumBin0
     weight = QuadSet% IncFlux(bin)/totalFlux
!     if (QuadSet% IncFlux(bin) > zero) then
     if (weight > threshold) then
       QuadSet% relError(bin) = abs( (QuadSet% IncFlux(bin)  -      &
                                      QuadSet% IncFluxOld(bin)) )/  &
                                      QuadSet% IncFlux(bin)
     else
       QuadSet% relError(bin) = zero
     endif

   enddo

!  Find largest error among all processes

   maxError(:) = QuadSet% relError(:)
   call MPIAllReduceT(maxError, "max", MPI_COMM_WORLD)

   maxFluxError = maxval( maxError(1:QuadSet% NumBin0) )

   nConv = 0
   do bin=1,QuadSet% NumBin0
     if (maxError(bin) <= fluxTolerance) then
       QuadSet% Converged(bin) = .TRUE.
       nConv                   = nConv + 1
     endif
   enddo

   if ( nConv == QuadSet% NumBin0 .or.  &
        fluxIter >= getMaxNumberOfIterations(incidentFluxControl) ) then
     FluxConverged = .TRUE.
   endif

!   if (Size%my_node == 0) then
!     write(6,100) fluxIter,QuadSet%NumBin,maxFluxError,fluxTolerance,tempError,phiError
!   endif

! 100 format("fluxIter = ",i2,2x,"NumBin = ",i3,2x,"maxFluxError = ",1pe11.3,2x,  &
!            "tolerance = ",1pe11.3,2x,"tempError = ",1pe11.3,2x,"phiError = ",1pe11.3)


 
   return
   end subroutine testFluxConv 

