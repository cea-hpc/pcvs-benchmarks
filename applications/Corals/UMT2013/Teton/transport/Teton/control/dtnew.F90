!***********************************************************************
!                        Version 0:  01/92, PFN                        *
!                                                                      *
!   DTNEW - Calculates a new time step based on the maximum changes in *
!           the electron and radiation temperatures.                   *
!                                                                      *
!   Input:  tez,tezn - current and previous time step zone average     *
!                      electron temperature                            *
!           trz,trzn - current and previous time step zone average     *
!                      radiation temperature                           *
!                                                                      *
!   Output: DTRAD    - time step for next radiation cycle              *
!           IZDTEX   - zone with the maximum change in electron temp   *
!           IZDTRX   - zone with the maximum change in (Tr)**4         *
!                                                                      *
!***********************************************************************
   subroutine dtnew


   use kind_mod
   use mpi_param_mod
   use mpif90_mod
   use constant_mod
   use iter_control_list_mod
   use iter_control_mod
   use TimeStepControls_mod
   use Size_mod
   use Geometry_mod
   use Material_mod
   use ZoneData_mod

   implicit none

!  Local

   integer, dimension (1)     :: iztr4max

   integer    :: zone,izdtex,izdtr4x,noutrt,noutmx,ninrt,ninmx,noutclose
   integer    :: nodeID,my_node

   real(adqt) :: dtemax,dtr4max,facdte,facdtr4,dterec,        &
                 dtr4rec,dtiter,dtrec,tmin,tez,tezn,          &
                 tr,trn,tr4,tr4n,dtclose,delta_te,delta_tr4,  &
                 delte,deltr4,dtrad,dtrmx,dtrmn,my_change,    &
                 change,threshold,trmax,tr4max

   character(len=8) :: constraint

!  Constants

   my_node = Size%my_node
!   tmin    = Size%tmin
   tmin    = 0.008d0

!  Iteration Controls

   temperatureControl => getIterationControl(IterControls,"temperature")
   intensityControl   => getIterationControl(IterControls,"intensity")

!  We test on the radiation energy density and electron temperature

   izdtex  = 1 
   izdtr4x = 1 
   dtemax  = zero
   dtr4max = zero

   iztr4max  = maxloc( Mat%trz(:) )
   trmax     = max( Mat%trz(iztr4max(1)), tmin )
   tr4max    = trmax*trmax*trmax*trmax

   Z => getZoneData(Geom, iztr4max(1))
   threshold = cutoff*tr4max*Z%VolumeZone

   call MPIAllReduceT(THRESHOLD, "max", MPI_COMM_WORLD)

   ZoneLoop: do zone=1,Size%nzones

     Z    => getZoneData(Geom, zone)

     tr   = Mat%trz(zone)
     trn  = Mat%trzn(zone)
     tr4  = tr*tr*tr*tr
     tr4n = trn*trn*trn*trn 

     tez  = Mat%tez(zone)
     tezn = Mat%tezn(zone)

     if (tr4*Z%VolumeZone > threshold) then
       delta_tr4 = abs(tr4 - tr4n)/tr4n

       if (delta_tr4 > dtr4max) then
         izdtr4x = zone 
         dtr4max = delta_tr4
       endif
     endif

     if (tez > tmin .and. tezn > tmin) then
       delta_te  = abs(tez - tezn)/tezn

       if (delta_te > dtemax) then
         izdtex = zone 
         dtemax = delta_te
       endif
     endif

   enddo ZoneLoop

!  What's controlling the time step

   constraint = 'none'

   if (dtr4max > dtemax) then
     constraint = 'RadTemp'
   elseif (dtr4max < dtemax) then
     constraint = 'ElecTemp'
   endif

!  Time step can decrease by only a factor 2 per cycle

   delte   = getMaxChangeTe(DtControls)
   deltr4  = getMaxChangeTr4(DtControls)
   dtrad   = getRadTimeStep(DtControls)

   facdte  = min(two,dtemax/delte)
   facdtr4 = min(two,dtr4max/deltr4)

!  Time step can increase by only a factor 1/TempFraction per cycle

   dterec  = dtrad/max(TempFraction,facdte)
   dtr4rec = dtrad/max(TempFraction,facdtr4)

!  If the iteration count is "close" to the maximum allowed,
!  do not increase the time step further.  If we are converging
!  the nonlinear iteration every timestep (noutmx > 1) then
!  just check the number of temperature iterations taken.  If
!  we are running in linear semi-implicit mode, check the number
!  of intensity iterations.

   dtclose = two*dtrad

   noutmx = getMaxNumberOfIterations(temperatureControl)
   noutrt = getNumberOfIterations(temperatureControl)

   ninmx = getMaxNumberOfIterations(intensityControl)
   ninrt = getNumberOfIterations(intensityControl)

   noutclose = IterFraction*noutmx

   if (noutmx > 1) then

     noutclose = IterFraction*noutmx
     if (noutrt >= noutclose) then
       dtclose    = dtrad
       constraint = 'SlowConv'
     endif

   else

     noutclose = IterFraction*ninmx
     if (ninrt >= noutclose) then
       dtclose    = dtrad
       constraint = 'SlowConv'
     endif

   endif

!  If the iteration did not converge cut the timestep by half

   dtiter = two*dtrad

   if (noutmx > 1) then
     if (noutrt >= noutmx) then
       dtiter     = IterFraction*dtrad
       constraint = 'NoConv'
     endif
   else
     if (ninrt >= ninmx) then
       dtiter     = IterFraction*dtrad
       constraint = 'NoConv'
     endif
   endif

   dtrec  = min(dterec,dtr4rec,dtiter,dtclose)

   dtrmn  = getMinTimeStep(DtControls)
   dtrmx  = getMaxTimeStep(DtControls)

   dtrad  = min(dtrec,dtrmx)
   dtrad  = max(dtrad,dtrmn)

!  Choose the minimum time step over all domains

   call MPIAllReduceT(DTRAD, "min", MPI_COMM_WORLD)

!  Use the maximum change in Tr4 or Te to decide which node
!  control the time step

   change    = max(dtr4max,dtemax)
   my_change = change

   call MPIAllReduceT(CHANGE, "max", MPI_COMM_WORLD)

   nodeID = -1
   if (my_change == change) then
     nodeID = my_node
   endif
                                                                                       
   call MPIAllReduceT(nodeID, "max", MPI_COMM_WORLD)

!  Update controls

   call setDtControls(DtControls,               &
                      ControlProcess=nodeID,    &
                      ZoneMaxChangeTr4=izdtr4x, &
                      ZoneMaxChangeTe=izdtex,   &
                      RadTimeStep=dtrad,        &
                      RecTimeStep=dtrad,        &
                      MaxFracChangeTr4=dtr4max, &
                      MaxFracChangeTe=dtemax,   &
                      Tr4Threshold=threshold,   &
                      DtConstraint=constraint   )


   return
   end subroutine dtnew


