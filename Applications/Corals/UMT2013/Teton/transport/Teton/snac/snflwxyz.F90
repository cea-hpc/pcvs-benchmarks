!***********************************************************************
!                        Version 1:  09/96, PFN                        *
!                                                                      *
!   SNFLWXYZ - This routine, called by RSWPMD and RTACCELMD, solves    *
!              the fixed-source transport problem on an arbitrary      *
!              grid in either xyz-geometry or rz-geometry.             *
!              An upstream corner-balance spatial discretization is    *
!              used.                                                   *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!                                                                      *
!***********************************************************************

   subroutine snflwxyz(ipath, PSIB, PSI, PHI, angleLoopTime)


   use kind_mod
   use constant_mod
   use Size_mod
   use Quadrature_mod

#include "assert.h"

   implicit none
   include 'mpif.h'

!  Arguments

   real(adqt), intent(inout) :: psib(QuadSet%Groups,Size%nbelem,QuadSet%NumAngles)
   real(adqt), intent(inout) :: psi(QuadSet%Groups,Size%ncornr,QuadSet%NumAngles)
   real(adqt), intent(inout) :: Phi(QuadSet%Groups,Size%ncornr),angleLoopTime

   character(len=8), intent(in) :: ipath

!  Local

   integer          :: Angle, mm, thnum, n_cpuL
   integer          :: Groups, fluxIter, ishared
   integer          :: binSend, binRecv, NangBin

   logical (kind=1) :: FluxConverged

   real(adqt)       :: maxFluxError
   real(adqt)       :: startOMPLoopTime, endOMPLoopTime, theOMPLoopTime

!  Function

   integer :: OMP_GET_THREAD_NUM, OMP_GET_MAX_THREADS

!  Set number of threads

!  n_cpuL = 1
   n_cpuL = OMP_GET_MAX_THREADS()
   theOMPLoopTime=0.0

   require(n_cpuL>0,   "Invalid Thread Count")
   require(n_cpuL<=32, "Invalid Thread Count") 

!  Mesh Constants

   Groups = QuadSet%Groups

!  Loop over angle bins

   if (ipath == 'sweep') then
     call setIncidentFlux(psib)
   endif
                                                                                         
   FluxConverged = .FALSE.
   fluxIter      =  0

   call restoreCommOrder(QuadSet)


   FluxIteration: do

!    Post receives for all data
                                                                                                  
     call InitExchange

     fluxIter = fluxIter + 1

     AngleBin: do binRecv=1,QuadSet% NumBin
       binSend = QuadSet% SendOrder(binRecv)
       NangBin = QuadSet% NangBinList(binSend)
       
!

!    Loop over angles, solving for each in turn:
     startOMPLoopTime = MPI_WTIME()

!
!$OMP PARALLEL DO  PRIVATE(Angle,mm,thnum)
       AngleLoop: do mm=1,NangBin

         Angle = QuadSet% AngleOrder(mm,binSend)
!        thnum = 1
         thnum = OMP_GET_THREAD_NUM() + 1 

!        Set angular fluxes for reflected angles

         call snreflect(Angle, PSIB)

!        Sweep the mesh, calculating PSI for each corner; the 
!        boundary flux array PSIB is also updated here. 
!        Mesh cycles are fixed automatically.

         call snswp3d(Groups, Angle,                                   &
                      QuadSet%next(1,Angle),QuadSet%nextZ(1,Angle),    &
                      PSI(1,1,Angle),PSIB(1,1,Angle))

       enddo AngleLoop
     endOMPLoopTime = MPI_WTIME()
     theOMPLoopTime = theOMPLoopTime + (endOMPLoopTime-startOMPLoopTime)

!      Exchange Boundary Fluxes

       call exchange(PSIB, binSend, binRecv) 

     enddo AngleBin

     if (ipath == 'sweep') then
       call setIncidentFlux(psib)
       call testFluxConv(FluxConverged, fluxIter, maxFluxError)
     else
       FluxConverged = .TRUE.
     endif

     if ( FluxConverged ) then
       exit FluxIteration
     else
       call setCommOrder(QuadSet)
       cycle FluxIteration
     endif

   enddo FluxIteration

!  Update the scaler flux 

   if (ipath == 'sweep') then
     call snmoments(psi, PHI)
     call restoreCommOrder(QuadSet)
   endif

   angleLoopTime = angleLoopTime + theOMPLoopTime


   return
   end subroutine snflwxyz


