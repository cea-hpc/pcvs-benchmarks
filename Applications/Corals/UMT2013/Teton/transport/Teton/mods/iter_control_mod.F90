module iter_control_mod

#include "assert.h"
      use kind_mod
      use constant_mod
      use io_mod

!=======================================================================
!                       Version 1.0: 03/99, MRZ
!-----------------------------------------------------------------------
! Iteration Control
!   This class contains an iteration control entry
!
! epsilonPoint    pointwise relative error convergence criterion
! maxIter         maximum number of iterations
! nIter           number of iterations to reach convergence
! nTotIter        total number of iterations over 
! zoneOfMax       zone number of maximum relative pointwise error
!-----------------------------------------------------------------------
! v1.0: Original implementation
!=======================================================================

private

! public interfaces
  public construct, setControls, resetNumberOfIterations,      &
         setNumberOfIterations, setZoneOfMax, setProcessOfMax, &
         setLocalError, setGlobalError, destruct,              &
         getEpsilonPoint, getLocalError, getGlobalError,       &
         getMaxNumberOfIterations, getNumberOfIterations,      &
         getTotalNumberOfIterations, getZoneOfMax,             &
         getProcessOfMax

  type, public :: IterControl
    private
    real(adqt)    :: epsilonPoint
    real(adqt)    :: localError
    real(adqt)    :: globalError
    integer       :: maxIter
    integer       :: nIter
    integer       :: nTotIter
    integer       :: zoneOfMax
    integer       :: processOfMax
  end type IterControl

  type(IterControl), pointer, public :: temperatureControl
  type(IterControl), pointer, public :: intensityControl
  type(IterControl), pointer, public :: scatteringControl
  type(IterControl), pointer, public :: greyControl
  type(IterControl), pointer, public :: incidentFluxControl

  interface construct
    module procedure iter_control_ctor
  end interface

  interface setControls
    module procedure iter_control_set
  end interface

  interface resetNumberOfIterations
    module procedure iter_control_reset_nIter
  end interface

  interface setNumberOfIterations
    module procedure iter_control_set_nIter
  end interface

  interface setZoneOfMax
    module procedure iter_control_set_zoneOfMax
  end interface

  interface setProcessOfMax
    module procedure iter_control_set_processOfMax
  end interface

  interface setLocalError
    module procedure iter_control_set_localError
  end interface

  interface setGlobalError
    module procedure iter_control_set_globalError
  end interface

  interface destruct
    module procedure iter_control_dtor
  end interface

  interface getEpsilonPoint
    module procedure iter_control_get_epsilonPoint
  end interface

  interface getMaxNumberOfIterations
    module procedure iter_control_get_maxIter
  end interface

  interface getNumberOfIterations
    module procedure iter_control_get_nIter
  end interface

  interface getTotalNumberOfIterations
    module procedure iter_control_get_nTotIter
  end interface

  interface getZoneOfMax
    module procedure iter_control_get_zoneOfMax
  end interface

  interface getProcessOfMax
    module procedure iter_control_get_ProcessOfMax
  end interface

  interface getLocalError
    module procedure iter_control_get_localError
  end interface

  interface getGlobalError
    module procedure iter_control_get_globalError
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================

  subroutine iter_control_ctor(self,epsilonPoint,localError, &
    globalError,maxNumberOfIterations,numberOfIterations, &
    totalNumberOfIterations,zoneOfMaximum,processOfMaximum)

!    Construct the iteration control object
!      epsilonPoint              rel. point error convergence criterion
!      localError                maximum pointwise relative error for process
!      globalError               global maximum pointwise relative error
!      maxNumberOfIterations     maximum number of iterations
!      numberOfIterations        number of iterations
!      totalNumberOfIterations   total number of iterations
!      zoneOfMaximum             zone of maximum pointwise error
!      processOfMaximum          process with the maximum pointwise error

!    variable declarations
     implicit none

!    passed variables
     type(IterControl),    intent(inout) :: self
     real(adqt), optional, intent(in)    :: epsilonPoint
     real(adqt), optional, intent(in)    :: localError
     real(adqt), optional, intent(in)    :: globalError
     integer,    optional, intent(in)    :: maxNumberOfIterations
     integer,    optional, intent(in)    :: numberOfIterations
     integer,    optional, intent(in)    :: totalNumberOfIterations
     integer,    optional, intent(in)    :: zoneOfMaximum
     integer,    optional, intent(in)    :: processOfMaximum

!    construct the iteration control object

     if (present(epsilonPoint)) then
        self % epsilonPoint = epsilonPoint
     else
        self % epsilonPoint = 1.0e-4_adqt
     endif

     self % localError = 0.0_adqt
     self % globalError = 0.0_adqt

     if (present(maxNumberOfIterations)) then
        self % maxIter = maxNumberOfIterations
     else
        self % maxIter = 10
     endif

     if (present(numberOfIterations)) then
        self % nIter = numberOfIterations
     else
        self % nIter = 0
     endif

     if (present(totalNumberOfIterations)) then
        self % nTotIter = totalNumberOfIterations
     else
        self % nTotIter = 0
     endif

     if (present(zoneOfMaximum)) then
        self % zoneOfMax = zoneOfMaximum
     else
        self % zoneOfMax = 0
     endif

     if (present(processOfMaximum)) then
        self % processOfMax = processOfMaximum
     else
        self % processOfMax = 0
     endif

!    assertions
     ensure(self%epsilonPoint>zero, "Invalid iter control ctor")
     ensure(self%localError>zero, "Invalid iter control ctor")
     ensure(self%globalError>zero, "Invalid iter control ctor")
     ensure(self%maxIter>zero, "Invalid iter control ctor")
     ensure(self%nIter>=zero, "Invalid iter control ctor")
     ensure(self%nTotIter>=zero, "Invalid iter control ctor")
     ensure(self%zoneOfMax>=zero, "Invalid iter control ctor")
     ensure(self%processOfMax>=zero, "Invalid iter control ctor")

     return
  end subroutine iter_control_ctor

!=======================================================================
! setControls interface
!=======================================================================

  subroutine iter_control_set(self,epsilonPoint,maxNumberOfIterations)

!    Set the iteration controls in the iteration control object
!      epsilonPoint            rel. point error convergence criterion
!      maxNumberOfIterations   maximum number of iterations

!    variable declarations
     implicit none

!    passed variables
     type(IterControl),    intent(inout) :: self
     real(adqt), optional, intent(in)    :: epsilonPoint
     integer,    optional, intent(in)    :: maxNumberOfIterations

!    set the iteration controls of the iteration control object

     if (present(epsilonPoint)) then
        self % epsilonPoint = epsilonPoint
     endif

     if (present(maxNumberOfIterations)) then
        self % maxIter = maxNumberOfIterations
     endif

     return
  end subroutine iter_control_set

!=======================================================================
! resetNumberOfIterations interface
!=======================================================================

  subroutine iter_control_reset_nIter(self)

!    Reset the number of iteration in the iteration control object

!    variable declarations
     implicit none

!    passed variables
     type(IterControl), intent(inout) :: self

!    reset the number of iterations
     self % nIter = 0
     self % nTotIter = 0

!    assertions
     ensure(self%nIter==0, "Invalid iter control reset")
     ensure(self%nTotIter==0, "Invalid iter control reset")

     return
  end subroutine iter_control_reset_nIter


!=======================================================================
! setNumberOfIterations interface
!=======================================================================

  subroutine iter_control_set_nIter(self, nIter)

!    Set the number of iterations in the iteration control object

!    variable declarations
     implicit none

!    passed variables
     type(IterControl), intent(inout) :: self
     integer,           intent(in)    :: nIter

!    assertions
     require(nIter>=0, "Invalid number of iterations")

!    reset the number of iterations
     self % nIter = nIter
     self % nTotIter = self % nTotIter + nIter

!    assertions
     ensure(self%nIter>=0, "Invalid number of iterations")
     ensure(self%nTotIter>=0, "Invalid number of iterations")

     return
  end subroutine iter_control_set_nIter

!=======================================================================
! setZoneOfMax interface
!=======================================================================
                                                                                      
  subroutine iter_control_set_zoneOfMax(self, zoneOfMax)
                                                                                      
!    Set the zone number of the maximum relative pointwise error
!     zoneOfMax   zone of maximum relative pointwise error
                                                                                      
!    variable declarations
     implicit none
                                                                                      
!    passed variables
     type(IterControl), intent(inout) :: self
     integer,           intent(in)    :: zoneOfMax 
                                                                                      
!    assertions
     require(zoneOfMax>=0, "Invalid zone number")
                                                                                      
!    reset the zone with maximum relative error 
     self % zoneOfMax = zoneOfMax 
                                                                                      
!    assertions
     ensure(self%zoneOfMax>0, "Invalid zone number")
                                                                                      
     return
  end subroutine iter_control_set_zoneOfMax

!=======================================================================
! setProcessOfMax interface
!=======================================================================
                                                                                      
  subroutine iter_control_set_processOfMax(self, processOfMax)
                                                                                      
!    Set the process number that has the maximum relative pointwise error
!    processOfMax   process of maximum relative pointwise error
                                                                                      
!    variable declarations
     implicit none
 
!    passed variables
     type(IterControl), intent(inout) :: self
     integer,           intent(in)    :: processOfMax
 
!    assertions
     require(processOfMax>=0, "Invalid process number")
 
!    reset the process with maximum relative error
     self % processOfMax = processOfMax
 
!    assertions
     ensure(self%processOfMax>=0, "Invalid process number")
 
     return
  end subroutine iter_control_set_processOfMax

!=======================================================================
! setLocalError interface
!=======================================================================
                                                                                       
  subroutine iter_control_set_localError(self, localError)
                                                                                       
!    Set the maximum relative pointwise error
!    localError   maximum relative pointwise error for process
   
!    variable declarations
     implicit none
   
!    passed variables
     type(IterControl), intent(inout) :: self
     real(adqt),        intent(in)    :: localError
   
!    assertions
     require(localError>=0, "Invalid local error")
   
!    reset local maximum relative error
     self % localError = localError 
   
!    assertions
     ensure(self%localError>=0, "Invalid local error")
   
     return
  end subroutine iter_control_set_localError

!=======================================================================
! setGlobalError interface
!=======================================================================
          
  subroutine iter_control_set_globalError(self, globalError)
                                                    
!    Set the maximum relative pointwise error
!    globalError   global maximum relative pointwise error
   
!    variable declarations
     implicit none
   
!    passed variables
     type(IterControl), intent(inout) :: self
     real(adqt),        intent(in)    :: globalError
   
!    assertions
     require(globalError>=0, "Invalid global error")
   
!    reset global maximum relative error
     self % globalError = globalError
   
!    assertions
     ensure(self%globalError>=0, "Invalid global error")
   
     return
  end subroutine iter_control_set_globalError

!=======================================================================
! destruct interface
!=======================================================================

  subroutine iter_control_dtor(self)

!    Destruct the iteration control object

!    variable declarations
     implicit none

!    passed variables
     type(IterControl), intent(inout) :: self

!    destruct the iteration control
     self % epsilonPoint = zero
     self % localError   = zero
     self % globalError  = zero
     self % maxIter      = 0
     self % nIter        = 0
     self % nTotIter     = 0
     self % zoneOfMax    = 0
     self % processOfMax = 0

     return
  end subroutine iter_control_dtor

!=======================================================================
! external data access routines
!=======================================================================

!-----------------------------------------------------------------------
  function iter_control_get_epsilonPoint(self) result(epsilonPoint)

!    Return the relative pointwise error convergence criterion
!      epsilonPoint   relative pointwise error convergence criterion

!    variable declarations
     implicit none

!    passed variables
     type(IterControl), intent(in) :: self
     real(adqt)                    :: epsilonPoint

     epsilonPoint = self % epsilonPoint

!    assertions
     ensure(epsilonPoint==self%epsilonPoint, "Invalid data access")

     return
  end function iter_control_get_epsilonPoint

!-----------------------------------------------------------------------
  function iter_control_get_maxIter(self) result(maxIter)

!   Return the maximum number of iterations
!     maxIter   maximum number of iterations

!   variable declarations
    implicit none

!   passed variables
    type(IterControl), intent(in) :: self
    integer                       :: maxIter

    maxIter = self%maxIter

!   assertions
    ensure(maxIter==self%maxIter, "Invalid data access")

    return
  end function iter_control_get_maxIter

!-----------------------------------------------------------------------
  function iter_control_get_nIter(self) result(nIter)

!   Return the required number of iterations
!     nIter   number of iterations

!   variable declarations
    implicit none

!   passed variables
    type(IterControl), intent(in) :: self
    integer                       :: nIter

    nIter = self%nIter

!   assertions
    ensure(nIter==self%nIter, "Invalid data access")

    return
  end function iter_control_get_nIter

!-----------------------------------------------------------------------
  function iter_control_get_nTotIter(self) result(nTotIter)

!   Return the total number of iterations required
!     nTotIter   total number of iterations

!   variable declarations
    implicit none

!   passed variables
    type(IterControl), intent(in) :: self
    integer                       :: nTotIter

    nTotIter = self%nTotIter

!   assertions
    ensure(nTotIter==self%nTotIter, "Invalid data access")

    return
  end function iter_control_get_nTotIter

!-----------------------------------------------------------------------
  function iter_control_get_zoneOfMax(self) result(zoneOfMax)

!   Return the zone number of the maximum relative pointwise error
!     zoneOfMax   zone of maximum relative pointwise error

!   variable declarations
    implicit none

!   passed variables
    type(IterControl), intent(in) :: self
    integer                       :: zoneOfMax

    zoneOfMax = self%zoneOfMax

!   assertions
    ensure(zoneOfMax==self%zoneOfMax, "Invalid data access")

    return
  end function iter_control_get_zoneOfMax

!-----------------------------------------------------------------------
  function iter_control_get_processOfMax(self) result(processOfMax)
                                                                                       
!   Return the process number of the maximum relative pointwise error
!     processOfMax   zone of maximum relative pointwise error
                                                                                       
!   variable declarations
    implicit none
                                                                                       
!   passed variables
    type(IterControl), intent(in) :: self
    integer                       :: processOfMax
                                                                                       
    processOfMax = self%processOfMax
                                                                                       
!   assertions
    ensure(processOfMax==self%processOfMax, "Invalid data access")
                                                                                       
    return
  end function iter_control_get_processOfMax

!-----------------------------------------------------------------------
  function iter_control_get_localError(self) result(localError)
                                                                                       
!   Return the maximum relative pointwise error for process
!     localError   maximum relative pointwise error
                                                                                       
!   variable declarations
    implicit none
                                                                                       
!   passed variables
    type(IterControl), intent(in) :: self
    real(adqt)                    :: localError 
                                                                                       
    localError = self%localError
                                                                                       
!   assertions
    ensure(localError==self%localError, "Invalid data access")
                                                                                       
    return
  end function iter_control_get_localError

!-----------------------------------------------------------------------
  function iter_control_get_globalError(self) result(globalError)
                                                                                       
!   Return the global maximum relative pointwise error
!     globalError   maximum relative pointwise error
                                                                                       
!   variable declarations
    implicit none
                                                                                       
!   passed variables
    type(IterControl), intent(in) :: self
    real(adqt)                    :: globalError
                                          
    globalError = self%globalError
                           
!   assertions
    ensure(globalError==self%globalError, "Invalid data access")
                                 
    return
  end function iter_control_get_globalError

end module iter_control_mod
