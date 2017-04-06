! Time Step Control Module:  Contains controls and edits for the time step 
                                                                                 
module TimeStepControls_mod 

  use kind_mod
  use constant_mod

  private

! public interfaces

  real(adqt), parameter, public :: TempFraction=0.9_adqt, &
                                   IterFraction=0.8_adqt, &
                                   cutoff=0.01_adqt

  public construct, destruct, setDtControls,              &
         getRadTime, getRadTimeStep, getRecTimeStep,      &
         getMaxChangeTe, getMaxChangeTi, getMaxChangeTr4, &
         getMinTimeStep, getMaxTimeStep,                  &
         getMaxFracChangeTr4, getMaxFracChangeTe,         &
         getMaxFracChangeTi, getZoneMaxChangeTr4,         &
         getZoneMaxChangeTe, getZoneMaxChangeTi,          &
         getControlProcess, getDtConstraint 
                                                                                 
  type, public :: TimeStepControls 

     integer          :: ZoneMaxChangeTr4  ! Zone with max change in Tr**4
     integer          :: ZoneMaxChangeTe   ! Zone with max change in Te
     integer          :: ZoneMaxChangeTi   ! Zone with max change in Ti
     integer          :: ControlProcess    ! Process controlling time step
                                                                                 
     real(adqt)       :: RadTimeStep       ! Current radiation time step
     real(adqt)       :: RecTimeStep       ! Recommended time step for next cycle
     real(adqt)       :: MaxFracChangeTr4  ! Max fractional change in Tr**4
     real(adqt)       :: MaxFracChangeTe   ! Max fractional change in Te
     real(adqt)       :: MaxFracChangeTi   ! Max fractional change in Ti
     real(adqt)       :: Tr4Threshold      ! Tr4 Threshold for time step control

     real(adqt)       :: RadTime           ! Current radiation time
     real(adqt)       :: MaxChangeTe       ! Max allowed change in Te per cycle 
     real(adqt)       :: MaxChangeTi       ! Max allowed change in Ti per cycle
     real(adqt)       :: MaxChangeTr4      ! Max allowed change in Tr**4 per cycle
     real(adqt)       :: MinTimeStep       ! Minimum allowed time step
     real(adqt)       :: MaxTimeStep       ! Maximum allowed time step

     character(len=8) :: DtConstraint      ! What's controlling the time step 

  end type TimeStepControls 

  type(TimeStepControls), pointer, public :: DtControls

  interface construct
    module procedure TimeStepControls_ctor
  end interface

  interface destruct
    module procedure TimeStepControls_dtor
  end interface

  interface setDtControls
    module procedure TimeStepControls_set
  end interface

  interface getRadTime
    module procedure TimeStepControls_get_RadTime
  end interface

  interface getRadTimeStep
    module procedure TimeStepControls_get_RadTimeStep
  end interface

  interface getRecTimeStep
    module procedure TimeStepControls_get_RecTimeStep
  end interface

  interface getMaxChangeTe
    module procedure TimeStepControls_get_MaxChangeTe
  end interface

  interface getMaxChangeTi
    module procedure TimeStepControls_get_MaxChangeTi
  end interface

  interface getMaxChangeTr4
    module procedure TimeStepControls_get_MaxChangeTr4
  end interface

  interface getMinTimeStep
    module procedure TimeStepControls_get_MinTimeStep
  end interface

  interface getMaxTimeStep
    module procedure TimeStepControls_get_MaxTimeStep
  end interface

  interface getMaxFracChangeTr4
    module procedure TimeStepControls_get_MaxFracChangeTr4
  end interface

  interface getMaxFracChangeTe
    module procedure TimeStepControls_get_MaxFracChangeTe
  end interface

  interface getMaxFracChangeTi
    module procedure TimeStepControls_get_MaxFracChangeTi
  end interface

  interface getZoneMaxChangeTr4
    module procedure TimeStepControls_get_ZoneMaxChangeTr4
  end interface

  interface getZoneMaxChangeTe
    module procedure TimeStepControls_get_ZoneMaxChangeTe
  end interface

  interface getZoneMaxChangeTi
    module procedure TimeStepControls_get_ZoneMaxChangeTi
  end interface

  interface getControlProcess
    module procedure TimeStepControls_get_ControlProcess
  end interface

  interface getDtConstraint
    module procedure TimeStepControls_get_DtConstraint
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================
                                                                                   
  subroutine TimeStepControls_ctor(self,RadTimeStep,MaxChangeTe,  &
                                   MaxChangeTi,MaxChangeTr4,      &
                                   MinTimeStep,MaxTimeStep)

    implicit none

!   Passed variables

    type(TimeStepControls),    intent(inout) :: self
    real(adqt), optional, intent(in)         :: RadTimeStep 
    real(adqt), optional, intent(in)         :: MaxChangeTe
    real(adqt), optional, intent(in)         :: MaxChangeTi
    real(adqt), optional, intent(in)         :: MaxChangeTr4
    real(adqt), optional, intent(in)         :: MinTimeStep
    real(adqt), optional, intent(in)         :: MaxTimeStep 

!   Construct the Timestep control object

    if (present(RadTimeStep)) then
      self % RadTimeStep = RadTimeStep
      self % RecTimeStep = RadTimeStep
    else
      self % RadTimeStep = 1.0e-3_adqt
      self % RecTimeStep = 1.0e-3_adqt 
    endif

    if (present(MaxChangeTe)) then
      self % MaxChangeTe = MaxChangeTe 
    else
      self % MaxChangeTe = 4.0e-1_adqt
    endif

    if (present(MaxChangeTi)) then
      self % MaxChangeTi = MaxChangeTi
    else
      self % MaxChangeTi = 4.0e-1_adqt
    endif

    if (present(MaxChangeTr4)) then
      self % MaxChangeTr4 = MaxChangeTr4 
    else
      self % MaxChangeTr4 = 4.0e-1_adqt 
    endif

    if (present(MinTimeStep)) then
      self % MinTimeStep = MinTimeStep
    else
      self % MinTimeStep = 1.0e-4_adqt
    endif

    if (present(MaxTimeStep)) then
      self % MaxTimeStep = MaxTimeStep
    else
      self % MaxTimeStep = 1.0e-1_adqt
    endif

    self % ZoneMaxChangeTr4 = -1
    self % ZoneMaxChangeTe  = -1
    self % ZoneMaxChangeTi  = -1
    self % ControlProcess   = -1

    self % MaxFracChangeTr4 = zero
    self % MaxFracChangeTe  = zero
    self % MaxFracChangeTi  = zero
    self % RadTime          = zero
    self % Tr4Threshold     = zero

    self % DtConstraint     = 'none'


    return

  end subroutine TimeStepControls_ctor
                                                      
!=======================================================================
! destruct interface
!=======================================================================
                                                                                    
  subroutine TimeStepControls_dtor(self)

    implicit none

!   Passed variables

    type(TimeStepControls),    intent(inout) :: self
                                                                
    self % ZoneMaxChangeTr4 = -1 
    self % ZoneMaxChangeTe  = -1
    self % ZoneMaxChangeTi  = -1
    self % ControlProcess   = -1
                                                                                         
    self % RadTimeStep      = zero
    self % RecTimeStep      = zero
    self % MaxFracChangeTr4 = zero
    self % MaxFracChangeTe  = zero
    self % MaxFracChangeTi  = zero
    self % Tr4Threshold     = zero
                                                                                         
    self % RadTime          = zero
    self % MaxChangeTe      = zero
    self % MaxChangeTi      = zero
    self % MaxChangeTr4     = zero
    self % MinTimeStep      = zero
    self % MaxTimeStep      = zero
                                                                                         
    self % DtConstraint     = 'null'


    return

  end subroutine TimeStepControls_dtor

!=======================================================================
! set interface
!=======================================================================
                                                                                         
  subroutine TimeStepControls_set(self,             &
                                  ControlProcess,   &
                                  ZoneMaxChangeTr4, &
                                  ZoneMaxChangeTe,  &
                                  ZoneMaxChangeTi,  &
                                  RadTimeStep,      &
                                  RecTimeStep,      &
                                  MaxFracChangeTr4, &
                                  MaxFracChangeTe,  &
                                  MaxFracChangeTi,  &
                                  Tr4Threshold,     &
                                  RadTime,          &
                                  DtConstraint      )
                                                                                         
    implicit none
                                                                                         
!   Passed variables
                                                                                         
    type(TimeStepControls),    intent(inout) :: self
    integer,    optional, intent(in)         :: ControlProcess
    integer,    optional, intent(in)         :: ZoneMaxChangeTr4
    integer,    optional, intent(in)         :: ZoneMaxChangeTe
    integer,    optional, intent(in)         :: ZoneMaxChangeTi
    real(adqt), optional, intent(in)         :: RadTimeStep
    real(adqt), optional, intent(in)         :: RecTimeStep 
    real(adqt), optional, intent(in)         :: MaxFracChangeTr4
    real(adqt), optional, intent(in)         :: MaxFracChangeTe
    real(adqt), optional, intent(in)         :: MaxFracChangeTi
    real(adqt), optional, intent(in)         :: Tr4Threshold
    real(adqt), optional, intent(in)         :: RadTime

    character(len=8), optional, intent(in)   :: DtConstraint
                                                                                         
!   Update the Timestep control object

    if (present(ControlProcess)) then
      self % ControlProcess = ControlProcess 
    endif

    if (present(ZoneMaxChangeTr4)) then
      self % ZoneMaxChangeTr4 = ZoneMaxChangeTr4 
    endif

    if (present(ZoneMaxChangeTe)) then
      self % ZoneMaxChangeTe = ZoneMaxChangeTe
    endif

    if (present(ZoneMaxChangeTi)) then
      self % ZoneMaxChangeTi = ZoneMaxChangeTi
    endif
                                                                                         
    if (present(RadTimeStep)) then
      self % RadTimeStep = RadTimeStep
    endif

    if (present(RecTimeStep)) then
      self % RecTimeStep = RecTimeStep
    endif

    if (present(MaxFracChangeTr4)) then
      self % MaxFracChangeTr4 = MaxFracChangeTr4 
    endif

    if (present(MaxFracChangeTe)) then
      self % MaxFracChangeTe = MaxFracChangeTe
    endif

    if (present(MaxFracChangeTi)) then
      self % MaxFracChangeTi = MaxFracChangeTi
    endif

    if (present(Tr4Threshold)) then
      self % Tr4Threshold = Tr4Threshold 
    endif

    if (present(RadTime)) then
      self % RadTime = RadTime 
    endif

    if (present(DtConstraint)) then
      self % DtConstraint = DtConstraint 
    endif

    return
 
  end subroutine TimeStepControls_set

!=======================================================================
! getRadTime interface
!=======================================================================
  function TimeStepControls_get_RadTime(self) result(RadTime)

!    Return the current radiation time (RadTime)  

!    variable declarations
     implicit none

!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: RadTime 

     RadTime = self % RadTime 

     return
  end function TimeStepControls_get_RadTime

!=======================================================================
! getRadTimeStep interface
!=======================================================================
  function TimeStepControls_get_RadTimeStep(self) result(RadTimeStep)

!    Return the current radiation time step (RadTimeStep)

!    variable declarations
     implicit none

!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: RadTimeStep

     RadTimeStep = self % RadTimeStep

     return
  end function TimeStepControls_get_RadTimeStep

!=======================================================================
! getRecTimeStep interface
!=======================================================================
  function TimeStepControls_get_RecTimeStep(self) result(RecTimeStep)
                                                                                       
!    Return the recommended radiation time step (RecTimeStep)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: RecTimeStep
       
     RecTimeStep = self % RecTimeStep
       
     return
  end function TimeStepControls_get_RecTimeStep

!=======================================================================
! getMaxChangeTe interface
!=======================================================================
  function TimeStepControls_get_MaxChangeTe(self) result(MaxChangeTe)

!    Return the Maximum allowed change in Te per cycle (MaxChangeTe)

!    variable declarations
     implicit none

!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: MaxChangeTe 

     MaxChangeTe = self % MaxChangeTe 

     return
  end function TimeStepControls_get_MaxChangeTe

!=======================================================================
! getMaxChangeTi interface
!=======================================================================
  function TimeStepControls_get_MaxChangeTi(self) result(MaxChangeTi)

!    Return the Maximum allowed change in Ti per cycle (MaxChangeTi)

!    variable declarations
     implicit none

!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: MaxChangeTi

     MaxChangeTi = self % MaxChangeTi

     return
  end function TimeStepControls_get_MaxChangeTi

!=======================================================================
! getMaxChangeTr4 interface
!=======================================================================
  function TimeStepControls_get_MaxChangeTr4(self) result(MaxChangeTr4)

!    Return the Maximum allowed change in Tr4 per cycle (MaxChangeTr4)

!    variable declarations
     implicit none

!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: MaxChangeTr4

     MaxChangeTr4 = self % MaxChangeTr4

     return
  end function TimeStepControls_get_MaxChangeTr4

!=======================================================================
! getMinTimeStep interface
!=======================================================================
  function TimeStepControls_get_MinTimeStep(self) result(MinTimeStep)

!    Return the minimum allowed time step (MinTimeStep)

!    variable declarations
     implicit none

!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: MinTimeStep 

     MinTimeStep = self % MinTimeStep 

     return
  end function TimeStepControls_get_MinTimeStep

!=======================================================================
! getMaxTimeStep interface
!=======================================================================
  function TimeStepControls_get_MaxTimeStep(self) result(MaxTimeStep)

!    Return the maximum allowed time step (MaxTimeStep)

!    variable declarations
     implicit none

!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: MaxTimeStep

     MaxTimeStep = self % MaxTimeStep

     return
  end function TimeStepControls_get_MaxTimeStep

!=======================================================================
! getMaxFracChangeTr4 interface
!=======================================================================
  function TimeStepControls_get_MaxFracChangeTr4(self) result(MaxFracChangeTr4)
                                                                                       
!    Return the maximum observed fractional change in Tr4 (MaxFracChangeTr4)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: MaxFracChangeTr4
                                                                                       
     MaxFracChangeTr4 = self % MaxFracChangeTr4 
                                                                                       
     return
  end function TimeStepControls_get_MaxFracChangeTr4

!=======================================================================
! getMaxFracChangeTe interface
!=======================================================================
  function TimeStepControls_get_MaxFracChangeTe(self) result(MaxFracChangeTe)
                                                                                       
!    Return the maximum observed fractional change in Te (MaxFracChangeTe)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: MaxFracChangeTe
                                                                                       
     MaxFracChangeTe = self % MaxFracChangeTe
                                                                                       
     return
  end function TimeStepControls_get_MaxFracChangeTe

!=======================================================================
! getMaxFracChangeTi interface
!=======================================================================
  function TimeStepControls_get_MaxFracChangeTi(self) result(MaxFracChangeTi)
                                                                                       
!    Return the maximum observed fractional change in Ti (MaxFracChangeTi)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(TimeStepControls), intent(in) :: self
     real(adqt)                         :: MaxFracChangeTi
                                                                                       
     MaxFracChangeTi = self % MaxFracChangeTi
                                                                                       
     return
  end function TimeStepControls_get_MaxFracChangeTi

!=======================================================================
! getZoneMaxChangeTr4 interface
!=======================================================================
  function TimeStepControls_get_ZoneMaxChangeTr4(self) result(ZoneMaxChangeTr4)
                                                                                       
!    Return the zone with the maximum observed fractional 
!    change in Tr4 (ZoneMaxChangeTr4)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(TimeStepControls), intent(in) :: self
     integer                            :: ZoneMaxChangeTr4 
                                                                                       
     ZoneMaxChangeTr4 = self % ZoneMaxChangeTr4 
                                                                                       
     return
  end function TimeStepControls_get_ZoneMaxChangeTr4

!=======================================================================
! getZoneMaxChangeTe interface
!=======================================================================
  function TimeStepControls_get_ZoneMaxChangeTe(self) result(ZoneMaxChangeTe)
                                                                                       
!    Return the zone with the maximum observed fractional
!    change in Te (ZoneMaxChangeTe)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(TimeStepControls), intent(in) :: self
     integer                            :: ZoneMaxChangeTe
                                                                                       
     ZoneMaxChangeTe = self % ZoneMaxChangeTe
                                                                                       
     return
  end function TimeStepControls_get_ZoneMaxChangeTe

!=======================================================================
! getZoneMaxChangeTi interface
!=======================================================================
  function TimeStepControls_get_ZoneMaxChangeTi(self) result(ZoneMaxChangeTi)
                                                                                       
!    Return the zone with the maximum observed fractional
!    change in Ti (ZoneMaxChangeTi)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(TimeStepControls), intent(in) :: self
     integer                            :: ZoneMaxChangeTi
                                                                                       
     ZoneMaxChangeTi = self % ZoneMaxChangeTi
                                                                                       
     return
  end function TimeStepControls_get_ZoneMaxChangeTi

!=======================================================================
! getControlProcess interface
!=======================================================================
  function TimeStepControls_get_ControlProcess(self) result(ControlProcess)
                                                                                       
!    Return the process controlling the time step (ControlProcess)

!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(TimeStepControls), intent(in) :: self
     integer                            :: ControlProcess 
                                                                                       
     ControlProcess = self % ControlProcess 
                                                                                       
     return
  end function TimeStepControls_get_ControlProcess

!=======================================================================
! getDtConstraint interface
!=======================================================================
  function TimeStepControls_get_DtConstraint(self) result(DtConstraint)
                                                                                       
!    Return the process controlling the time step (DtConstraint)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(TimeStepControls), intent(in) :: self
     character(len=8)                   :: DtConstraint 
                                                                                       
     DtConstraint = self % DtConstraint 
                                                                                       
     return
  end function TimeStepControls_get_DtConstraint


end module TimeStepControls_mod

