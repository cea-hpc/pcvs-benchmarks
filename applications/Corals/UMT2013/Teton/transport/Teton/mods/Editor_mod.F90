! Editor Module:  Contains various problem edits 
                                                                                 
module Editor_mod 

  use kind_mod
  use constant_mod

  private

! public interfaces

  public construct, destruct, setEdits,              &
         getTrMaxZone, getTeMaxZone, getTiMaxZone,   &
         getTrMaxNode, getTeMaxNode, getTiMaxNode,   &
         getTrMax, getTeMax, getTiMax,               &
         getEnergyRadiation, getEnergyMaterial,      &
         getEnergyIncident, getEnergyEscaped,        &
         getEnergyExtSources, getEnergyCheck,        &
         getDeltaEnergyRad, getDeltaEnergyMat,       &
         getDeltaEnergyInc, getDeltaEnergyEsc,       &
         getDeltaEnergyExtSrc, getDeltaEnergyCheck

  type, public :: Editor 

     integer, dimension (1) :: TrMaxZone       ! Zone with max value of Tr
     integer, dimension (1) :: TeMaxZone       ! Zone with max value of Te
     integer, dimension (1) :: TiMaxZone       ! Zone with max value of Ti

     integer             :: TrMaxNode          ! Node with max Tr
     integer             :: TeMaxNode          ! Node with max Te
     integer             :: TiMaxNode          ! Node with max Ti

     real(adqt)          :: TrMax              ! Max value of Tr
     real(adqt)          :: TeMax              ! Max value of Te
     real(adqt)          :: TiMax              ! Max value of Ti

!    Time-integrated edits
     real(adqt)          :: EnergyRadiation    ! Global Energy in radiation field
     real(adqt)          :: EnergyMaterial     ! Global Energy in the material
     real(adqt)          :: EnergyIncident     ! Global Energy incident on boundaries
     real(adqt)          :: EnergyEscaped      ! Global Energy escaping from boundaries
     real(adqt)          :: EnergyExtSources   ! Global Energy from external sources
     real(adqt)          :: EnergyCheck        ! Global Energy check or error

!    Per Cycle Edits
     real(adqt)          :: DeltaEnergyRad     ! Change in radiation energy 
     real(adqt)          :: DeltaEnergyMat     ! Change in material energy
     real(adqt)          :: DeltaEnergyInc     ! Change in incident energy 
     real(adqt)          :: DeltaEnergyEsc     ! Change in escaping energy 
     real(adqt)          :: DeltaEnergyExtSrc  ! Change in external source energy
     real(adqt)          :: DeltaEnergyHydWork ! Change in energy due to hydrodynamic work
     real(adqt)          :: DeltaEnergyCheck   ! Energy error this cycle
     real(adqt)          :: EnergyRadBOC       ! Beginning of cycle radiation energy
     real(adqt)          :: EnergyRadEOC       ! End of cycle radiation energy

     real(adqt), pointer :: RadEnergyEscRate(:)  ! RadEnergyEscRate(ngr*(nbedit+1)) 
     real(adqt), pointer :: RadEnergyIncRate(:)  ! RadEnergyIncRate(ngr*(nbedit+1))
     real(adqt), pointer :: RadEnergyEscape(:)   ! RadEnergyEscape(ngr*(nbedit+1))
     real(adqt), pointer :: RadEnergyIncident(:) ! RadEnergyIncident(ngr*(nbedit+1))
                                                                                 
  end type Editor 

  type(Editor), pointer, public :: RadEdit

  interface construct
    module procedure Editor_ctor
  end interface

  interface destruct
    module procedure Editor_dtor
  end interface

  interface setEdits
    module procedure Editor_set
  end interface

  interface getTrMaxZone
    module procedure Editor_get_TrMaxZone
  end interface

  interface getTeMaxZone
    module procedure Editor_get_TeMaxZone
  end interface

  interface getTiMaxZone
    module procedure Editor_get_TiMaxZone
  end interface

  interface getTrMaxNode
    module procedure Editor_get_TrMaxNode
  end interface
                                                                                       
  interface getTeMaxNode
    module procedure Editor_get_TeMaxNode
  end interface
                                                                                       
  interface getTiMaxNode
    module procedure Editor_get_TiMaxNode
  end interface

  interface getTrMax
    module procedure Editor_get_TrMax
  end interface

  interface getTeMax
    module procedure Editor_get_TeMax
  end interface

  interface getTiMax
    module procedure Editor_get_TiMax
  end interface

  interface getEnergyRadiation
    module procedure Editor_get_EnergyRadiation
  end interface

  interface getEnergyMaterial
    module procedure Editor_get_EnergyMaterial
  end interface

  interface getEnergyIncident
    module procedure Editor_get_EnergyIncident
  end interface

  interface getEnergyEscaped
    module procedure Editor_get_EnergyEscaped
  end interface

  interface getEnergyExtSources
    module procedure Editor_get_EnergyExtSources
  end interface

  interface getEnergyCheck
    module procedure Editor_get_EnergyCheck
  end interface

  interface getDeltaEnergyRad
    module procedure Editor_get_DeltaEnergyRad
  end interface

  interface getDeltaEnergyMat
    module procedure Editor_get_DeltaEnergyMat
  end interface

  interface getDeltaEnergyInc
    module procedure Editor_get_DeltaEnergyInc
  end interface

  interface getDeltaEnergyEsc
    module procedure Editor_get_DeltaEnergyEsc
  end interface

  interface getDeltaEnergyExtSrc
    module procedure Editor_get_DeltaEnergyExtSrc
  end interface

  interface getDeltaEnergyCheck
    module procedure Editor_get_DeltaEnergyCheck
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================
                                                                                   
  subroutine Editor_ctor(self)

    use Size_mod

    implicit none

!   Passed variables

    type(Editor),    intent(inout) :: self

!   Initialize scalers

    self % TrMaxZone          =  0
    self % TeMaxZone          =  0
    self % TiMaxZone          =  0
    self % TrMaxNode          = -1
    self % TeMaxNode          = -1
    self % TiMaxNode          = -1

    self % TrMax              = zero
    self % TeMax              = zero
    self % TiMax              = zero

    self % EnergyRadiation    = zero
    self % EnergyMaterial     = zero
    self % EnergyIncident     = zero
    self % EnergyEscaped      = zero
    self % EnergyExtSources   = zero
    self % EnergyCheck        = zero

    self % DeltaEnergyRad     = zero
    self % DeltaEnergyMat     = zero
    self % DeltaEnergyInc     = zero
    self % DeltaEnergyEsc     = zero
    self % DeltaEnergyExtSrc  = zero 
    self % DeltaEnergyHydWork = zero
    self % DeltaEnergyCheck   = zero

!   Allocate arrays in the edit object

!!$    allocate( self % RadEnergyEscRate( Size%ngr*(Size%nbedit+1) ) )
!!$    allocate( self % RadEnergyIncRate( Size%ngr*(Size%nbedit+1) ) )
!!$    allocate( self % RadEnergyEscape( Size%ngr*(Size%nbedit+1) ) )
!!$    allocate( self % RadEnergyIncident( Size%ngr*(Size%nbedit+1) ) )
!!$
!!$    self % RadEnergyEscRate(:)  = zero
!!$    self % RadEnergyIncRate(:)  = zero
!!$    self % RadEnergyEscape(:)   = zero
!!$    self % RadEnergyIncident(:) = zero


    return

  end subroutine Editor_ctor

!=======================================================================
! destruct interface
!=======================================================================
                                                                                          
  subroutine Editor_dtor(self)
                                                                                          
    implicit none
                                                                                          
!   Passed variables
                                                                                          
    type(Editor),    intent(inout) :: self
                                                                                          
!   Zero scalers
                                                                                          
    self % TrMaxZone        = 0
    self % TeMaxZone        = 0
    self % TiMaxZone        = 0
    self % TrMaxNode        = -1
    self % TeMaxNode        = -1
    self % TiMaxNode        = -1

    self % TrMax            = zero
    self % TeMax            = zero
    self % TiMax            = zero

    self % EnergyRadiation  = zero
    self % EnergyMaterial   = zero
    self % EnergyIncident   = zero
    self % EnergyEscaped    = zero
    self % EnergyExtSources = zero
    self % EnergyCheck      = zero

    self % DeltaEnergyRad    = zero
    self % DeltaEnergyMat    = zero
    self % DeltaEnergyInc    = zero
    self % DeltaEnergyEsc    = zero
    self % DeltaEnergyExtSrc = zero
    self % DeltaEnergyCheck  = zero
                          
!   Deallocate arrays in the edit object
                          
!!$    deallocate( self % RadEnergyEscRate  )
!!$    deallocate( self % RadEnergyIncRate  )
!!$    deallocate( self % RadEnergyEscape   )
!!$    deallocate( self % RadEnergyIncident )
                          
                          
    return
          
  end subroutine Editor_dtor

!=======================================================================
! set interface
!=======================================================================
                                                                                         
  subroutine Editor_set(self,              &
                        TrMaxZone,         &
                        TeMaxZone,         &
                        TiMaxZone,         &
                        TrMaxNode,         &
                        TeMaxNode,         &
                        TiMaxNode,         &
                        TrMax,             &
                        TeMax,             &
                        TiMax,             &
                        EnergyRadiation,   &
                        EnergyMaterial,    &
                        EnergyIncident,    &
                        EnergyEscaped,     &
                        EnergyExtSources,  &
                        EnergyCheck,       &
                        DeltaEnergyRad,    &
                        DeltaEnergyMat,    &
                        DeltaEnergyInc,    &
                        DeltaEnergyEsc,    &
                        DeltaEnergyExtSrc, &
                        DeltaEnergyCheck   )

    implicit none
                                                                                         
!   Passed variables
                                                                                         
    type(Editor),         intent(inout)             :: self
    integer,    optional, intent(in), dimension (1) :: TrMaxZone
    integer,    optional, intent(in), dimension (1) :: TeMaxZone
    integer,    optional, intent(in), dimension (1) :: TiMaxZone
    integer,    optional, intent(in)                :: TrMaxNode
    integer,    optional, intent(in)                :: TeMaxNode
    integer,    optional, intent(in)                :: TiMaxNode
    real(adqt), optional, intent(in)                :: TrMax 
    real(adqt), optional, intent(in)                :: TeMax 
    real(adqt), optional, intent(in)                :: TiMax
    real(adqt), optional, intent(in)                :: EnergyRadiation
    real(adqt), optional, intent(in)                :: EnergyMaterial
    real(adqt), optional, intent(in)                :: EnergyIncident
    real(adqt), optional, intent(in)                :: EnergyEscaped
    real(adqt), optional, intent(in)                :: EnergyExtSources
    real(adqt), optional, intent(in)                :: EnergyCheck
    real(adqt), optional, intent(in)                :: DeltaEnergyRad
    real(adqt), optional, intent(in)                :: DeltaEnergyMat
    real(adqt), optional, intent(in)                :: DeltaEnergyInc
    real(adqt), optional, intent(in)                :: DeltaEnergyEsc
    real(adqt), optional, intent(in)                :: DeltaEnergyExtSrc
    real(adqt), optional, intent(in)                :: DeltaEnergyCheck

!   Update the Timestep control object

    if (present(TrMaxZone)) then
      self % TrMaxZone = TrMaxZone 
    endif

    if (present(TeMaxZone)) then
      self % TeMaxZone = TeMaxZone
    endif

    if (present(TiMaxZone)) then
      self % TiMaxZone = TiMaxZone
    endif

    if (present(TrMaxNode)) then
      self % TrMaxNode = TrMaxNode
    endif

    if (present(TeMaxNode)) then
      self % TeMaxNode = TeMaxNode
    endif

    if (present(TiMaxNode)) then
      self % TiMaxNode = TiMaxNode
    endif

    if (present(TrMax)) then
      self % TrMax = TrMax
    endif

    if (present(TeMax)) then
      self % TeMax = TeMax
    endif

    if (present(TiMax)) then
      self % TiMax = TiMax
    endif

    if (present(EnergyRadiation)) then
      self % EnergyRadiation = EnergyRadiation
    endif

    if (present(EnergyMaterial)) then
      self % EnergyMaterial = EnergyMaterial 
    endif

    if (present(EnergyIncident)) then
      self % EnergyIncident = EnergyIncident 
    endif

    if (present(EnergyEscaped)) then
      self % EnergyEscaped = EnergyEscaped 
    endif

    if (present(EnergyExtSources)) then
      self % EnergyExtSources = EnergyExtSources 
    endif

    if (present(EnergyCheck)) then
      self % EnergyCheck = EnergyCheck 
    endif

    if (present(DeltaEnergyRad)) then
      self % DeltaEnergyRad = DeltaEnergyRad 
    endif

    if (present(DeltaEnergyMat)) then
      self % DeltaEnergyMat = DeltaEnergyMat
    endif

    if (present(DeltaEnergyInc)) then
      self % DeltaEnergyInc = DeltaEnergyInc
    endif

    if (present(DeltaEnergyEsc)) then
      self % DeltaEnergyEsc = DeltaEnergyEsc
    endif

    if (present(DeltaEnergyExtSrc)) then
      self % DeltaEnergyExtSrc = DeltaEnergyExtSrc
    endif

    if (present(DeltaEnergyCheck)) then
      self % DeltaEnergyCheck = DeltaEnergyCheck
    endif


    return
 
  end subroutine Editor_set

!=======================================================================
! getTrMaxZone interface
!=======================================================================
  function Editor_get_TrMaxZone(self) result(TrMaxZone)

!    Return the zone with maximum radiation temperature (TrMaxZone)  

!    variable declarations
     implicit none

!    passed variables
     type(Editor), intent(in) :: self
     integer, dimension (1)   :: TrMaxZone 

     TrMaxZone = self % TrMaxZone 

     return
  end function Editor_get_TrMaxZone

!=======================================================================
! getTeMaxZone interface
!=======================================================================
  function Editor_get_TeMaxZone(self) result(TeMaxZone)

!    Return the zone with maximum electron temperature (TeMaxZone)

!    variable declarations
     implicit none

!    passed variables
     type(Editor), intent(in) :: self
     integer, dimension (1)   :: TeMaxZone

     TeMaxZone = self % TeMaxZone

     return
  end function Editor_get_TeMaxZone

!=======================================================================
! getTiMaxZone interface
!=======================================================================
  function Editor_get_TiMaxZone(self) result(TiMaxZone)

!    Return the zone with maximum ion temperature (TiMaxZone)

!    variable declarations
     implicit none

!    passed variables
     type(Editor), intent(in) :: self
     integer, dimension (1)   :: TiMaxZone

     TiMaxZone = self % TiMaxZone

     return
  end function Editor_get_TiMaxZone

!=======================================================================
! getTrMaxNode interface
!=======================================================================
  function Editor_get_TrMaxNode(self) result(TrMaxNode)
                                                                                       
!    Return the node with maximum radiation temperature (TrMaxNode)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(Editor), intent(in) :: self
     integer                  :: TrMaxNode
                                                                                       
     TrMaxNode = self % TrMaxNode
                                                                                       
     return
  end function Editor_get_TrMaxNode

!=======================================================================
! getTeMaxNode interface
!=======================================================================
  function Editor_get_TeMaxNode(self) result(TeMaxNode)
                                                                                       
!    Return the node with maximum electron temperature (TeMaxNode)
                                                                                       
!    variable declarations
     implicit none
                                                                                       
!    passed variables
     type(Editor), intent(in) :: self
     integer                  :: TeMaxNode
                                                                                       
     TeMaxNode = self % TeMaxNode
                                                                                       
     return
  end function Editor_get_TeMaxNode

!=======================================================================
! getTiMaxNode interface
!=======================================================================
  function Editor_get_TiMaxNode(self) result(TiMaxNode)
      
!    Return the node with maximum ion temperature (TiMaxNode)
      
!    variable declarations
     implicit none
      
!    passed variables
     type(Editor), intent(in) :: self
     integer                  :: TiMaxNode
      
     TiMaxNode = self % TiMaxNode
      
     return
  end function Editor_get_TiMaxNode

!=======================================================================
! getTrMax interface
!=======================================================================
  function Editor_get_TrMax(self) result(TrMax)

!    Return the maximum radiation temperature (TrMax)

!    variable declarations
     implicit none

!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: TrMax

     TrMax = self % TrMax

     return
  end function Editor_get_TrMax

!=======================================================================
! getTeMax interface
!=======================================================================
  function Editor_get_TeMax(self) result(TeMax)

!    Return the maximum electron temperature (TeMax)

!    variable declarations
     implicit none
 
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: TeMax
 
     TeMax = self % TeMax
 
     return
  end function Editor_get_TeMax

!=======================================================================
! getTiMax interface
!=======================================================================
 
  function Editor_get_TiMax(self) result(TiMax)
 
!    Return the maximum ion temperature (TiMax)
 
!    variable declarations
     implicit none
 
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: TiMax
 
     TiMax = self % TiMax
 
     return
  end function Editor_get_TiMax

!=======================================================================
! getEnergyRadiation interface
!=======================================================================
                                                                                         
  function Editor_get_EnergyRadiation(self) result(EnergyRadiation)
                                                                                         
                                                                                         
!    Return the radiation energy (EnergyRadiation)
                                                                                         
!    variable declarations
     implicit none
                                                                                         
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: EnergyRadiation

     EnergyRadiation = self % EnergyRadiation

     return
  end function Editor_get_EnergyRadiation

!=======================================================================
! getEnergyMaterial interface
!=======================================================================
                                                                                          
  function Editor_get_EnergyMaterial(self) result(EnergyMaterial)
                                                                                          
                                                                                          
!    Return the material energy (EnergyMaterial)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: EnergyMaterial
                                                                                          
     EnergyMaterial = self % EnergyMaterial
                                                                                          
     return
  end function Editor_get_EnergyMaterial

!=======================================================================
! getEnergyIncident interface
!=======================================================================
                                                                                          
  function Editor_get_EnergyIncident(self) result(EnergyIncident)
                                                                                          
                                                                                          
!    Return the incident energy on the boundary (EnergyIncident)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: EnergyIncident
                                                                                          
     EnergyIncident = self % EnergyIncident
                                                                                          
     return
  end function Editor_get_EnergyIncident

!=======================================================================
! getEnergyEscaped interface
!=======================================================================
                                                                                          
  function Editor_get_EnergyEscaped(self) result(EnergyEscaped)
                                                                                          
                                                                                          
!    Return the escaped energy from the boundary (EnergyEscaped)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: EnergyEscaped
                                                                                          
     EnergyEscaped = self % EnergyEscaped
                                                                                          
     return
  end function Editor_get_EnergyEscaped

!=======================================================================
! getEnergyExtSources interface
!=======================================================================
                                                                                          
  function Editor_get_EnergyExtSources(self) result(EnergyExtSources)
                                                                                          
                                                                                          
!    Return the energy from external sources (EnergyExtSources)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: EnergyExtSources
                                                                                          
     EnergyExtSources = self % EnergyExtSources
                                                                                          
     return
  end function Editor_get_EnergyExtSources

!=======================================================================
! getEnergyCheck interface
!=======================================================================
                                                                                          
  function Editor_get_EnergyCheck(self) result(EnergyCheck)
                                                                                          
                                                                                          
!    Return the energy check or error (EnergyCheck)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: EnergyCheck
                                                                                          
     EnergyCheck = self % EnergyCheck
                                                                                          
     return
  end function Editor_get_EnergyCheck

!=======================================================================
! getDeltaEnergyRad interface
!=======================================================================
                                                                                          
  function Editor_get_DeltaEnergyRad(self) result(DeltaEnergyRad)
                                                                                          
                                                                                          
!    Return the change in radiation energy this cycle (DeltaEnergyRad)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: DeltaEnergyRad 
                                                                                          
     DeltaEnergyRad = self % DeltaEnergyRad 
                                                                                          
     return
  end function Editor_get_DeltaEnergyRad

!=======================================================================
! getDeltaEnergyMat interface
!=======================================================================
                                                                                          
  function Editor_get_DeltaEnergyMat(self) result(DeltaEnergyMat)
                                                                                          
                                                                                          
!    Return the change in material energy this cycle (DeltaEnergyMat)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: DeltaEnergyMat
                                                                                          
     DeltaEnergyMat = self % DeltaEnergyMat
                                                                                          
     return
  end function Editor_get_DeltaEnergyMat

!=======================================================================
! getDeltaEnergyInc interface
!=======================================================================
                                                                                          
  function Editor_get_DeltaEnergyInc(self) result(DeltaEnergyInc)
                                                                                          
                                                                                          
!    Return the change in incident energy this cycle (DeltaEnergyInc)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: DeltaEnergyInc
                                                                                          
     DeltaEnergyInc = self % DeltaEnergyInc
                                                                                          
     return
  end function Editor_get_DeltaEnergyInc

!=======================================================================
! getDeltaEnergyEsc interface
!=======================================================================
                                                                                          
  function Editor_get_DeltaEnergyEsc(self) result(DeltaEnergyEsc)
                                                                                          
                                                                                          
!    Return the change in escaping energy this cycle (DeltaEnergyEsc)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: DeltaEnergyEsc
                                                                                          
     DeltaEnergyEsc = self % DeltaEnergyEsc
                                                                                          
     return
  end function Editor_get_DeltaEnergyEsc

!=======================================================================
! getDeltaEnergyExtSrc interface
!=======================================================================
                                                                                          
  function Editor_get_DeltaEnergyExtSrc(self) result(DeltaEnergyExtSrc)
                                                                                          
                                                                                          
!    Return the change in external source energy this cycle (DeltaEnergyExtSrc)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: DeltaEnergyExtSrc
                                                                                          
     DeltaEnergyExtSrc = self % DeltaEnergyExtSrc
                                                                                          
     return
  end function Editor_get_DeltaEnergyExtSrc

!=======================================================================
! getDeltaEnergyCheck interface
!=======================================================================
                                                                                          
  function Editor_get_DeltaEnergyCheck(self) result(DeltaEnergyCheck)
                                                                                          
                                                                                          
!    Return the energy check for this cycle (DeltaEnergyCheck)
                                                                                          
!    variable declarations
     implicit none
                                                                                          
!    passed variables
     type(Editor), intent(in) :: self
     real(adqt)               :: DeltaEnergyCheck
                                                                                          
     DeltaEnergyCheck = self % DeltaEnergyCheck
                                                                                          
     return
  end function Editor_get_DeltaEnergyCheck


end module Editor_mod

