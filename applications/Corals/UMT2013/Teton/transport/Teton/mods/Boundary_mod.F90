! Boundary Module:  Contains data structures for boundary fluxes 

module Boundary_mod 

  use kind_mod

  private

! public interfaces

  public construct, destruct, getNumberOfBdyElements,     &
         getFirstBdyElement, getProfileID, getNeighborID, &
         getEditID, getType, constructFlux, destructFlux, &
         constructReflectedAngle, setReflectedAngle,      &
         getReflectedAngle

  type, public :: ReflectedAngle
     integer,    pointer  :: ReflAngle(:)      ! list of angle IDs
  end type ReflectedAngle
                                                                                 
  type, public :: Boundary 

     integer              :: NumBdyElem        ! number of boundary elements 
     integer              :: BdyElem1          ! index of first boundary element
     integer              :: ProfileID         ! source profile ID 
     integer              :: NeighborID        ! shared process ID
     integer              :: EditID            ! edit ID

     character(len=8)     :: Type              ! boundary type 

     integer, pointer     :: BdyToC(:)         ! BdyToC(NumBdyElem)

     real(adqt), pointer  :: A_bdy(:,:)        ! A_bdy(ndim,NumBdyElem)
     real(adqt), pointer  :: Radius(:)         ! Radius(NumBdyElem)
     real(adqt), pointer  :: Psi(:,:)          ! Psi(Groups,NumBdyElem)

     type(ReflectedAngle), pointer :: iRef(:)  ! pointers to reflected angle data

  end type Boundary 

  type(Boundary), pointer, public :: Bdy
!$OMP threadprivate(Bdy)

  interface construct
    module procedure Boundary_ctor
  end interface

  interface getNumberOfBdyElements
    module procedure Boundary_getNumBdyElem
  end interface

  interface getFirstBdyElement
    module procedure Boundary_getFirstBE
  end interface

  interface getProfileID
    module procedure Boundary_getProf
  end interface

  interface getNeighborID
    module procedure Boundary_getNeigh
  end interface

  interface getEditID
    module procedure Boundary_getEdit
  end interface

  interface getType
    module procedure Boundary_getType
  end interface

  interface constructFlux
    module procedure Boundary_ctor_flux
  end interface

  interface destructFlux
    module procedure Boundary_dtor_flux
  end interface

  interface constructReflectedAngle
    module procedure Boundary_ctorRef
  end interface

  interface destructReflectedAngle
    module procedure Boundary_dtorRef
  end interface
                                                                                                   
  interface setReflectedAngle
    module procedure Boundary_setRef
  end interface
                                                                                                   
  interface getReflectedAngle
    module procedure Boundary_getRef
  end interface

  interface destruct
    module procedure Boundary_dtor
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================
                                                                                   
  subroutine Boundary_ctor(self,        &
                           Type,        &
                           NumBdyElem,  &
                           BdyElem1,    &
                           ProfileID,   &
                           NeighborID,  &
                           EditID)

    use Size_mod
    use QuadratureList_mod

    implicit none

!   Passed variables

    type(Boundary), intent(inout)  :: self

    integer, intent(in)           :: NumBdyElem         
    integer, intent(in)           :: BdyElem1
    integer, intent(in)           :: ProfileID       
    integer, intent(in)           :: NeighborID
    integer, intent(in)           :: EditID

    character(len=8), intent(in)  :: Type         

!   Local

    integer                       :: NumQuadSets 

!   Set Properties

    self % NumBdyElem = NumBdyElem 
    self % BdyElem1   = BdyElem1
    self % ProfileID  = ProfileID 
    self % NeighborID = NeighborID
    self % EditID     = EditID
    self % Type       = Type

    allocate( self % BdyToC(self% NumBdyElem) )
    allocate( self % A_bdy(Size%ndim,self% NumBdyElem) )

    if (Size%ndim == 2) then
      allocate( self % Radius(self% NumBdyElem) )
    endif

    if (self % Type == 'refl') then
      NumQuadSets = getNumQuadSets(Quad)
      allocate( self % iRef(NumQuadSets) )
    endif


    return

  end subroutine Boundary_ctor

!=======================================================================
! construct flux interface
!=======================================================================
                                                                                                  
  subroutine Boundary_ctor_flux(self, Groups)

    implicit none
                                                                                                  
!   Passed variables
                                                                                                  
    type(Boundary), intent(inout)  :: self
                                                                                                  
    integer, intent(in)            :: Groups 

!   Allocate space 
                                                                                                  
!    allocate( self % Psi(Groups,self% NumBdyElem) )

    return
                                                                                                  
  end subroutine Boundary_ctor_flux

!=======================================================================
! destruct flux interface
!=======================================================================
                                                                                                  
  subroutine Boundary_dtor_flux(self)
                                                                                                  
    implicit none
                                                                                                  
!   Passed variables
                                                                                                  
    type(Boundary), intent(inout)  :: self
                                                                                                  
!   Free space
                                                                                                  
!    deallocate( self % Psi )
                                                                                                  
    return
                                                                                                  
  end subroutine Boundary_dtor_flux

!=======================================================================
! construct Reflected Angle interface
!=======================================================================
                                                                                                   
  subroutine Boundary_ctorRef(self)
                                                                                                   
    use QuadratureList_mod
    use Quadrature_mod

    implicit none
                                                                                                   
!   Passed variables
    type(Boundary), intent(inout)  :: self

!   Local
    integer                        :: NumQuadSets
    integer                        :: set
    type(ReflectedAngle), pointer  :: iRef
                                                                                                   
!   Allocate space

    NumQuadSets = getNumQuadSets(Quad)

    do set=1,NumQuadSets
      QuadSet => getQuadrature(Quad,set)
      iRef    => self% iRef(set)

      allocate( iRef% ReflAngle(QuadSet% NumAngles) )

      iRef% ReflAngle(:) = -1
    enddo

    return
                                                                                                   
  end subroutine Boundary_ctorRef

!=======================================================================
! destruct Reflected Angle interface
!=======================================================================
                                                                                                   
  subroutine Boundary_dtorRef(self)

    use QuadratureList_mod

    implicit none
                                                                                                   
!   Passed variables
    type(Boundary), intent(inout)  :: self

!   Local
    integer                        :: NumQuadSets
    integer                        :: set
    type(ReflectedAngle), pointer  :: iRef

!   Release space

    NumQuadSets = getNumQuadSets(Quad)

    do set=1,NumQuadSets
      iRef    => self% iRef(set)
                                                                                                   
      deallocate( iRef% ReflAngle )
    enddo
                                                                                                   
    return
                                                                                                   
  end subroutine Boundary_dtorRef

!=======================================================================
! get Reflected Angle interface
!=======================================================================
                                                                                                   
  function Boundary_getRef(self, IncAngle) result(ReflAngle)
                                                                                                   
    use Quadrature_mod

    implicit none
                                                                                                   
!   Passed variables
                                                                                                   
    type(Boundary),       intent(inout)  :: self
    integer,              intent(in)     :: IncAngle
    integer                              :: ReflAngle

!   Local
    integer                              :: set
    type(ReflectedAngle), pointer        :: iRef

                                                                                                   
    set  =  QuadSet% QuadID
    iRef => self% iRef(set)

    ReflAngle = iRef% ReflAngle(IncAngle)
                                                                                                   
    return
                                                                                                   
  end function Boundary_getRef

!=======================================================================
! set Reflected Angle interface
!=======================================================================
                                                                                                   
  subroutine Boundary_setRef(self, IncAngle, ReflAngle)
                                                                                                   
    use Quadrature_mod

    implicit none
                                                                                                   
!   Passed variables
                                                                                                   
    type(Boundary),       intent(inout)  :: self
    integer,              intent(in)     :: IncAngle
    integer,              intent(in)     :: ReflAngle

!   Local
    integer                              :: set
    type(ReflectedAngle), pointer        :: iRef
                                                                                                   
    set  =  QuadSet% QuadID
    iRef => self% iRef(set)                                                                                                 
    iRef% ReflAngle(IncAngle) = ReflAngle
                                                                                                   
    return
                                                                                                   
  end subroutine Boundary_setRef

!=======================================================================
! getNumberOfBdyElements interface
!=======================================================================
  function Boundary_getNumBdyElem(self) result(NumBdyElem)

!    Returns the number of boundary elements 

!    variable declarations
     implicit none

!    passed variables
     type(Boundary), intent(in) :: self
     integer                    :: NumBdyElem

     NumBdyElem = self% NumBdyElem

     return

  end function Boundary_getNumBdyElem 

!=======================================================================
! getFirstBdyElement interface
!=======================================================================
  function Boundary_getFirstBE(self) result(BdyElem1)
                                                                                                    
!    Returns the first boundary element
                                                                                                    
!    variable declarations
     implicit none
                                                                                                    
!    passed variables
     type(Boundary), intent(in) :: self
     integer                    :: BdyElem1 
                                                                                                    
     BdyElem1 = self% BdyElem1 
                                                                                                    
     return
                                                                                                    
  end function Boundary_getFirstBE

!=======================================================================
! getProfileID interface
!=======================================================================
  function Boundary_getProf(self) result(ProfileID)
                                                                                                    
!    Returns the profile ID 
                                                                                                    
!    variable declarations
     implicit none
                                                                                                    
!    passed variables
     type(Boundary), intent(in) :: self
     integer                    :: ProfileID 
                                                                                                    
     ProfileID = self% ProfileID 
                                                                                                    
     return
                                                                                                    
  end function Boundary_getProf

!=======================================================================
! getNeighborID interface
!=======================================================================
  function Boundary_getNeigh(self) result(NeighborID)
                                                                                                    
!    Returns the NeighborID
                                                                                                    
!    variable declarations
     implicit none
                                                                                                    
!    passed variables
     type(Boundary), intent(in) :: self
     integer                    :: NeighborID
                                                                                                    
     NeighborID = self% NeighborID
                                                                                                    
     return
                                                                                                    
  end function Boundary_getNeigh

!=======================================================================
! getEditID interface
!=======================================================================
  function Boundary_getEdit(self) result(EditID)

!    Returns the EditID

!    variable declarations
     implicit none

!    passed variables
     type(Boundary), intent(in) :: self
     integer                    :: EditID

     EditID = self% EditID

     return

  end function Boundary_getEdit

!=======================================================================
! getType interface
!=======================================================================
  function Boundary_getType(self) result(Type)

!    Returns the type 
                                                                                                 
!    variable declarations
     implicit none
                                                                                                 
!    passed variables
     type(Boundary), intent(in) :: self
     character(len=8)           :: Type 
                                                                                                 
     Type = self% Type 
                                                                                                 
     return
                                                                                                 
  end function Boundary_getType

!=======================================================================
! destruct interface
!=======================================================================
                                                                                    
  subroutine Boundary_dtor(self)


    implicit none

!   Passed variables
                                                                                     
    type(Boundary),  intent(inout) :: self

!   Local

    deallocate( self % BdyToC )
    deallocate( self % A_bdy  )
    deallocate( self % Radius )


    return

  end subroutine Boundary_dtor

end module Boundary_mod

