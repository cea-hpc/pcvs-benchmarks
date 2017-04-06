! BoundaryList Module:  Contains attributes for problem boundaries 

module BoundaryList_mod

  use kind_mod
  use Boundary_mod

  private

! public interfaces

  public construct, setBoundary, getNumberOfReflecting,  &
         getNumberOfBoundaries,                          &
         getNumberOfVacuum, getNumberOfShared,           &
         getNumberOfSource, getReflecting, getVacuum,    &
         getSource, getShared, getBoundary, destruct

  type, public :: BoundaryList

     integer                 :: NumBoundary        ! number of total boundaries
     integer                 :: NumReflecting      ! number of reflecting boundaries 
     integer                 :: NumVacuum          ! number of vacuum boundaries
     integer                 :: NumSource          ! number of source boundaries
     integer                 :: NumShared          ! number of shared boundaries

     integer                 :: PtrVac             ! points to vacuum boundary
     integer                 :: PtrSrc             ! points to source boundary
     integer                 :: PtrShared          ! points to shared boundary

     type(Boundary), pointer :: iBoundary(:)       ! boundary flux pointers

  end type BoundaryList

  type(BoundaryList), pointer, public :: RadBoundary


  interface construct
    module procedure BoundaryList_ctor
  end interface

  interface setBoundary 
    module procedure BoundaryList_set
  end interface

  interface getReflecting
    module procedure BoundaryList_getReflBdy
  end interface

  interface getVacuum
    module procedure BoundaryList_getVacBdy
  end interface

  interface getSource
    module procedure BoundaryList_getSrcBdy
  end interface

  interface getShared
    module procedure BoundaryList_getSharBdy
  end interface

  interface getBoundary
    module procedure BoundaryList_getBdy
  end interface

  interface getNumberOfBoundaries
    module procedure BoundaryList_getNum
  end interface

  interface getNumberOfReflecting
    module procedure BoundaryList_getNumRefl
  end interface

  interface getNumberOfVacuum
    module procedure BoundaryList_getNumVac
  end interface

  interface getNumberOfShared
    module procedure BoundaryList_getNumShared
  end interface

  interface getNumberOfSource
    module procedure BoundaryList_getNumSrc
  end interface

  interface destruct
    module procedure BoundaryList_dtor
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================

  subroutine BoundaryList_ctor(self, NumReflecting, &
                                     NumVacuum,     &
                                     NumSource,     &
                                     NumShared)

    implicit none

!   Passed variables

    type(BoundaryList), intent(inout) :: self
    integer, intent(in)               :: NumReflecting
    integer, intent(in)               :: NumVacuum
    integer, intent(in)               :: NumSource
    integer, intent(in)               :: NumShared 

!   Initialize type counters
                                                                                                    
    self % NumReflecting = NumReflecting 
    self % NumVacuum     = NumVacuum
    self % NumSource     = NumSource
    self % NumShared     = NumShared

    self % NumBoundary   = NumReflecting + NumVacuum +  &
                           NumSource     + NumShared 

    self % PtrVac        = NumReflecting
    self % PtrSrc        = NumReflecting + NumVacuum
    self % PtrShared     = NumReflecting + NumVacuum + NumSource

    allocate( self % iBoundary(self % NumBoundary) )

    return

  end subroutine BoundaryList_ctor

!=======================================================================
! set interface
!=======================================================================

  subroutine BoundaryList_set(self,        &
                              BoundaryID,  &
                              Type,        &
                              NumBdyElem,  &
                              BdyElem1,    &
                              ProfileID,   &
                              NeighborID,  &
                              EditID) 

    implicit none

!   Passed variables

    type(BoundaryList), intent(inout)      :: self
    integer, intent(in)                    :: BoundaryID
    integer, intent(in)                    :: NumBdyElem 
    integer, intent(in)                    :: BdyElem1 
    integer, intent(in)                    :: ProfileID 
    integer, intent(in)                    :: NeighborID
    integer, intent(in)                    :: EditID

    character(len=8), intent(in)           :: Type

    call construct(self % iBoundary(BoundaryID), &
                          Type,                  &
                          NumBdyElem,            &
                          BdyElem1,              &
                          ProfileID,             &
                          NeighborID,            &
                          EditID)

    return

  end subroutine BoundaryList_set

!=======================================================================
! getBoundary interface
!=======================================================================
  function BoundaryList_getBdy(self,BdyID) result(iBoundary)
                                                                                                  
                                                                                                  
!    Return a pointer to a boundary definition
                                                                                                  
!    variable declarations
     implicit none
                                                                                                  
     
!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer,            intent(in)   :: BdyID
     type(Boundary),     pointer      :: iBoundary
     
     
     
     iBoundary => self % iBoundary(BdyID)
     
     return
     
  end function BoundaryList_getBdy

!=======================================================================
! getReflectingBoundary interface
!=======================================================================
  function BoundaryList_getReflBdy(self,BdyID) result(iBoundary)
                                                                                                    
!    Return a pointer to a boundary definition 

!    variable declarations
     implicit none
                                                                                                    
!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer,            intent(in)   :: BdyID
     type(Boundary),     pointer      :: iBoundary
                                                                                                    
 
     iBoundary => self % iBoundary(BdyID)
 
     return
 
  end function BoundaryList_getReflBdy

!=======================================================================
! getVacuumBoundary interface
!=======================================================================
  function BoundaryList_getVacBdy(self,BdyID) result(iBoundary)

!    Return a pointer to a boundary definition

!    variable declarations
     implicit none

!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer,            intent(in)   :: BdyID
     type(Boundary),     pointer      :: iBoundary


     iBoundary => self % iBoundary(self%PtrVac + BdyID)

     return

  end function BoundaryList_getVacBdy

!=======================================================================
! getSourceBoundary interface
!=======================================================================
  function BoundaryList_getSrcBdy(self,BdyID) result(iBoundary)

!    Return a pointer to a boundary definition

!    variable declarations
     implicit none

!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer,            intent(in)   :: BdyID
     type(Boundary),     pointer      :: iBoundary


     iBoundary => self % iBoundary(self%PtrSrc + BdyID)

     return

  end function BoundaryList_getSrcBdy

!=======================================================================
! getSharedBoundary interface
!=======================================================================
  function BoundaryList_getSharBdy(self,BdyID) result(iBoundary)

!    Return a pointer to a boundary definition

!    variable declarations
     implicit none

!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer,            intent(in)   :: BdyID
     type(Boundary),     pointer      :: iBoundary


     iBoundary => self % iBoundary(self%PtrShared + BdyID)

     return

  end function BoundaryList_getSharBdy

!=======================================================================
! getNumberOfReflecting interface
!=======================================================================
  function BoundaryList_getNum(self) result(NumBoundary)

!    Return the number of reflecting boundaries
                                                                                                 
!    variable declarations
     implicit none

!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer                          :: NumBoundary
                                                                                                 
     NumBoundary = self % NumBoundary
                                                                                                 
     return
                                                                                                 
  end function BoundaryList_getNum

!=======================================================================
! getNumberOfReflecting interface
!=======================================================================
  function BoundaryList_getNumRefl(self) result(NumReflecting)
                                                                                                    
!    Return the number of reflecting boundaries 
                                                                                                    
!    variable declarations
     implicit none
                                                                                                    
!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer                          :: NumReflecting
                                                                                                    
     NumReflecting = self % NumReflecting
                                                                                                    
     return
                                                                                                    
  end function BoundaryList_getNumRefl

!=======================================================================
! getNumberOfVacuum interface
!=======================================================================
  function BoundaryList_getNumVac(self) result(NumVacuum)
                                                                                                    
!    Return the number of vacuum boundaries
                                                                                                    
!    variable declarations
     implicit none
                                                                                                    
!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer                          :: NumVacuum
                                                                                                    
     NumVacuum = self % NumVacuum 
                                                                                                    
     return
                                                                                                    
  end function BoundaryList_getNumVac

!=======================================================================
! getNumberOfShared interface
!=======================================================================
  function BoundaryList_getNumShared(self) result(NumShared)
                                                                                                    
!    Return the number of shared boundaries
                                                                                                    
!    variable declarations
     implicit none
                                                                                                    
!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer                          :: NumShared
                                                                                                    
     NumShared = self % NumShared
                                                                                                    
     return
                                                                                                    
  end function BoundaryList_getNumShared

!=======================================================================
! getNumberOfSource interface
!=======================================================================
  function BoundaryList_getNumSrc(self) result(NumSource)
                                                                                                    
!    Return the number of source boundaries
                                                                                                    
!    variable declarations
     implicit none
                                                                                                    
!    passed variables
     type(BoundaryList), intent(in)   :: self
     integer                          :: NumSource
                                                                                                    
     NumSource = self % NumSource
                                                                                                    
     return
                                                                                                    
  end function BoundaryList_getNumSrc

!=======================================================================
! destruct interface
!=======================================================================

  subroutine BoundaryList_dtor(self)

    implicit none

!   Passed variables

    type(BoundaryList),  intent(inout) :: self

!   Local

    integer :: i

!    do i=1,self % NumBoundary 
!      deallocate( self % iBoundary(i) )
!    enddo

    return

  end subroutine BoundaryList_dtor


end module BoundaryList_mod

