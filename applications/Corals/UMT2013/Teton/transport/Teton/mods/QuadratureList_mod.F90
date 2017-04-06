! QuadratureList Module:  Contains a list of source profiles

module QuadratureList_mod

  use kind_mod
  use Quadrature_mod

  private

! public interfaces

  public construct, destruct, setQuadrature,           &
         getQuadrature, getNumQuadSets, getNumSnSets,  &
         getGTAQuadrature, getEnergyGroups

  type, public :: QuadratureList

     private
     integer                     :: NumQuadSets  ! Total number of quadrature sets 
     integer                     :: NumSnSets    ! Number of sets used for Sn sweeps
     type(Quadrature),   pointer :: iQuads(:)    ! Pointers to quadrature sets

  end type QuadratureList

  type(QuadratureList), pointer, public :: Quad

  interface construct
    module procedure QuadratureList_ctor
  end interface

  interface setQuadrature 
    module procedure QuadratureList_set
  end interface

  interface getQuadrature
    module procedure QuadratureList_getQuad
  end interface

  interface getNumQuadSets
    module procedure QuadratureList_getNumQuadSets
  end interface

  interface getNumSnSets
    module procedure QuadratureList_getNumSnSets
  end interface

  interface getGTAQuadrature
    module procedure QuadratureList_getGTAQuad
  end interface

  interface getEnergyGroups
    module procedure QuadratureList_getEnergyGroups
  end interface

  interface destruct
    module procedure QuadratureList_dtor
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================

  subroutine QuadratureList_ctor(self, NumQuadSets)

    implicit none

!   Passed variables

    type(QuadratureList), intent(inout) :: self
    integer, intent(in)                 :: NumQuadSets 


    self % NumQuadSets = NumQuadSets
!    self % NumSnSets   = NumQuadSets - 1 
    self % NumSnSets   = 1

    allocate( self % iQuads(self % NumQuadSets) )


    return

  end subroutine QuadratureList_ctor

!=======================================================================
! set interface
!=======================================================================

  subroutine QuadratureList_set(self,          &
                                QuadID,        &
                                Groups,        &
                                NumAngles,     &
                                NumMoments,    &
                                Order,         &
                                NPolar,        &
                                NAzimuthal,    &
                                PolarAxis,     &
                                Type,          &
                                Gnu)

    implicit none

!   Passed variables

    type(QuadratureList), intent(inout) :: self

    integer,    intent(in)              :: QuadID
    integer,    intent(in)              :: Groups
    integer,    intent(in)              :: NumAngles
    integer,    intent(in)              :: NumMoments
    integer,    intent(in)              :: Order
    integer,    intent(in)              :: NPolar
    integer,    intent(in)              :: NAzimuthal
    integer,    intent(in)              :: PolarAxis
    integer,    intent(in)              :: Type
    real(adqt), intent(in)              :: Gnu(Groups+1)

!   Local

    character(len=8) :: TypeName

    select case (Type)
      case (1)
        TypeName = 'levelsym'
      case (2)
        TypeName = 'product'
      case (3)
        TypeName = 'lobatto'
    end select 
 
    call construct(self % iQuads(QuadID), &
                          QuadID,         &
                          Groups,         &
                          NumAngles,      &
                          NumMoments,     &
                          Order,          &
                          NPolar,         &
                          NAzimuthal,     &
                          PolarAxis,      &
                          TypeName,       &
                          Gnu)

    return

  end subroutine QuadratureList_set

!=======================================================================
! destruct interface
!=======================================================================

  subroutine QuadratureList_dtor(self)

    implicit none

!   Passed variables

    type(QuadratureList),  intent(inout) :: self

!   Local

    integer :: i

!    do i=1,self % NumQuadSets
!      deallocate( self % iQuads(i) )
!    enddo

    return

  end subroutine QuadratureList_dtor

!-----------------------------------------------------------------------
  function QuadratureList_getNumQuadSets(self) result(NumQuadSets)
                                                                                            
!    Returns the number of unique quadrature sets
!      NumQuadSets   number of quadrature sets

!    variable declarations
     implicit none
                                                                                            
!    passed variables
     type(QuadratureList), intent(in) :: self
     integer                          :: NumQuadSets

     NumQuadSets = self% NumQuadSets
                                                                                            
     return
                                                                                            
  end function QuadratureList_getNumQuadSets

!-----------------------------------------------------------------------
  function QuadratureList_getNumSnSets(self) result(NumSnSets)
                                                                                            
!    Returns the number of non-acceleration quadrature sets
!      NumSnSets   number of Sn sets
                                                                                            
!    variable declarations
     implicit none
                                                                                            
!    passed variables
     type(QuadratureList), intent(in) :: self
     integer                          :: NumSnSets
                                                                                            
     NumSnSets = self% NumSnSets
                                                                                            
     return
                                                                                            
  end function QuadratureList_getNumSnSets

!-----------------------------------------------------------------------
  function QuadratureList_getQuad(self,QuadID) result(iQuad)

!    Return a pointer to a quadrature set 
!      QuadID   quadrature set ID number 
!      iQuad    pointer to the quadrature set 

!    variable declarations
     implicit none

!    passed variables
     type(QuadratureList), intent(in) :: self
     integer,              intent(in) :: QuadID 
     type(Quadrature),     pointer    :: iQuad

            
     iQuad => self % iQuads(QuadID)
            
     return

  end function QuadratureList_getQuad 

!-----------------------------------------------------------------------
  function QuadratureList_getGTAQuad(self) result(iQuad)
                                                                                           
!    Return a pointer to the GTA quadrature set
!      iQuad    pointer to the quadrature set
                                                                                           
!    variable declarations
     implicit none
                                                                                           
!    passed variables
     type(QuadratureList), intent(in) :: self
     type(Quadrature),     pointer    :: iQuad
                                                                                           
                                                                                           
     iQuad => self % iQuads(self % NumSnSets + 1)
                                                                                           
     return
                                                                                           
  end function QuadratureList_getGTAQuad

!-----------------------------------------------------------------------
  function QuadratureList_getEnergyGroups(self,numGroups) result(GrpBnds)
                                                                                            
!    Returns all energy group bounds
!      GrpBnds    array of energy group bounds 
                                                                                            
!    variable declarations
     implicit none
                                                                                            
!    passed variables
     type(QuadratureList), intent(in) :: self
     integer, intent(in)              :: numGroups

!    Local
     type(Quadrature), pointer    :: iQuad
     integer                      :: i, ig, ng
     real(adqt)                   :: GrpBnds(numGroups+1)
                                                                                            
     ng = 0
     do i=1,self % NumSnSets
       iQuad => self % iQuads(i)
       do ig=1,iQuad% Groups+1
         GrpBnds(ng+ig) = iQuad% Gnu(ig)
       enddo
       ng = ng + iQuad% Groups
     enddo
                                                                                            
     return
                                                                                            
  end function QuadratureList_getEnergyGroups



end module QuadratureList_mod

