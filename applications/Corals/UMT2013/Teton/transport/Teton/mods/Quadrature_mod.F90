! Quadrature Module:  Contains data structures describing angular quadrature 

module Quadrature_mod 

  use kind_mod
  use constant_mod
  use Communicator_mod

  private

! public interfaces

  public construct, getMessage, destruct, destructComm,  &
         constructExitList, getExitList, destructExitList, &
         restoreCommOrder, setCommOrder
                                                                                 
  type, public :: Exit
     integer             :: nExit             ! number of exiting boundary elements
     integer,    pointer :: ListExit(:,:)     ! pairs of boundary element and corner IDs
  end type Exit


  type, public :: Quadrature 

     integer              :: QuadID            ! quadrature ID
     integer              :: Groups            ! number of energy groups 
     integer              :: NumAngles         ! number of angles 
     integer              :: NumMoments        ! number of angular moments
     integer              :: Order             ! quadrature order 
     integer              :: NPolar            ! number of polar angles
     integer              :: NAzimuthal        ! number of azimuthal angles
     integer              :: PolarAxis         ! polar axis (1,2 or 3)

     character(len=8)     :: TypeName          ! quadrature type

     logical(kind=1), pointer  :: StartingDirection(:)
     logical(kind=1), pointer  :: FinishingDirection(:)

     real(adqt), pointer  :: Gnu(:)            ! energy group boundaries
     real(adqt), pointer  :: Omega(:,:)        ! direction cosines 
     real(adqt), pointer  :: Weight(:)         ! quadrature weights 

!    Spherical Harmonics
     real(adqt), pointer  :: Ynm(:,:)          ! Ynm(NumMoments,NumAngles)
     real(adqt), pointer  :: Pnm(:,:)          ! Pnm(NumMoments,NumAngles)

!    Angular coefficients used in 1D and 2D
     real(adqt), pointer  :: Alpha(:)          ! Alpha(NumAngles)
     real(adqt), pointer  :: Tau(:)            ! Tau(NumAngles)
     real(adqt), pointer  :: Falpha(:)         ! Falpha(NumAngles)
     real(adqt), pointer  :: Adweta(:)         ! Adweta(NumAngles)

!    Sweep and communication data structures
     integer             :: totcycles          ! total number of mesh cycles
     integer             :: NumBin0            !
     integer             :: NumBin             ! number of angle bins for communication
     integer             :: NangBin            ! maximun number of angles per bin

     integer,    pointer :: NangBinList(:)     ! NangBinList(NumBin)
     integer,    pointer :: AngleToBin(:)      ! AngleToBin(NumAngles) 
     integer,    pointer :: next(:,:)          ! next(ncornr+1,NumAngles)
     integer,    pointer :: nextZ(:,:)         ! nextZ(nzones,NumAngles)
     integer,    pointer :: AngleOrder(:,:)    ! AngleOrder(NangBin,NumBin)
     integer,    pointer :: RecvOrder0(:,:)    ! RecvOrder0(ncomm,NumBin)
     integer,    pointer :: SendOrder0(:)      ! SendOrder0(NumBin)
     integer,    pointer :: RecvOrder(:,:)     ! RecvOrder(ncomm,NumBin)
     integer,    pointer :: SendOrder(:)       ! SendOrder(NumBin)

     integer,    pointer :: fluxIterations(:)  ! fluxIterations(NumBin)
     real(adqt), pointer :: IncFlux(:)         ! IncFlux(NumBin)
     real(adqt), pointer :: IncFluxOld(:)      ! IncFluxOld(NumBin)
     real(adqt), pointer :: Flux(:,:)
     real(adqt), pointer :: relError(:)        ! relError(NumBin)
     logical(kind=1), pointer  :: Converged(:) ! Converged(NumBin)

     type(Communicator), pointer :: iComms(:,:) ! Pointers to communicators
     type(Exit),         pointer :: iExit(:)    ! Pointers to exiting boundary lists

  end type Quadrature 

  type(Quadrature), pointer, public :: QuadSet
  type(Exit),       pointer, public :: ExitBdy
!$OMP threadprivate (ExitBdy)

  interface construct
    module procedure Quadrature_ctor
  end interface

  interface getMessage 
    module procedure Quadrature_getComm
  end interface

  interface destruct
    module procedure Quadrature_dtor
  end interface

  interface destructComm
    module procedure Quadrature_dtorComm
  end interface

  interface constructExitList
    module procedure Quadrature_ctorExit
  end interface

  interface getExitList
    module procedure Quadrature_getExit
  end interface

  interface destructExitList
    module procedure Quadrature_dtorExit
  end interface

  interface restoreCommOrder
    module procedure Quadrature_resCommOrd
  end interface

  interface setCommOrder
    module procedure Quadrature_setCommOrd
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================
                                                                                   
  subroutine Quadrature_ctor(self,       &
                             QuadID,     &
                             Groups,     &
                             NumAngles,  &
                             NumMoments, &
                             Order,      &
                             NPolar,     &
                             NAzimuthal, &
                             PolarAxis,  &
                             TypeName,   &
                             Gnu) 

    use Size_mod

    implicit none

!   Passed variables

    type(Quadrature), intent(inout)    :: self

    integer, intent(in)                :: QuadID
    integer, intent(in)                :: Groups       
    integer, intent(in)                :: NumAngles
    integer, intent(in)                :: NumMoments
    integer, intent(in)                :: Order 
    integer, intent(in)                :: NPolar
    integer, intent(in)                :: NAzimuthal
    integer, intent(in)                :: PolarAxis
    character(len=8), intent(in)       :: TypeName
    real(adqt), intent(in)             :: Gnu(Groups+1)

!   Local

    integer          :: i, isctp1, Ndim, nlevel, Nang, delNang
    integer          :: bin, NangBin, angle0
    character(len=8) :: igeom

!   Set Properties

    self % QuadID     = QuadID
    self % Groups     = Groups 
    self % NumAngles  = NumAngles 
    self % NumMoments = NumMoments
    self % Order      = Order 
    self % NPolar     = NPolar
    self % NAzimuthal = NAzimuthal
    self % PolarAxis  = PolarAxis
    self % TypeName   = TypeName

    isctp1            = 1
    Ndim              = Size% ndim
    igeom             = Size% igeom


    allocate( self % Gnu(self % Groups + 1) )
    allocate( self % StartingDirection(self % NumAngles) )
    allocate( self % FinishingDirection(self % NumAngles) )
    allocate( self % Omega(Ndim,self % NumAngles) )
    allocate( self % Weight(self % NumAngles) )
    allocate( self % Ynm(self % NumMoments,self % NumAngles) )
    allocate( self % Pnm(self % NumMoments,self % NumAngles) )


    self % Gnu(:) = Gnu(:)

    call rtquad(self, Ndim, igeom)
    call snynmset(self, Ndim, isctp1)
    call snpnmset(self, Ndim, isctp1)

!   Space for sweep data structures

    self% totcycles = 0

    if (Ndim == 1) then
      self% NumBin  = 2
      self% NangBin = self% NumAngles/self% NumBin
    elseif (Ndim == 2) then
      if (self% TypeName == 'levelsym') then
        self% NumBin  = self% Order 
        self% NangBin = self% Order + 2
        delNang       = 2 
      elseif (self% TypeName == 'product') then
        self% NumBin  = 2*self% NPolar
        self% NangBin = self% NumAngles/self% NumBin
        delNang       = 0
      endif
    elseif (Ndim == 3) then
!     self% NumBin  = self% NumAngles
!     self% NangBin = 1
       self% NumBin    = 8 
       self% NangBin   = self% NumAngles/self% NumBin
    endif

    if (Ndim > 1) then
      allocate( self% NangBinList(self% NumBin) )
      allocate( self% AngleToBin(self% NumAngles) )
      allocate( self% next(Size%ncornr+1,self % NumAngles) )
      allocate( self% nextZ(Size%nzones,self % NumAngles) )
      allocate( self% AngleOrder(self% NangBin,self% NumBin) )
      allocate( self% RecvOrder0(self% NumBin,Size% ncomm) )
      allocate( self% SendOrder0(self% NumBin) )
      allocate( self% RecvOrder(self% NumBin,Size% ncomm) )
      allocate( self% SendOrder(self% NumBin) )

      allocate( self% fluxIterations(self% NumBin) )
      allocate( self% IncFlux(self% NumBin) )
      allocate( self% IncFluxOld(self% NumBin) )
      allocate( self% Flux(self% NumBin,Size% ncomm) )
      allocate( self% relError(self% NumBin) )
      allocate( self% Converged(self% NumBin) )

      allocate( self% iExit(self % NumAngles) )
      allocate( self% iComms(Size% ncomm,self% NumBin) )
    endif

!   Allow for a variable number of angles per bin

    if (Ndim == 2) then

      nlevel = self% NumBin/2
      Nang   = self% NangBin

      do i=1,nlevel
        self% NangBinList(2*i-1) = Nang
        self% NangBinList(2*i)   = Nang
        Nang                     = Nang - delNang
      enddo

    elseif (Ndim == 3) then
      self% NangBinList(:) = self% NangBin
    endif

!   Create a map from angle ID to bin ID

    angle0 = 0
    do bin=1,self% NumBin
      NangBin = self% NangBinList(bin)
      do i=1,NangBin
        self% AngleToBin(angle0+i) = bin
      enddo
      angle0 = angle0 + NangBin
    enddo
    
!   Space for angular coefficients

    if (Ndim == 1) then
      allocate( self % Alpha(self % NumAngles) )
      allocate( self % Tau(self % NumAngles) )
      allocate( self % Falpha(self % NumAngles) )
      allocate( self % Adweta(self % NumAngles) )

!!$      call AngleCoef1D(self, igeom)

    else if (Ndim == 2) then
      allocate( self % Alpha(self % NumAngles) )
      allocate( self % Tau(self % NumAngles) )

!!$      call AngleCoef2D(self)

    endif

    self% fluxIterations(:) = 0
    self% IncFlux(:)        = zero
    self% IncFluxOld(:)     = zero
    self% relError(:)       = zero
    self% Converged(:)      = .FALSE.

    self% Flux(:,:)         = zero

    self% NumBin0 = self% NumBin


    return

  end subroutine Quadrature_ctor

!-----------------------------------------------------------------------
  function Quadrature_getComm(self,bin,ishared) result(iComm)

!    Return a pointer to a communicator for this quadrature set
!      bin      angle bin number
!      ishared  number of neighbor
!      iComm    pointer to a communicator

!    variable declarations
     implicit none

!    passed variables
     type(Quadrature),  intent(in)   :: self
     integer,           intent(in)   :: bin, ishared 
     type(Communicator),   pointer   :: iComm

     iComm => self % iComms(ishared,bin)

     return

  end function Quadrature_getComm
                                                      
!=======================================================================
! destruct interface
!=======================================================================
                                                                                    
  subroutine Quadrature_dtor(self, Ndim)


    implicit none

!   Passed variables
                                                                                     
    type(Quadrature),  intent(inout) :: self
    integer,           intent(in)    :: Ndim

!   Local

    deallocate( self % Gnu )
    deallocate( self % StartingDirection )
    deallocate( self % FinishingDirection )
    deallocate( self % Omega )
    deallocate( self % Weight )
    deallocate( self % Ynm )
    deallocate( self % Pnm )

!   Space for sweep data structures
    if (Ndim > 1) then
      deallocate( self% NangBinList )
      deallocate( self% AngleToBin )
      deallocate( self% next )
      deallocate( self% nextZ )
      deallocate( self% AngleOrder )
      deallocate( self% RecvOrder0 )
      deallocate( self% SendOrder0 )
      deallocate( self% RecvOrder )
      deallocate( self% SendOrder )
      deallocate( self% fluxIterations )
      deallocate( self% IncFlux )
      deallocate( self% IncFluxOld )
      deallocate( self% relError )
      deallocate( self% Converged )
      deallocate( self% Flux )
      deallocate( self% iComms )
      deallocate( self% iExit )
    endif

!   Space for angular coefficients
    if (Ndim == 1 .or. Ndim == 2) then
      deallocate( self % Alpha )
      deallocate( self % Tau )
      deallocate( self % Falpha )
      deallocate( self % Adweta )
    else if (Ndim == 2) then
      deallocate( self % Alpha )
      deallocate( self % Tau )
    endif


    return

  end subroutine Quadrature_dtor

!=======================================================================
! destruct communicator interface
!=======================================================================
  subroutine Quadrature_dtorComm(self)
                                                                                            
    use Size_mod
                                                                                            
    implicit none
                                                                                            
!   Passed variables
    type(Quadrature),  intent(inout) :: self

!   Local
    type(Communicator), pointer :: Comm
    integer                     :: i, bin
                                                                                            
!   Free Communication Requests
                                                                                            
    if (Size% decomp_s == 'on') then
                                                                                            
      do bin=1,self% NumBin
        do i=1,Size% ncomm
          Comm => getMessage(self, bin, i)
          call destructBuffer(Comm)
        enddo
      enddo
                                                                                            
    endif
                                                                                            
    return

  end subroutine Quadrature_dtorComm

!=======================================================================
! construct Exit List interface
!=======================================================================
  subroutine Quadrature_ctorExit(self, angleID, nExit, ListExit)
                                                                                                 
    implicit none
                                                                                                 
!   Passed variables
    type(Quadrature),  intent(inout) :: self
    integer,           intent(in)    :: angleID
    integer,           intent(in)    :: nExit
    integer,           intent(in)    :: ListExit(2,nExit)
                                                                                                 
!   Local
    type(Exit), pointer              :: iExit 


    iExit => self% iExit(angleID)

    allocate( iExit% ListExit(2,nExit) )

    iExit% nExit               = nExit
    iExit% ListExit(:,1:nExit) = ListExit(:,1:nExit)

    return
                                                                                                 
  end subroutine Quadrature_ctorExit

!=======================================================================
! restore Communication order 
!=======================================================================
  subroutine Quadrature_resCommOrd(self)
                                                                                                  
    implicit none
                                                                                                  
!   Passed variables
    type(Quadrature),  intent(inout) :: self


    self% NumBin         = self% NumBin0
    self% SendOrder(:)   = self% SendOrder0(:)
    self% RecvOrder(:,:) = self% RecvOrder0(:,:)
    self% Converged(:)   = .FALSE.


    return
                                                                                                  
  end subroutine Quadrature_resCommOrd

!=======================================================================
! set Communication order
!=======================================================================
  subroutine Quadrature_setCommOrd(self)
                                                                                                  
    use Size_mod

    implicit none
                                                                                                  
!   Passed variables
    type(Quadrature),  intent(inout) :: self
                                                                                                  
!   Local
    integer  :: bin, i, ishared, nrecv

!   Set the send and receive orders based on a convergence test

    self% NumBin = 0 

    do i=1,self% NumBin0
      bin = self% SendOrder0(i)
      if ( .not. self% Converged(bin) ) then
        self% NumBin                  = self% NumBin + 1
        self% SendOrder(self% NumBin) = bin 
      endif
    enddo

    do ishared=1,Size% ncomm
      nrecv = 0
      do i=1,self% NumBin0
        bin = self% RecvOrder0(i,ishared)
        if ( .not. self% Converged(bin) ) then
          nrecv                          = nrecv + 1
          self% RecvOrder(nrecv,ishared) = bin 
        endif
      enddo
      if (nrecv /= self% NumBin) then
        call f90fatal("Mismatch of send and receive counts in setCommOrder")
      endif
    enddo

!   Check the no-op case

    if (self% NumBin == self% NumBin0) then
      do i=1,self% NumBin0
        if (self% SendOrder(i) /= self% SendOrder0(i)) then
          call f90fatal("No-op case failed for SendOrder")
        endif
      enddo

      do ishared=1,Size% ncomm
        do i=1,self% NumBin0
          if (self% RecvOrder(i,ishared) /= self% RecvOrder0(i,ishared)) then
            call f90fatal("No-op case failed for RecvOrder")
          endif
        enddo
      enddo

    endif
                                                                                                  
    return
                                                                                                  
  end subroutine Quadrature_setCommOrd

!=======================================================================
! get Exit List interface
!=======================================================================
  function Quadrature_getExit(self,angleID) result(iExit)
                                                                                                 
!    Return a pointer to the exit list for this angle in the
!    quadrature set
!      angleID  angle ID number
!      iExit    pointer to an exit list
                                                                                                 
!    variable declarations
     implicit none
                                                                                                 
!    passed variables
     type(Quadrature),  intent(in)   :: self
     integer,           intent(in)   :: angleID 
     type(Exit),        pointer      :: iExit
                                                                                                 
     iExit => self % iExit(angleID)
                                                                                                 
     return
                                                                                                 
  end function Quadrature_getExit

!=======================================================================
! destruct Exit List interface
!=======================================================================
  subroutine Quadrature_dtorExit(self)
                                                                                                   
    implicit none
                                                                                                   
!   Passed variables
    type(Quadrature),  intent(inout) :: self

!   Local
    type(Exit), pointer              :: iExit
    integer                          :: angleID 
       
       
    do angleID=1,self% NumAngles
      iExit => self% iExit(angleID)
      deallocate( iExit% ListExit )
    enddo
       
       
    return
       
  end subroutine Quadrature_dtorExit



end module Quadrature_mod

