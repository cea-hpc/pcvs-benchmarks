! Geometry Modules:  geometry and connectivity information used by Sn
                                                                                 
module Size_mod 

  use kind_mod
  use constant_mod

  private

! public interfaces

  public construct
                                                                                 
  type, public :: MeshSize
     integer :: my_node   ! node ID
     integer :: nzones    ! number of zones
     integer :: ncornr    ! number of corners
     integer :: nfaces    ! number of zone faces
     integer :: npnts     ! number of points
     integer :: nbelem    ! number of boundary elements
     integer :: ndim      ! number of spatial dimensions
     integer :: maxcf     ! maximum number of zone faces a corner touches
     integer :: maxCorner ! maximum number of corners in a zone
     integer :: ngr       ! number of energy groups
     integer :: nangSN    ! number of angles used for SN sweeps (no acceleration)
     integer :: nangac    ! number of angles used for GTA sweeps
     integer :: npsi      ! number of angles in the flux array PSIR
     integer :: ncomm     ! number of processors to communicate with
     integer :: nbshare   ! number of elements on shared boundaries
     integer :: nbedit    ! number of boundary edits

     real(adqt)       :: tfloor   ! temperature floor
     real(adqt)       :: tmin     ! minimum temperature for time step/convergence control
     real(adqt)       :: wtiso    ! isotropic normalization factor
     real(adqt)       :: radForceMultiplier  ! radiation force multiplier
     real(adqt)       :: tau      ! reciprocal of timestep*SpeedOfLight
     real(adqt)       :: CommTimeCycle
     real(adqt)       :: CommTimeTotal

     character(len=8) :: igeom    ! geometry flag
     character(len=8) :: ittyp    ! time dependence flag
     character(len=8) :: iaccel   ! iterative acceleration flag
     character(len=8) :: iscat    ! scattering flag
     character(len=8) :: itimsrc  ! time-dependent source flag
     character(len=8) :: decomp_s ! spatial decomposition flag

  end type MeshSize 

  type(MeshSize), pointer, public :: Size

  interface construct
    module procedure Size_ctor
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================

  subroutine Size_ctor(self,my_node,                                &
                            nzones, ncornr, nfaces, npnts,          &
                            nbelem, ndim, maxcf, maxCorner, ngr,    &
                            nangSN, npsi, ncomm, nbshare, nbedit,   &
                            tfloor, tmin, radForceMultiplier,       & 
                            igeom, ittyp, iaccel, iscat, itimsrc,   &
                            decomp_s) 

    implicit none
                                                                                         
!   Passed variables

    type(MeshSize),    intent(inout) :: self

    integer, optional, intent(in) :: my_node
    integer, optional, intent(in) :: nzones
    integer, optional, intent(in) :: ncornr
    integer, optional, intent(in) :: nfaces
    integer, optional, intent(in) :: npnts
    integer, optional, intent(in) :: nbelem
    integer, optional, intent(in) :: ndim
    integer, optional, intent(in) :: maxcf
    integer, optional, intent(in) :: maxCorner
    integer, optional, intent(in) :: ngr
    integer, optional, intent(in) :: nangSN
    integer, optional, intent(in) :: npsi
    integer, optional, intent(in) :: ncomm
    integer, optional, intent(in) :: nbshare
    integer, optional, intent(in) :: nbedit
 
    real(adqt), optional, intent(in) :: tfloor
    real(adqt), optional, intent(in) :: tmin
    real(adqt), optional, intent(in) :: radForceMultiplier 
 
    character(len=8), optional, intent(in) :: igeom
    character(len=8), optional, intent(in) :: ittyp
    character(len=8), optional, intent(in) :: iaccel
    character(len=8), optional, intent(in) :: iscat
    character(len=8), optional, intent(in) :: itimsrc
    character(len=8), optional, intent(in) :: decomp_s

!   Problem Size Parameters
 
    self % my_node   = my_node
    self % nzones    = nzones
    self % ncornr    = ncornr
    self % nfaces    = nfaces
    self % npnts     = npnts
    self % nbelem    = nbelem
    self % ndim      = ndim
    self % maxcf     = maxcf
    self % maxCorner = maxCorner 
    self % ngr       = ngr
    self % nangSN    = nangSN 
    self % nangac    = 8 
    self % npsi      = npsi
    self % ncomm     = ncomm
    self % nbshare   = nbshare
    self % nbedit    = nbedit
 
    self % tfloor             = tfloor
    self % tmin               = tmin
    self % radForceMultiplier = radForceMultiplier
    self % tau                = zero
    self % CommTimeCycle      = zero
    self % CommTimeTotal      = zero

!  Angle-dependent variables are (by convention) per unit
!  cosine in 1D, per 2pi steradians in 2D and per 4pi
!  steradians in 3D.
                                                                                          
    select case (self % ndim)
      case (1)
        self % wtiso = half
      case (2)
        self % wtiso = one/(two*pi)
      case (3)
        self % wtiso = one/(four*pi)
    end select

 
    self % igeom    = igeom
    self % ittyp    = ittyp
    self % iaccel   = iaccel
    self % iscat    = iscat
    self % itimsrc  = itimsrc
    self % decomp_s = decomp_s

    return
   
  end subroutine Size_ctor
                                                                                 
                                                      
end module Size_mod

