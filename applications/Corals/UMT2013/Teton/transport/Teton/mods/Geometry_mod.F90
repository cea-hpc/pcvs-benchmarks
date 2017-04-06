! Geometry Modules:  geometry and connectivity information used by Sn
                                                                                 
module Geometry_mod 

  use kind_mod
  use ZoneData_mod

  private

! public interfaces
                                                                                             
  public construct, destruct, getZoneData
                                                                                 
                                                                                 
  type, public :: Geometry 

     real(adqt), pointer :: px(:,:)           ! px(ndim,npnts)  - coordinates of zone vertices
     real(adqt), pointer :: volc(:)           ! volc(ncornr)    - corner volumes

!    1D & 2D Specific Arrays
     real(adqt), pointer :: areac(:)          ! areac(ncornr)
     real(adqt), pointer :: rj2(:)            ! rj2(nzones)
     real(adqt), pointer :: r2(:)             ! r2(nzones+1)
     real(adqt), pointer :: hj(:)             ! hj(nzones)

     integer, pointer :: CToFace(:,:)         ! CToFace(maxcf,ncornr)
     integer, pointer :: CToZone(:)           ! CToZone(ncornr)
     integer, pointer :: CToPoint(:)          ! CToPoint(ncornr)
     integer, pointer :: ZoneToSrc(:)         ! ZoneToSrc(nzones)
     integer, pointer :: nfpc(:)              ! nfpc(ncornr)

     type(ZoneData), pointer :: ZData(:)      ! zone data pointers

  end type Geometry

  type(Geometry), pointer, public :: Geom

  interface construct
    module procedure Geometry_ctor
  end interface

  interface getZoneData
    module procedure Geometry_getZone
  end interface

  interface destruct
    module procedure Geometry_dtor
  end interface

contains

!=======================================================================
! construct interface
!=======================================================================

  subroutine Geometry_ctor(self)

    use Size_mod

    implicit none

!   Passed variables

    type(Geometry),  intent(inout) :: self

!!$    allocate( self % px(Size%ndim,Size%npnts) )
    allocate( self % volc(Size%ncornr) )

!   Geometry Specific Arrays

    if (Size%ndim == 1) then
      allocate( self % rj2(Size%nzones) )
      allocate( self % r2(Size%nzones+1) )
      allocate( self % hj(Size%nzones) )
      allocate( self % areac(Size%ncornr) )
    endif

!   Mesh Connectivity

    allocate( self % CToFace(Size%maxcf,Size%ncornr) )
    allocate( self % CToZone(Size%ncornr) )
    allocate( self % CToPoint(Size%ncornr) )
    allocate( self % ZoneToSrc(Size%nzones) )
    allocate( self % nfpc(Size%ncornr) )

!   Pointers

    allocate( self % ZData(Size%nzones) )


    return
                                                                                             
  end subroutine Geometry_ctor

!=======================================================================
! get Zone Data interface
!=======================================================================
  function Geometry_getZone(self,zoneID) result(ZData)
 
!    Return a pointer to a zone definition
 
!    variable declarations
     implicit none
 
!    passed variables
     type(Geometry),     intent(in)   :: self
     integer,            intent(in)   :: zoneID
     type(ZoneData),     pointer      :: ZData
 
     ZData => self % ZData(zoneID)
 
     return
 
  end function Geometry_getZone
                                                                                             
!=======================================================================
! destruct interface
!=======================================================================

  subroutine Geometry_dtor(self)

    use Size_mod

    implicit none

!   Passed variables

    type(Geometry),  intent(inout) :: self


!!$    deallocate( self % px )
    deallocate( self % volc )

!   Geometry Specific Arrays

    if (Size%ndim == 1) then
      deallocate( self % rj2 )
      deallocate( self % r2 )
      deallocate( self % hj )
      deallocate( self % areac )
    endif

!   Mesh Connectivity

    deallocate( self % CToFace )
    deallocate( self % CToZone )
    deallocate( self % CToPoint )
    deallocate( self % ZoneToSrc )
    deallocate( self % nfpc )


    return

  end subroutine Geometry_dtor

                                                      
end module Geometry_mod

