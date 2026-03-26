! Material Module:  material properties 
                                                                                 
module Material_mod
                                                                                 
  use kind_mod

  private

! public interfaces
                                                                                             
  public construct, destruct
                                                                                 
  type, public :: Material

     real(adqt), pointer :: siga(:,:)            ! siga(ngr,nzones) 
     real(adqt), pointer :: sigs(:,:)            ! sigs(ngr,nzones)
     real(adqt), pointer :: cve(:)               ! cve(nzones) 
     real(adqt), pointer :: rho(:)               ! rho(nzones)
     real(adqt), pointer :: SMatEff(:)           ! SMatEff(nzones)
     real(adqt), pointer :: qext(:,:)            ! qext(ngr,ncornr)
     real(adqt), pointer :: denez(:)             ! denez(nzones)
     real(adqt), pointer :: trz(:)               ! trz(nzones)
     real(adqt), pointer :: tez(:)               ! tez(nzones)
     real(adqt), pointer :: trzn(:)              ! trzn(nzones)
     real(adqt), pointer :: tezn(:)              ! tezn(nzones)
     real(adqt), pointer :: denec(:)             ! denec(ncornr)
     real(adqt), pointer :: tec(:)               ! tec(ncornr)

     real(adqt), pointer :: decompton(:)         ! decompton(ncornr)
     real(adqt), pointer :: etac(:)              ! etac(ncornr)
     real(adqt), pointer :: tecn(:)              ! tecn(ncornr)
     real(adqt), pointer :: tezold(:)            ! tezold(nzones)
     real(adqt), pointer :: AbsorptionRate(:)    ! AbsorptionRate(ncornr)
     real(adqt), pointer :: AbsorptionRateOld(:) ! AbsorptionRateOld(ncornr)
     real(adqt), pointer :: SFixed(:,:)          ! SFixed(ngr,ncornr)

  end type Material

  type(Material), pointer, public :: Mat

  interface construct
    module procedure Material_ctor
  end interface
 
  interface destruct
    module procedure Material_dtor
  end interface
 
contains
 
!=======================================================================
! construct interface
!=======================================================================

  subroutine Material_ctor(self)
 
    use Size_mod
 
    implicit none
 
!   Passed variables
 
    type(Material),    intent(inout) :: self

!!$    allocate( self % siga(Size%ngr,Size%nzones) )
!!$    allocate( self % sigs(Size%ngr,Size%nzones) )
!!$    allocate( self % cve(Size%nzones) ) 
!!$    allocate( self % rho(Size%nzones) )
!!$    allocate( self % SMatEff(Size%nzones) )
    allocate( self % qext(Size%ngr,Size%ncornr) )
!!$    allocate( self % denez(Size%nzones) ) 
!!$    allocate( self % trz(Size%nzones) )
!!$    allocate( self % tez(Size%nzones) )
    allocate( self % trzn(Size%nzones) )
    allocate( self % tezn(Size%nzones) )
    allocate( self % denec(Size%ncornr) )
!!$    allocate( self % tec(Size%ncornr) ) 

    allocate( self % decompton(Size%ncornr) )
    allocate( self % etac(Size%ncornr) )
    allocate( self % tecn(Size%ncornr) )
    allocate( self % tezold(Size%nzones) )
    allocate( self % AbsorptionRate(Size%ncornr) )
    allocate( self % AbsorptionRateOld(Size%ncornr) )
    allocate( self % SFixed(Size%ngr,Size%ncornr) )
 

    return
 
  end subroutine Material_ctor

!=======================================================================
! destruct interface
!=======================================================================
                                                            
  subroutine Material_dtor(self)
                                      
    use Size_mod
                
    implicit none
                 
!   Passed variables
                    
    type(Material),    intent(inout) :: self

!!$    deallocate( self % siga )
!!$    deallocate( self % sigs )
!!$    deallocate( self % cve )
!!$    deallocate( self % rho )
!!$    deallocate( self % SMatEff )
    deallocate( self % qext )
!!$    deallocate( self % denez )
!!$    deallocate( self % trz )
!!$    deallocate( self % tez )
    deallocate( self % trzn )
    deallocate( self % tezn )
    deallocate( self % denec )
!!$    deallocate( self % tec )

    deallocate( self % decompton )
    deallocate( self % etac )
    deallocate( self % tecn )
    deallocate( self % tezold )
    deallocate( self % AbsorptionRate )
    deallocate( self % AbsorptionRateOld )
    deallocate( self % SFixed )

    return
          
  end subroutine Material_dtor


                                                      
end module Material_mod

