!***********************************************************************
!                         Version 0: 04/06 PFN                         *
!                                                                      *
!    SetMaterialModule -  Called from Teton.cc to associate pointers   *
!                         in the MaterialModule with memory allocated  *   
!                         in C++.                                      *
!                                                                      *
!    Input:   Size  - structure containing mesh parameters             *
!             Mat   - structure containing material arrays             *
!                                                                      *
!    Output:                                                           *
!                                                                      *
!***********************************************************************
   subroutine setMaterialModule(siga, sigs, cve, rho,      &
                                SMatEff, denez, trz, tez, tec)

   use kind_mod
   use Size_mod 
   use Material_mod

   implicit none


!  Arguments

   real(adqt), target, intent(in) :: siga(Size%ngr,Size%nzones) 
   real(adqt), target, intent(in) :: sigs(Size%ngr,Size%nzones)
   real(adqt), target, intent(in) :: cve(Size%nzones)
   real(adqt), target, intent(in) :: rho(Size%nzones)
   real(adqt), target, intent(in) :: SMatEff(Size%nzones)
   real(adqt), target, intent(in) :: denez(Size%nzones)
   real(adqt), target, intent(in) :: trz(Size%nzones)
   real(adqt), target, intent(in) :: tez(Size%nzones)
   real(adqt), target, intent(in) :: tec(Size%ncornr)

!  Material Properties 

   Mat%siga    => siga 
   Mat%sigs    => sigs 
   Mat%cve     => cve 
   Mat%rho     => rho
   Mat%SMatEff => SMatEff 
   Mat%denez   => denez
   Mat%trz     => trz
   Mat%tez     => tez
   Mat%tec     => tec


   return
   end subroutine setMaterialModule 



