!***********************************************************************
!                                    Version 0: 02/02 MKN              *
!                                                                      *
!    setEnergyEdits -  Called from Teton.cc to set current values for  *
!                      the energy edits.                               *
!                                                                      *
!    Input:   -  RadEdit                                               *
!                                                                      *
!    Output:                                                           *
!                                                                      *
!***********************************************************************
   subroutine setEnergyEdits(erad, emat, eradinct, eradesct,  &
                             eextsrc, echeck, deltaEsrc, deltaEhyd)

   use kind_mod
   use Editor_mod

   implicit none


!  Arguments

   real(adqt), intent(in)    :: erad
   real(adqt), intent(in)    :: emat
   real(adqt), intent(in)    :: eradinct
   real(adqt), intent(in)    :: eradesct 
   real(adqt), intent(in)    :: eextsrc 
   real(adqt), intent(in)    :: echeck
   real(adqt), intent(in)    :: deltaEsrc
   real(adqt), intent(in)    :: deltaEhyd

!  Energy Edits

   RadEdit% EnergyRadiation    = erad
   RadEdit% EnergyMaterial     = emat
   RadEdit% EnergyIncident     = eradinct
   RadEdit% EnergyEscaped      = eradesct
   RadEdit% EnergyExtSources   = eextsrc
   RadEdit% EnergyCheck        = echeck
   RadEdit% DeltaEnergyExtSrc  = deltaEsrc
   RadEdit% DeltaEnergyHydWork = deltaEhyd


   return
   end subroutine setEnergyEdits



