!***********************************************************************
!                         Version 0: 04/06 PFN                         *
!                                                                      *
!    SetEditorModule -    Called from Teton.cc to associate pointers   *
!                         in the EditorModule with memory allocated    *   
!                         in C++.                                      *
!                                                                      *
!    Input:   Size    - structure containing mesh parameters           *
!             RadEdit - structure containing edit arrays               *
!                                                                      *
!    Output:                                                           *
!                                                                      *
!***********************************************************************
   subroutine setEditorModule(RadEnergyEscRate, RadEnergyIncRate,  &
                              RadEnergyEscape, RadEnergyIncident)

   use kind_mod
   use Size_mod 
   use Editor_mod

   implicit none


!  Arguments

   real(adqt), target, intent(in) :: RadEnergyEscRate(Size%ngr*(Size%nbedit+1)) 
   real(adqt), target, intent(in) :: RadEnergyIncRate(Size%ngr*(Size%nbedit+1))
   real(adqt), target, intent(in) :: RadEnergyEscape(Size%ngr*(Size%nbedit+1)) 
   real(adqt), target, intent(in) :: RadEnergyIncident(Size%ngr*(Size%nbedit+1))

!  Edits on problem boundaries 

   RadEdit% RadEnergyEscRate  => RadEnergyEscRate 
   RadEdit% RadEnergyIncRate  => RadEnergyIncRate 
   RadEdit% RadEnergyEscape   => RadEnergyEscape 
   RadEdit% RadEnergyIncident => RadEnergyIncident 


   return
   end subroutine setEditorModule 



