!***********************************************************************
!                     Version 0: 01/2005 PFN                           *
!                                                                      *
!    GETRUNSTATS -  Called from Teton.cc to get information for        *
!                   convergence and time step control.                 *
!                                                                      *
!    Input:   IterControls - structure of the iteration values for     *
!                            each type ( eg., temperature )            *
!                                                                      *
!    Output:  noutrt, ninrt and ngdart.                                *
!                                                                      *
!***********************************************************************
   subroutine getRunStats(ConvControlNode, ConvControlZone,      &
                          DtControlNode, DtControlZoneTr4,       &
                          DtControlZoneTe,                       &
                          ConvControlError, ConvControlTr,       &
                          ConvControlTe, ConvControlRho,         &
                          ConvControlCve, ConvControlEdep,       &
                          DtControlChangeTr4, DtControlChangeTe, &
                          DtControlTr, DtControlTe,              &
                          DtControlTrOld, DtControlTeOld,        &
                          CommTimeCycle, CommTimeTotal,          &
                          ConvControlReason, DtControlReason)

   use kind_mod
   use Size_mod
   use Material_mod
   use iter_control_list_mod
   use iter_control_mod
   use TimeStepControls_mod

   implicit none


!  Arguments

   integer    :: ConvControlNode, ConvControlZone
   integer    :: DtControlNode, DtControlZoneTr4, DtControlZoneTe

   real(adqt) :: ConvControlError, ConvControlTr, ConvControlTe, &
                 ConvControlRho, ConvControlCve, ConvControlEdep

   real(adqt) :: DtControlChangeTr4, DtControlChangeTe,  &
                 DtControlTr, DtControlTe,               &
                 DtControlTrOld, DtControlTeOld 

   real(adqt) :: CommTimeCycle, CommTimeTotal

   character(len=8) :: ConvControlReason, DtControlReason

!  Local

   real(adqt) :: errorTemp, errorPsi
                                                                                       
   integer    :: my_node

!  Iteration Controls

   temperatureControl => getIterationControl(IterControls,"temperature")
   intensityControl   => getIterationControl(IterControls,"intensity")

   my_node = Size%my_node

!  Iteration Statistics

   errorTemp = getGlobalError(temperatureControl)
   errorPsi  = getGlobalError(intensityControl)

   if (errorPsi >= errorTemp) then
     ConvControlNode   = getProcessOfMax(intensityControl)
     ConvControlReason = 'FluxIter'
     if (my_node == ConvControlNode) then
       ConvControlZone  = getZoneOfMax(intensityControl)
       ConvControlError = errorPsi 
       ConvControlTr    = Mat%trz(ConvControlZone)
       ConvControlTe    = Mat%tez(ConvControlZone)
       ConvControlRho   = Mat%rho(ConvControlZone)
       ConvControlCve   = Mat%cve(ConvControlZone)
       ConvControlEdep  = Mat%SMatEff(ConvControlZone)
     endif
   else
     ConvControlNode   = getProcessOfMax(temperatureControl)
     ConvControlReason = 'TempIter'
     if (my_node == ConvControlNode) then
       ConvControlZone  = getZoneOfMax(temperatureControl)
       ConvControlError = errorTemp
       ConvControlTr    = Mat%trz(ConvControlZone)
       ConvControlTe    = Mat%tez(ConvControlZone)
       ConvControlRho   = Mat%rho(ConvControlZone)
       ConvControlCve   = Mat%cve(ConvControlZone)
       ConvControlEdep  = Mat%SMatEff(ConvControlZone)
     endif
   endif

!  Time Step Statistics 

   DtControlNode = getControlProcess(DtControls)

   if (my_node == DtControlNode) then
     DtControlReason    = getDtConstraint(DtControls)
     DtControlZoneTr4   = getZoneMaxChangeTr4(DtControls)
     DtControlZoneTe    = getZoneMaxChangeTe(DtControls)
     DtControlChangeTr4 = getMaxFracChangeTr4(DtControls)
     DtControlChangeTe  = getMaxFracChangeTe(DtControls)
     DtControlTr        = Mat%trz(DtControlZoneTr4)
     DtControlTe        = Mat%tez(DtControlZoneTe) 
     DtControlTrOld     = Mat%trzn(DtControlZoneTr4)
     DtControlTeOld     = Mat%tezn(DtControlZoneTe)
   endif

!  Timings

   CommTimeCycle = Size%CommTimeCycle
   CommTimeTotal = Size%CommTimeTotal



   return
   end subroutine getRunStats



