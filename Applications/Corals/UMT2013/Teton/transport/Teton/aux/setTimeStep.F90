!***********************************************************************
!                         Version 0: 04/06 PFN                         *
!                                                                      *
!    setTimeStep  -  Called from Teton.cc to update the radiation      *
!                    time and timestep before calling RADTR.           *
!                                                                      *
!    Input:   dtrad      - radiation timestep                          *
!             timerad    - radiation time                              *
!                                                                      *
!    Output:  DtControls - structure containing timestep information   *
!                                                                      *
!***********************************************************************
   subroutine setTimeStep(dtrad, timerad, tfloor, tmin)

   use kind_mod
   use Size_mod
   use TimeStepControls_mod

   implicit none


!  Arguments

   real(adqt), intent(in)    :: dtrad, timerad, tfloor, tmin

!  Update controls
                                                                                             
   call setDtControls(DtControls,         &
                      RadTimeStep=dtrad,  &
                      RadTime=timerad     )

   Size%tfloor = tfloor
   Size%tmin   = tmin


   return
   end subroutine setTimeStep 



