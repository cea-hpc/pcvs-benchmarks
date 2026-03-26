!***********************************************************************
!                        Version 0:  02/02, MKN                        *
!                                                                      *
!   CINTERFACE  -   Wrapper for modules that can be called from C++    * 
!                   used to get DtControls pointer                     *
!                                                                      *
!***********************************************************************


   subroutine ConstructDtControls(dtrad, dtrmn, dtrmx, delte, deltr)

!  Include

   use kind_mod
   use TimeStepControls_mod


   implicit none

!  Arguments

!  Time Step Controls
   real(adqt), intent(in)    :: dtrad
   real(adqt), intent(in)    :: dtrmn
   real(adqt), intent(in)    :: dtrmx
   real(adqt), intent(in)    :: delte
   real(adqt), intent(in)    :: deltr

!  Construct Time Step Controls

   allocate (DtControls)

   call construct(DtControls,         & 
                  RadTimeStep=dtrad,  &
                  MaxChangeTe=delte,  &
                  MaxChangeTr4=deltr, &
                  MinTimeStep=dtrmn,  &
                  MaxTimeStep=dtrmx)


   return
   end subroutine ConstructDtControls

