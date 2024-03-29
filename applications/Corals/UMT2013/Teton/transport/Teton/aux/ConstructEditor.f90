# 1 "aux/ConstructEditor.F90"
# 1 "<interne>"
# 1 "<ligne-de-commande>"
# 1 "aux/ConstructEditor.F90"
!***********************************************************************
!                        Version 0:  02/02, MKN                        *
!                                                                      *
!   CINTERFACE  -   Wrapper for modules that can be called from C++    * 
!                   used to get IterControls pointer                   *
!                                                                      *
!***********************************************************************


   subroutine ConstructEditor

!  Include

   use kind_mod
   use Editor_mod


   implicit none


!  Construct Problem Edits 

   allocate (RadEdit)

   call construct(RadEdit)


   return
   end subroutine ConstructEditor

