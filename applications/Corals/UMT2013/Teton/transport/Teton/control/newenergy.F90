!***********************************************************************
!                        Version 1:  03/02, PFN                        *
!                                                                      *
!   NEWENERGY - Updates the corner specific energies and converts      *
!               photon intensity to photon specific intensity.         *
!                                                                      *
!   Input:      denec,denic                                            *
!                                                                      *
!   Output:     ENEC,ENIC                                              *
!                                                                      *
!***********************************************************************
 
   subroutine newenergy

   use kind_mod
   use Size_mod
   use Material_mod

   implicit none 

!***********************************************************************
!  Update changes in zonal specific energy                             *
!***********************************************************************
 
   call rtave(Mat%denec, Mat%DENEZ)
 

   return
   end subroutine newenergy 


