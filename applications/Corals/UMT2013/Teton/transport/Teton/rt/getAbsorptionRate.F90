!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   getAbsorptionRate - Computes the total absorption rate.            *
!                                                                      *
!   Input:   snetc  - corner net emission source rate (group sum)      *
!                                                              (E/V/t) *
!                                                                      *
!   Local:   etchic - scratch array                                    *
!                                                                      *
!   Output:  SRCC   - scalar group-dependent source            (E/V/t) *
!                                                                      *
!***********************************************************************
   subroutine getAbsorptionRate(Phi) 

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use Material_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(in)    :: Phi(Size%ngr,Size%ncornr)

!  Local

   integer    :: c, c0, nCorner, ig, zone 

   real(adqt) :: sumrad 

!  Calculate the total energy absorption rate density 

   ZoneLoop: do zone=1,Size%nzones
                                                                                                 
     Z       => getZoneData(Geom, zone)
                                                                                                 
     nCorner = Z% nCorner
     c0      = Z% c0
                                                                                                 
     do c=1,nCorner
                                                                                                 
       sumrad = zero
                                                                                                 
       do ig=1,Size%ngr
         sumrad = sumrad + Mat%siga(ig,zone)*Phi(ig,c0+c)
       enddo
                                                                                                 
       Mat%AbsorptionRate(c0+c) = sumrad
                                                                                                 
     enddo
                                                                                                 
   enddo ZoneLoop

 
   return
   end subroutine getAbsorptionRate 

