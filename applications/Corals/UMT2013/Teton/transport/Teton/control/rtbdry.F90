!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTBDRY - Calculates the radiation intensity incident on the        *
!            problem boundary.  Three types of boundary conditions     *
!            are currently allowed:  vacuum, fds and milne.  A vacuum  *
!            condition yields a zero incoming intensity.  For FDS, the *
!            frequency-dependent intensity is specified by the user    *
!            and is assumed to be isotropic.  For a MILNE condition,   *
!            the temperature is specified and the intensity is a       *
!            Planckian in frequency space and is isotropic.            *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:  PHINC  - incoming intensity at outer problem boundary     *
!                     for all mu<0                            (E/A/t)  *
!                                                                      *
!   Local:   a - radiation constant      (E/V/T^4)                     *
!            c - speed of light          (L/t)                         *
!                                                                      *
!***********************************************************************

   subroutine rtbdry

   use kind_mod
   use constant_mod
   use Size_mod
   use ProfileList_mod
   use Profile_mod

   implicit none

!  Local

   integer    :: ia,ig,ip,ngr,nangSN,NumProfiles

   real(adqt) :: wtiso

   character(len=8) :: Type, Status, Location, Shape

!  Dynamic

   real(adqt),  allocatable :: smult(:,:)

!  For Sn, PHINC is per steradian

   ngr    = Size% ngr
   nangSN = Size% nangSN
   wtiso  = Size% wtiso

!  Set angular shape for each group of each profile

   allocate( smult(ngr,nangSN) )

   NumProfiles = getNumberOfProfiles(SourceProfiles)

   ProfileLoop: do ip=1,NumProfiles 

     ProfID => getProfile(SourceProfiles, ip)
 
     Status   = ProfID% Status
     Location = ProfID% Location
     Type     = ProfID% Type
     Shape    = ProfID% Shape

     CheckStatus: if (Status == 'on' .and. Location == 'boundary') then

       smult(:,:) = zero

       if (Shape == 'iso') then

         smult(:,:) = one

!  The only option that works at the moment is isotropic

!  Normal Incidence

       elseif (Shape == 'normal') then

         smult(:,:) = one

!  Grazing Angle

       elseif (Shape == 'grazing') then

         smult(:,:) = one

!  NOTE:  "tnormal" and "bnormal" options only work in 3D
!  Normally Incident from the top - pick angles that are closest
!  to (mu = 0, eta = 0, xi = -1)

       elseif (Shape == 'tnormal') then

         smult(:,:) = one

!  Normally Incident from the bottom - pick angles that are closest
!  to (mu = 0, eta = 0, xi = +1)

       elseif (Shape == 'bnormal') then

         smult(:,:) = one

       endif

 
!***********************
!  Temperature or FDS  *
!***********************

       if (Type == 'temp' .or. Type == 'fds') then

         do ia=1,nangSN
           SourceProfiles% Psi_Inc(:,ia,ip) = wtiso*smult(:,ia)*ProfID% InterpValues(:)
         enddo

       endif

     endif CheckStatus
                                                                                         
   enddo ProfileLoop

!  Release temporary

   deallocate( smult )

 
   return
   end subroutine rtbdry

