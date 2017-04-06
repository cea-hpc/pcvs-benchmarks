!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTVSRC - Calculates the radiation intensity for zonal sources.     *
!            Allowed sources are temperature or frequency dependent.   *
!            Volume source profile identifiers are in the integer      *
!            array ZoneToSrc.  For FDS, the frequency-dependent        *
!            intensity is specified by the user and is assumed to be   *
!            isotropic.  For a TEMP source, the temperature is         *
!            specified and the intensity is a Planckian in frequency   *
!            space and is isotropic.  Unlike sources on boundaries     *
!            which specify an incoming flux, volume sources dictate    *
!            the radiation intensity in a defined region of the        *
!            problem. The solution in the remainder of the spatial     *
!            domain is determined by the physics of the problem.       *
!                                                                      *
!   Input:   pintv     - interpolated profile intensities     (E/A/t)  *
!            ZoneToSrc - volume (zonal) source profile identifiers     *
!                                                                      *
!   Output:  QEXT      - external radiation source            (E/V/t)  *
!            SIGT      - total cross section                   (1/cm)  *
!                                                                      *
!***********************************************************************

   subroutine rtvsrc 

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use Material_mod
   use ProfileList_mod
   use Profile_mod
   use ZoneData_mod

   implicit none

!  Local

   integer    :: ic,ip,ig,zone,ngr,nzones,nCorner,NumProfiles, c0,c 

   real(adqt) :: bignum

   character(len=8) :: Status, Type, Location

!  Constants

   parameter (bignum=1.0d20)

   ngr    = Size% ngr
   nzones = Size% nzones


!  Set QEXT for volume sources; note that the angular distribution
!  is isotropic for frequency-dependent and temperature sources.

!  Initialize QEXT (some sources may have been turned off since
!  the last cycle.)

   Mat%qext(:,:) = zero

!  We need to check each profile to see if it is
!  on (set to zero if off).

!********************************
!  Temperature or FDS 'sources' *
!********************************

   NumProfiles = getNumberOfProfiles(SourceProfiles)

   ProfileLoop: do ip=1,NumProfiles

     ProfID => getProfile(SourceProfiles, ip)

     Status   = ProfID% Status
     Location = ProfID% Location
     Type     = ProfID% Type

     CheckStatus: if (Status == 'on' .and. Location == 'region') then

       if (Type == 'temp' .or. Type == 'fds') then

         ZoneLoop: do zone=1,nzones
            
            if (Geom%ZoneToSrc(zone) == ip) then
               Z       => getZoneData(Geom, zone)
               nCorner = Z% nCorner
               c0      = Z% c0
               
               do c=1,nCorner
                  do ig=1,ngr
                     Mat%qext(ig,c0+c) = ProfID% InterpValues(ig)
                  enddo
               enddo
            endif
        enddo ZoneLoop

!********************************
!  Temperature or FDS 'drives'  *
!********************************

       elseif (Type == 'tempdriv' .or. Type == 'fdsdriv') then 

         ZoneLoop2: do zone=1,nzones

           if (Geom%ZoneToSrc(zone) == ip) then

             Z       => getZoneData(Geom, zone)
             nCorner = Z% nCorner
             c0      = Z% c0
             
             do c=1,nCorner
                do ig=1,ngr
                   Z% Sigt(ig)       = bignum
                   Mat%qext(ig,c0+c) = bignum*ProfID% InterpValues(ig)
                enddo
             enddo
           endif
         enddo ZoneLoop2

!********************************
!  Energy Sources               *
!********************************

       elseif (Type == 'energy') then

         ZoneLoop3: do zone=1,nzones
           if (Geom%ZoneToSrc(zone) == ip) then

             Mat%SMatEff(zone) = Mat%SMatEff(zone) + ProfID% InterpValues(1) 
           endif
         enddo ZoneLoop3

       endif


     endif CheckStatus

   enddo ProfileLoop


   return
   end subroutine rtvsrc

