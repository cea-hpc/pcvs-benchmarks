!***********************************************************************
!                        Version 1:  02/94, PFN                        *
!                                                                      *
!   PROFINT - Performs an interpolation of all profiles at the current *
!             time.                                                    *
!                                                                      *
!***********************************************************************

   subroutine profint

   use kind_mod
   use constant_mod
   use radconstant_mod
   use TimeStepControls_mod
   use ProfileList_mod
   use Profile_mod
   use QuadratureList_mod
   use Size_mod

   implicit none

!  Local
                                                                                         
   integer    :: ip,ig,n,nl,nh,nlow,nhigh,ngr,NumTimes,NumProfiles

   real(adqt) :: ac,timel,timeh,dtime,tmp, &
                 t4,xl,xh,timerad,Mult

   real(adqt) :: gnu(Size%ngr+1),x(Size%ngr+1),frac(Size%ngr+1)

   character(len=8) :: Type

!  Constants

   ac      = rad_constant*speed_light 
   ngr     = Size% ngr

   timerad = getRadTime(DtControls)

   gnu(:) = getEnergyGroups(Quad,ngr)

!  We need to check each profile to see if it is
!  on (set to zero if off).

   NumProfiles = getNumberOfProfiles(SourceProfiles)

   ProfileLoop: do ip=1,NumProfiles

     ProfID => getProfile(SourceProfiles, ip)

     Type           = ProfID% Type
     NumTimes       = ProfID% NumTimes
     Mult           = ProfID% Multiplier
     ProfID% Status = 'off'

!    Interpolate in time

     ProfID% InterpValues(1:ngr) = zero

     TestTime: if ( (timerad > ProfID% Times(1)) .and.  &
                    (timerad < ProfID% Times(NumTimes)) ) then

       ProfID% Status = 'on'

!      Find time factor

       n = 0

       TimeIteration: do 
         n = n + 1

         if (timerad > ProfID% Times(n) .and. n < NumTimes) then
           timel = ProfID% Times(n)
           nl    = n
           cycle TimeIteration
         else
           exit TimeIteration 
         endif

       enddo TimeIteration

       nh    = nl + 1
       timeh = ProfID% Times(nh)
       dtime = (timerad - timel)/(timeh - timel)

!********************
!  Temperature      *
!********************
                                                                                         
       TestType: if ( (Type == 'temp') .or. (Type == 'tempdriv') ) then 
                                                                                         
!  Find the interpolated temperature and then generate a
!  Planckian energy spectrum at that temperature

         tmp =  ProfID% Values(nl) + dtime*  &
               (ProfID% Values(nh) - ProfID% Values(nl))

         t4  = ac*tmp*tmp*tmp*tmp

!  Compute the fraction of the total emission in each energy group
!  The input for RTPLNK is (h*nu)/(k*Te).

         if (ngr == 1) then

           ProfID% InterpValues(1) = Mult*t4

         else

           x(:) = gnu(:)/tmp
           call rtplnk(ngr+1,x,FRAC)

           frac(1)     = zero
           frac(ngr+1) = one

           do ig=1,ngr
             ProfID% InterpValues(ig) = Mult*t4*  &
                                       (frac(ig+1) - frac(ig))
           enddo
 
         endif

!********************
!  FDS              *
!********************

       elseif ( (Type == 'fds') .or. (Type == 'fdsdriv') ) then

!  For frequency-dependent sources there are NGR values for
!  each time

         nlow  = (nl - 1)*ngr
         nhigh = (nh - 1)*ngr

         do ig=1,ngr
           ProfID% InterpValues(ig) = speed_light*Mult*         &
                                     ( gnu(ig+1) - gnu(ig) )*   &
                     (ProfID% Values(nlow+ig) + dtime*          &
                     (ProfID% Values(nhigh+ig) - ProfID% Values(nlow+ig)))
         enddo

!********************
!  ENERGY           *
!********************

       elseif (Type == 'energy') then

!  For energy sources there is one value for each time

        ProfID% InterpValues(1) =  Mult*             &
                      (ProfID% Values(nl) + dtime*   &
                      (ProfID% Values(nh) - ProfID% Values(nl)))

       endif TestType

     endif TestTime

   enddo ProfileLoop


   return
   end subroutine profint

