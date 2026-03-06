!***********************************************************************
!                        Version 0:  01/97, PFN                        *
!                                                                      *
!   RADTR  - Control program for radiation transport. It initializes   *
!            arrays, controls timestep, calls the transport package    *
!            and performs edits.                                       *
!                                                                      *
!                                                                      *
!***********************************************************************

   subroutine radtr(PSIR, PHI, RADENERGYDENSITY, angleLoopTime)

!  Include

   use kind_mod
   use Size_mod
   use Material_mod
   use Geometry_mod
   use TimeStepControls_mod
   use ProfileList_mod
   use ZoneData_mod
   use constant_mod
   use radconstant_mod

   implicit none

!  Arguments

!  Types
                                                                                         
   real(adqt), intent(inout) :: psir(Size%ngr,Size%ncornr,Size%nangSN),  &
                                Phi(Size%ngr,Size%ncornr),               &
                                RadEnergyDensity(Size%nzones,Size%ngr), angleLoopTime

!  Local

   integer    :: zone

   real(adqt) :: dtrad

!  Allocate Memory 

   call constructPsiInc(SourceProfiles)

!***********************************************************************
!     ADD TIME-ABSORPTION TO THE TOTAL CROSS SECTION ARRAY             *
!***********************************************************************

   dtrad = getRadTimeStep(DtControls)

   if (Size%ittyp == 'timedep') then
     Size%tau = one/(speed_light*dtrad)
   else
     Size%tau = zero
   endif

   do zone=1,Size%nzones
     Z => getZoneData(Geom, zone)

     Z% Sigt(:)    = Mat%siga(:,zone) + Mat%sigs(:,zone) + Size%tau
     Z% SigtInv(:) = one/Z% Sigt(:)
   enddo

!***********************************************************************
!     INTERPOLATE SOURCE PROFILES                                      *
!***********************************************************************

   call profint
 
!***********************************************************************
!     BOUNDARY CONDITIONS                                              *
!***********************************************************************
 
   call rtbdry

!***********************************************************************
!     VOLUME RADIATION SOURCES                                         *
!***********************************************************************

   call rtvsrc

!***********************************************************************
!     SAVE ZONE AVERAGE TEMPERATURES FOR TIME STEP CALCULATION         *
!*********************************************************************** 

   call advanceRT(dtrad, PSIR, PHI)

!***********************************************************************
!     RADIATION TRANSPORT MODULE                                       *
!***********************************************************************

   call rtmainsn(dtrad, PSIR, PHI, angleLoopTime) 

!***********************************************************************
!     ENERGY UPDATE                                                    *
!***********************************************************************
         
   ! call newenergy
   call rtave(Mat%denec, Mat%DENEZ)

!***********************************************************************
!     EDITS                                                            *
!***********************************************************************

   call rtedit(dtrad, Phi)

!***********************************************************************
!     TIME STEP CONTROL                                                *
!***********************************************************************

   call dtnew

!***********************************************************************
!     UPDATE RADIATION MOMENTS                                         *
!***********************************************************************

   call RadMoments(Phi, RadEnergyDensity)

!***********************************************************************
!     RELEASE MEMORY                                                   *
!***********************************************************************

   call destructPsiInc(SourceProfiles)


   return
   end subroutine radtr

