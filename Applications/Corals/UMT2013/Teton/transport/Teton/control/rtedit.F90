!***********************************************************************
!                        Version 1:  05/92, PFN                        *
!                                                                      *
!   RTEDIT  - Computes zone average temperatures and energies at the   *
!             conclusion of the radiation cycle as an edit.            *
!                                                                      *
!   Input:   phir  - corner scalar photon intensity      (E/A/t)       *
!            tec   - corner electron temperatures        (T)           *
!            denec - corner electron energy change       (E/m)         *
!            volc  - corner volumes                      (V)           *
!                                                                      *
!   Output:  TRZ   - zone average radiation temperature  (T)           *
!            TEZ   - zone average electron temperature   (T)           *
!            ENEZ  - zone average electron energy        (E/m)         *
!                                                                      *
!***********************************************************************
   subroutine rtedit(dtrad, Phi) 


   use kind_mod
   use mpi_param_mod
   use mpif90_mod
   use constant_mod
   use radconstant_mod
   use Size_mod
   use Geometry_mod
   use Material_mod
   use Editor_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(in)    :: dtrad

   real(adqt), intent(in)    :: Phi(Size%ngr,Size%ncornr)

!  Local

   integer, dimension (1) :: iztrmx
   integer, dimension (1) :: iztemx

   integer    :: c, c0, nCorner, zone, ig, nzones, ngr, nn
   integer    :: nodeTr, nodeTe

   real(adqt) :: factor, geomfactor, tr4min, mass, sumExt, sumRad, tfloor

   real(adqt) :: my_trmax,my_temax,trmax,temax,PhiAve,TeAve
   real(adqt) :: derad,demat,desrc,desrcMat,desrcRad,dehyd,deradinc,deradesc,decheck
   real(adqt) :: erad,emat,eextsrc,eradinct,eradesct,echeck,eradEOC,edep

!  Parameters 

   nzones = Size%nzones
   ngr    = Size%ngr

!  Geometry factor for true 3D volume

   geomfactor = one
   tfloor     = Size%tfloor

   if (Size%igeom == 'rz') then
     geomfactor = two*pi
   endif
 
!  Compute zone-average radiation and electron temperatures,
!  end-of-cycle radiation energy and energy change due to
!  external radiation sources (desrcRad)
!  "Phi" has units of energy/area/time 

   eradEOC  = zero
   desrcRad = zero
   desrcMat = zero
   demat    = zero
   factor   = geomfactor/speed_light
   tr4min   = tfloor*tfloor*tfloor*tfloor

   ZoneLoop: do zone=1,nzones

     Z       => getZoneData(Geom, zone)
                                                                                                   
     nCorner = Z% nCorner
     c0      = Z% c0
     PhiAve  = zero
     TeAve   = zero

     do c=1,nCorner
       sumRad = zero
       sumExt = zero
       do ig=1,ngr
         sumRad = sumRad + Phi(ig,c0+c)
         sumExt = sumExt + Mat%qext(ig,c0+c)
       enddo
       PhiAve   = PhiAve   + Z% Volume(c)*sumRad
       TeAve    = TeAve    + Z% Volume(c)*Mat%tec(c0+c)
       desrcRad = desrcRad + Z% Volume(c)*dtrad*geomfactor*sumExt
     enddo

     eradEOC = eradEOC + PhiAve
     PhiAve  = PhiAve/(Z% VolumeZone*rad_constant*speed_light)

!  Radiation and Electron temperatures

     Mat%trz(zone) = sqrt( sqrt( max(PhiAve, tr4min) ) )
     Mat%tez(zone) = TeAve/Z% VolumeZone

!  Energy changes in the matter

     mass     = geomfactor*Mat%rho(zone)*Z% VolumeZone
     deMat    = deMat    + mass*Mat%denez(zone)
     desrcMat = desrcMat + mass*dtrad*Mat%SMatEff(zone)

   enddo ZoneLoop

   RadEdit% EnergyRadEOC = factor*eradEOC
   erad                  = factor*eradEOC

!  Find total radiation energy and the radiation converted
!  to matter energy

   derad  = RadEdit% EnergyRadEOC - RadEdit% EnergyRadBOC 
   desrc  = RadEdit% DeltaEnergyExtSrc
   dehyd  = RadEdit% DeltaEnergyHydWork + desrcRad
   edep   = desrcRad + desrcMat

!  Find incident/escaping energy on problem boundaries

   deradinc = zero
   deradesc = zero
                                                                                          
   nn = Size%nbedit*Size%ngr
                                                                                          
   RadEdit%RadEnergyIncident(:) = RadEdit%RadEnergyIncident(:) +  & 
                                  dtrad*RadEdit%RadEnergyIncRate(:)
   RadEdit%RadEnergyEscape(:)   = RadEdit%RadEnergyEscape(:)   +  &
                                  dtrad*RadEdit%RadEnergyEscRate(:)
 
   do ig=1,Size%ngr
     deradinc = deradinc + dtrad*RadEdit%RadEnergyIncRate(nn+ig)
     deradesc = deradesc + dtrad*RadEdit%RadEnergyEscRate(nn+ig)
   enddo

!  Maximum temperatures

   iztrmx   = maxloc( Mat%trz(:) )
   trmax    = Mat%trz(iztrmx(1))
   my_trmax = trmax

   iztemx   = maxloc( Mat%tez(:) )
   temax    = Mat%tez(iztemx(1))
   my_temax = temax

   call MPIAllReduceT(TRMAX,    "max", MPI_COMM_WORLD)
   call MPIAllReduceT(TEMAX,    "max", MPI_COMM_WORLD)
   call MPIAllReduceT(ERAD,     "sum", MPI_COMM_WORLD)
   call MPIAllReduceT(DERAD,    "sum", MPI_COMM_WORLD)
   call MPIAllReduceT(DEMAT,    "sum", MPI_COMM_WORLD)
   call MPIAllReduceT(DESRC,    "sum", MPI_COMM_WORLD)
   call MPIAllReduceT(DEHYD,    "sum", MPI_COMM_WORLD)
   call MPIAllReduceT(EDEP,     "sum", MPI_COMM_WORLD)
   call MPIAllReduceT(DESRCRAD, "sum", MPI_COMM_WORLD)

   if (my_trmax == trmax) then
     nodeTr = Size%my_node
   else
     nodeTr = -1
     iztrmx = -1
   endif

   if (my_temax == temax) then
     nodeTe = Size%my_node
   else
     nodeTe = -1
     iztemx = -1
   endif
                                                                                       
   call MPIAllReduceT(nodeTr, "max", MPI_COMM_WORLD)
   call MPIAllReduceT(nodeTe, "max", MPI_COMM_WORLD)
   call MPIAllReduceT(iztrmx, "max", MPI_COMM_WORLD)
   call MPIAllReduceT(iztemx, "max", MPI_COMM_WORLD)

   decheck = deradinc + edep - deradesc - derad - demat

!  Update Edit module

   emat     = getEnergyMaterial(RadEdit)   + demat
   eradinct = getEnergyIncident(RadEdit)   + deradinc
   eradesct = getEnergyEscaped(RadEdit)    + deradesc
   eextsrc  = getEnergyExtSources(RadEdit) + desrc
   echeck   = getEnergyCheck(RadEdit)      + decheck

   call setEdits(RadEdit,                  &
                 TrMaxZone=iztrmx,         & 
                 TeMaxZone=iztemx,         &
                 TrMaxNode=nodeTr,         &
                 TeMaxNode=nodeTe,         &
                 TrMax=trmax,              &
                 TeMax=temax,              &
                 EnergyRadiation=erad,     &
                 EnergyMaterial=emat,      &
                 EnergyIncident=eradinct,  &
                 EnergyEscaped=eradesct,   &
                 EnergyExtSources=eextsrc, &
                 EnergyCheck=echeck,       &
                 DeltaEnergyRad=derad,     &
                 DeltaEnergyMat=demat,     &
                 DeltaEnergyInc=deradinc,  &
                 DeltaEnergyEsc=deradesc,  &
                 DeltaEnergyExtSrc=desrc,  &
                 DeltaEnergyCheck=decheck)

!  Timing

   call MPIAllReduceT(Size%CommTimeCycle, "max", MPI_COMM_WORLD)

   Size%CommTimeTotal = Size%CommTimeTotal + Size%CommTimeCycle


 
   return
   end subroutine rtedit

