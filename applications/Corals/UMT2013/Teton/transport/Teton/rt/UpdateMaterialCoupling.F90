!***********************************************************************
!                        Version 1:  04/2008, PFN                      *
!                                                                      *
!   UpdateMaterialCoupling:                                            * 
!                                                                      *
!   Calculates electron temperature and energy change. Updates         *
!   sources and GTA opacities.                                         *
!                                                                      *
!                                                                      *
!***********************************************************************
   subroutine UpdateMaterialCoupling(dtrad)

   use kind_mod
   use constant_mod
   use radconstant_mod
   use Size_mod
   use Geometry_mod
   use Material_mod
   use QuadratureList_mod
   use ZoneData_mod

   implicit none

!  Arguments

   real(adqt), intent(in)    :: dtrad
 
!  Local

   integer    :: ic,ig,iter,MaxIter,ngr,ngrp
   integer    :: c, c0, nCorner, zone

   real(adqt) :: chimin,ac,ccon,ex,temp,t3,t4,Eta,one_Eta,Rho,Cve,  &
                 gnukt,dBdT,sigdbdt,srcmat,sum,NetEmisAbsRate,  &
                 tecOld,MaxError,error,wtiso,tempInv,SMatEff

   real(adqt) :: deltaE, echeck

   real(adqt) :: gnu(Size%ngr+1)
   real(adqt) :: x(Size%ngr+1)
   real(adqt) :: dbex(Size%ngr+1)
   real(adqt) :: frac(Size%ngr+1)
   real(adqt) :: Chi(Size%ngr)
   real(adqt) :: Siga(Size%ngr)
   real(adqt) :: Planck(Size%ngr)
   real(adqt) :: EmissionRate(Size%ngr)

!  Constants

   parameter (chimin=1.0d-12)
   parameter (MaxError=1.0e-8)
   parameter (MaxIter=50)

   ac     = rad_constant*speed_light
   ccon   = rad_constant2*speed_light
   ngr    = Size%ngr
   ngrp   = ngr + 1
   wtiso  = Size%wtiso

   gnu(:) = getEnergyGroups(Quad,ngr)

!  Compute the fraction of the total emission in each energy group
!  This fraction is calculated for each corner.  The input for RTPLNK
!  is (h*nu)/(k*Te).  If this is a single group calculation the
!  emission source is ac*T^4 and the derivative is 4*ac*T^3.

!      if (ngr == 1) then
!
!        do ic=1,Size%ncornr
!          Planck(1)  =      ac*tec(ic)*tec(ic)*tec(ic)*tec(ic)
!          dBdT       = four*ac*tec(ic)*tec(ic)*tec(ic)
!          chic(1,ic) = one
!          sigdbdt    = Siga(1)*dBdT*dtrad
!          etac(ic)   = sigdbdt/(Rho*Cve + sigdbdt)
!        enddo
!
!      endif 

   ZoneLoop: do zone=1,Size%nzones

     Z          => getZoneData(Geom, zone)
     nCorner    = Z% nCorner
     c0         = Z% c0

     Rho        = Mat%rho(zone)
     Cve        = Mat%cve(zone)
     SMatEff    = Mat%SMatEff(zone)
     Siga(:)    = Mat%siga(:,zone)

     CornerLoop: do c=1,nCorner

       ic   = c0 + c
       iter = 0

       TeIteration: do 

         iter   = iter + 1
         tecOld = Mat%tec(ic)

!  Load powers of the corner temperature into scalars

         temp = Mat%tec(ic)
         t3   = ccon*temp*temp*temp
         t4   =   ac*temp*temp*temp*temp
         tempInv = one/temp

!  Compute hnu/kt at upper energy boundary and
!  exponentials for Planckian Derivative

         do ig=1,ngr+1
           gnukt    = gnu(ig)*tempInv
           x(ig)    = gnukt
           ex       = exp(-gnukt)
           dbex(ig) = gnukt*gnukt*gnukt*gnukt*ex/(one - ex)
         enddo

         call rtplnk(ngrp,x,FRAC)

!  Use a lower bound of zero in calculating the Planckian

         frac(1)     = zero
         frac(ngr+1) = one
         dbex(1)     = zero

!  Compute the Planckian emission source for all groups

         do ig=1,ngr
           Planck(ig) = t4*(frac(ig+1) - frac(ig))
         enddo

!  Compute the spectral functions CHIC and ETAC using the
!  Planckian derivative (dBdT  units:  E/A/t/T )

         sum = zero

         do ig=1,ngr
           dBdT    = t3*(dbex(ig) - dbex(ig+1)) + four*Planck(ig)*tempInv
           sum     = sum + Siga(ig)*dBdT
           Chi(ig) = Siga(ig)*dBdT
         enddo

!*********************************************
!* Complete the calculation of CHI & ETA     *
!*********************************************

         Eta     = dtrad*sum/(Rho*Cve + dtrad*sum)
         one_Eta = Rho*Cve/(Rho*Cve + dtrad*sum)

         do ig=1,ngr
           Chi(ig) = Chi(ig)/sum
           if (Chi(ig) < chimin) then
             Chi(ig) = zero
           endif
         enddo

!  Normalize CHI

         sum = zero

         do ig=1,ngr
           sum = sum + Chi(ig) 
         enddo

         do ig=1,ngr
           Chi(ig) = Chi(ig)/sum
         enddo

!  The change in energy is the net specific energy deposited
!  during the cycle.  Electron/ion coupling term is treated
!  implicitly.

         NetEmisAbsRate = Mat%AbsorptionRate(ic)

         do ig=1,ngr
           EmissionRate(ig) = Siga(ig)*Planck(ig)
           NetEmisAbsRate   = NetEmisAbsRate - Siga(ig)*Planck(ig)
         enddo

         deltaE        = dtrad*( SMatEff - Mat%decompton(ic) + NetEmisAbsRate/Rho )
         Mat%denec(ic) = Eta*Mat%denec(ic) + one_Eta*deltaE
         Mat%tec(ic)   = max(Mat%tecn(ic) + Mat%denec(ic)/Cve,Size%tfloor)

         error  = abs( Mat%tec(ic) - tecOld )/Mat%tec(ic)
         echeck = abs( Mat%denec(ic) - deltaE )
         error  = max( error,echeck )

         if ( error < MaxError .or. iter >= MaxIter ) then
           exit TeIteration
         else
           cycle TeIteration
         endif


       enddo TeIteration

!  Update fixed source in the radiation transport equation

       srcmat =   Eta*( NetEmisAbsRate + Rho*   &
                ( SMatEff - Mat%decompton(ic) - (Mat%denec(ic)/dtrad) ) )

       Mat% SFixed(:,ic) = wtiso*( EmissionRate(:) + Mat%qext(:,ic) +  &
                                   Rho*Mat%decompton(ic) + Chi(:)*srcmat )
       Mat% etac(ic)     = Eta

!  Set GTA opacities

!!$       call setGTAOpacity(ic,Eta,Chi,Siga,Z%SigtInv)


     enddo CornerLoop

   enddo ZoneLoop


 
   return
   end subroutine UpdateMaterialCoupling 

