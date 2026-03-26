!***********************************************************************
!                        Version 1:  03/01, PFN                        *
!                                                                      *
!   SNSWP3D  - This routine calculates angular fluxes for a single     *
!              direction for an upstream corner-balance spatial        *
!              discretization in 3D.                                   *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!                                                                      *
!***********************************************************************

   subroutine snswp3d(Groups, Angle,         &
                      next, nextZ, PSIC, PSIB)

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use Quadrature_mod
   use Material_mod
   use ZoneData_mod

   implicit none

!  Arguments

   integer,    intent(in)    :: Groups, Angle 

   integer,    intent(in)    :: next(Size%ncornr+1)
   integer,    intent(in)    :: nextZ(Size%nzones)

   real(adqt), intent(inout) :: psic(Groups,Size%ncornr),       &
                                psib(Groups,Size%nbelem)

!  Local Variables

   integer    :: i, ib, ic, icfp, icface, ifp, ig, k, nxez
   integer    :: nzones, zone, c, c0, cez, ii, ndone
   integer    :: nCorner, nCFaces

   integer    :: ez_exit(Size%maxcf)

   real(adqt) :: fouralpha, fouralpha4, aez, aez2, area_opp, area_inv
   real(adqt) :: source, sigv, sigv2, gnum, gtau, sez, sumArea

   real(adqt) :: omega(3)
   real(adqt) :: psi_opp(Groups)
   real(adqt) :: SigtVol(Groups,Size% maxCorner)
   real(adqt) :: src(Groups,Size% maxCorner)
   real(adqt) :: Q(Groups,Size% maxCorner)
   real(adqt) :: afpm(Size%maxcf)
   real(adqt) :: coefpsic(Size%maxcf)
   real(adqt) :: psifp(Groups,Size%maxcf)
   real(adqt) :: tpsic(Groups,Size% maxCorner)

#ifdef PROFILING_ON
   integer profiler(2) / 0, 0 /
   save profiler
#endif

!  Constants

   parameter (fouralpha=1.82d0)
   parameter (fouralpha4=5.82d0)

#ifdef PROFILING_ON
   call TAU_PROFILE_TIMER(profiler, 'snswp3d')
   call TAU_PROFILE_START(profiler)
#endif

   nzones    = Size%nzones

   omega(:) = QuadSet% omega(:,Angle)

!  Loop through all of the corners using the NEXT list

   ndone = 0

   ZoneLoop: do ii=1,nzones
 
     zone = nextZ(ii)

     Z    => getZoneData(Geom, zone)

     nCorner   = Z% nCorner
     nCFaces   = Z% nCFaces
     c0        = Z% c0 

!  Contributions from volume terms

     do c=1,nCorner
       do ig=1,Groups
         source        = Z%STotal(ig,c) + Z%STime(ig,c,Angle)
         Q(ig,c)       = Z%SigtInv(ig)*source 
         src(ig,c)     = Z%Volume(c) *source
         SigtVol(ig,c) = Z%Sigt(ig)*Z%Volume(c)
       enddo
     enddo

     CornerLoop: do i=1,nCorner

       ic      = next(ndone+i)
       c       = ic - c0

!  Calculate Area_CornerFace dot Omega to determine the 
!  contributions from incident fluxes across external 
!  corner faces (FP faces)

       sumArea = zero
 
       do icface=1,ncfaces

         afpm(icface) = omega(1)*Z%A_fp(1,icface,c) + &
                        omega(2)*Z%A_fp(2,icface,c) + &
                        omega(3)*Z%A_fp(3,icface,c)

         icfp    = Z%Connect(1,icface,c)
         ib      = Z%Connect(2,icface,c)
                                                                                                   
         if ( afpm(icface) >= zero ) then
           sumArea = sumArea + afpm(icface)
         else
           if (icfp == 0) then
             psifp(:,icface) = psib(:,ib)
           else
             psifp(:,icface) = psic(:,icfp)
           endif

           src(:,c)  = src(:,c) - afpm(icface)*psifp(:,icface)
         endif
       enddo

!  Contributions from interior corner faces (EZ faces)

       nxez = 0

       do icface=1,nCFaces

         aez = omega(1)*Z%A_ez(1,icface,c) + &
               omega(2)*Z%A_ez(2,icface,c) + &
               omega(3)*Z%A_ez(3,icface,c) 

         if (aez > zero ) then

           sumArea        = sumArea + aez
           area_opp       = zero
           nxez           = nxez + 1
           cez            = Z%Connect(3,icface,c)
           ez_exit(nxez)  = cez
           coefpsic(nxez) = aez

           if (nCFaces == 3) then

             ifp = mod(icface,nCFaces) + 1

             if ( afpm(ifp) < zero ) then
               area_opp   = -afpm(ifp)
               psi_opp(:) =  psifp(:,ifp)
             endif

           else

             ifp        = icface
             area_opp   = zero
             psi_opp(:) = zero

             do k=1,nCFaces-2
               ifp = mod(ifp,nCFaces) + 1
               if ( afpm(ifp) < zero ) then
                 area_opp   = area_opp   - afpm(ifp)
                 psi_opp(:) = psi_opp(:) - afpm(ifp)*psifp(:,ifp)
               endif
             enddo

             area_inv = one/area_opp

             psi_opp(:) = psi_opp(:)*area_inv

           endif

           TestOppositeFace: if (area_opp > zero) then

             aez2 = aez*aez

             do ig=1,Groups
  
               sigv         = SigtVol(ig,c)
               sigv2        = sigv*sigv
 
               gnum         = aez2*( fouralpha*sigv2 +              &
                              aez*(four*sigv + three*aez) )

               gtau         = gnum/                                    &
                            ( gnum + four*sigv2*sigv2 + aez*sigv*(six*sigv2 + &
                              two*aez*(two*sigv + aez)) ) 

               sez          = gtau*sigv*( psi_opp(ig) - Q(ig,c) ) +   &
                              half*aez*(one - gtau)*( Q(ig,c) - Q(ig,cez) )
               src(ig,c)    = src(ig,c)   + sez
               src(ig,cez)  = src(ig,cez) - sez

             enddo

           else

             do ig=1,Groups
               sez          = half*aez*( Q(ig,c) - Q(ig,cez) )
               src(ig,c)    = src(ig,c)   + sez
               src(ig,cez)  = src(ig,cez) - sez
             enddo

           endif TestOppositeFace

         endif

       enddo

!  Corner angular flux

       tpsic(:,c) = src(:,c)/(sumArea + SigtVol(:,c))

!  Calculate the angular flux exiting all "FP" surfaces
!  and the current exiting all "EZ" surfaces.
!  The downstream corner index is "ez_exit."

!  Zone Interior or "EZ" Faces

       do icface=1,nxez
         cez        = ez_exit(icface)
         src(:,cez) = src(:,cez) + coefpsic(icface)*tpsic(:,c)
       enddo

     enddo CornerLoop

     ndone = ndone + nCorner

!  Copy temporary flux array into the global one

     do c=1,nCorner
       psic(:,c0+c) = tpsic(:,c)
     enddo
                                                                                                
   enddo ZoneLoop

!  Set exiting boundary fluxes

   ExitBdy => getExitList(QuadSet, Angle)

   do i=1,ExitBdy% nExit
     ib = ExitBdy% ListExit(1,i)
     ic = ExitBdy% ListExit(2,i)
     psib(:,ib) = psic(:,ic)
   enddo



#ifdef PROFILING_ON
   call TAU_PROFILE_STOP(profiler)
#endif


   return
   end subroutine snswp3d


