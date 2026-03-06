!***********************************************************************
!                        Version 1:  01/2001, PFN                      *
!                                                                      *
!   RTGEOM3 - Calculates geometry factors for Sn radiation transport   *
!             in 3D for a corner-based method.                         *
!                                                                      *
!   Input:   px     - point coordinates  (L)                           *
!                                                                      *
!   Output:  A_SURF - area vectors on corner faces (A)                 *
!            A_BDY  - area vectors on the problem boundary (A)         *
!            VOLC   - corner volume  (V)                               *
!            VOLZ   - zone volume    (V)                               *
!                                                                      *
!***********************************************************************

   subroutine rtgeom3

   use kind_mod
   use Size_mod
   use Geometry_mod
   use constant_mod
   use BoundaryList_mod
   use Boundary_mod
   use ZoneData_mod

   implicit none

!  Local Variables

   type(ZoneData), pointer   :: Z2

   integer    :: nBoundary, nBdyElem, b0

   integer    :: i,ic,icface1,icface2,icfp,icez1,icez2,  &
                 ib,ipnt0,ipnt1,ipnt2,iface,zone,zonefp,nCFaces

   integer    :: ndim, nbelem, nzones, nfaces, c, c0, cez, cfp, nCorner 

   real(adqt) :: tdl(3),tfl(3),tzl(3),tfz(3),tfe1(3),  &
                 tfe2(3),A_fep(3),A_fez1(3),A_fez2(3), &
                 pnt0(3),pnt1(3),pnt2(3),zx(3)

!  Dynamic arrays

   real(adqt), allocatable :: fx(:,:)
   real(adqt), allocatable :: A_bdy(:,:)

!  Mesh Constants

   ndim      = Size%ndim
   nzones    = Size%nzones
   nfaces    = Size%nfaces
   nbelem    = Size%nbelem
   nBoundary = getNumberOfBoundaries(RadBoundary)


!  Allocate temporary arrays

   allocate( fx(ndim,nfaces) )
   allocate( A_bdy(ndim,nbelem) )

!  Calculate the location of the face-centers (FX)

   call face_coords(FX) 

!  Compute Vectors from edge-center to point (TEL), face-center to
!  point (TFL) and zone-center to point (TZL).  These are used
!  to compute outward normals on corner faces.  The corner-face
!  area is the sum of two half-side areas. 

   Geom%volc(:) = zero

   ZoneLoop: do zone=1,nzones

     Z => getZoneData(Geom, zone)

     call zone_coords(ZX)

     nCorner = Z% nCorner
     nCFaces = Z% nCFaces
     c0      = Z% c0

     Z%VolumeZone  = zero
     Z%A_ez(:,:,:) = zero

     CornerLoop: do c=1,nCorner 

       ic    = c0 + c 
       ipnt0 = Geom%CToPoint(ic)

       CornerFaceLoop: do icface1=1,nCFaces

         icface2 = mod(icface1,nCFaces) + 1

         icfp    = Z%Connect(1,icface1,c)
         ib      = Z%Connect(2,icface1,c)

         icez1   = Z%Connect(3,icface1,c)
         icez2   = Z%Connect(3,icface2,c)

         ipnt1   = Geom%CToPoint(icez1+c0)
         ipnt2   = Geom%CToPoint(icez2+c0)

         iface   = Geom%CToFace(icface1,ic)

         pnt0(:) = Geom%px(:,ipnt0)
         pnt1(:) = Geom%px(:,ipnt1)
         pnt2(:) = Geom%px(:,ipnt2)


         tdl(:)  = half*(pnt1(:)     - pnt2(:))
         tfl(:)  =       fx(:,iface) - pnt0(:)
         tzl(:)  =       zx(:)       - pnt0(:)
         tfz(:)  =       fx(:,iface) - zx(:)
         tfe1(:) =       fx(:,iface) - half*(pnt1(:) + pnt0(:))
         tfe2(:) =       fx(:,iface) - half*(pnt2(:) + pnt0(:))

!  Calculate the components of the outward normals on
!  "FP" corner faces; this is the sum of two tet area vectors

         A_fep(1) = half*( tfl(3)*tdl(2) - tfl(2)*tdl(3) )
         A_fep(2) = half*( tfl(1)*tdl(3) - tfl(3)*tdl(1) )
         A_fep(3) = half*( tfl(2)*tdl(1) - tfl(1)*tdl(2) )

         if (icfp /= 0) then

!  Ensure that outward normals on FP faces are equal and opposite

           if (icfp > ic) then
             Z%A_fp(:,icface1,c)  =  A_fep(:)

             zonefp =  Geom% CToZone(icfp)
             Z2     => getZoneData(Geom, zonefp)
             cfp    =  icfp - Z2%c0

             Z2%A_fp(:,ib,cfp) = -A_fep(:)
           else
             A_fep(:) = Z%A_fp(:,icface1,c)
           endif

!  Set the outward normal on problem boundary

         elseif (icfp == 0) then
           Z%A_fp(:,icface1,c) = A_fep(:)
           A_bdy(:,ib)         = A_fep(:)
         endif

!  "EZ" corner faces; here we add the tet area vectors
!  to two different "EZ" faces

         A_fez1(1) = half*( tfz(3)*tfe1(2) - tfz(2)*tfe1(3) )
         A_fez1(2) = half*( tfz(1)*tfe1(3) - tfz(3)*tfe1(1) )
         A_fez1(3) = half*( tfz(2)*tfe1(1) - tfz(1)*tfe1(2) )

         A_fez2(1) = half*( tfz(2)*tfe2(3) - tfz(3)*tfe2(2) )
         A_fez2(2) = half*( tfz(3)*tfe2(1) - tfz(1)*tfe2(3) )
         A_fez2(3) = half*( tfz(1)*tfe2(2) - tfz(2)*tfe2(1) )

!  Accumulate corner surface areas on "EZ" faces

         Z%A_ez(:,icface1,c) = Z%A_ez(:,icface1,c) + A_fez1(:)
         Z%A_ez(:,icface2,c) = Z%A_ez(:,icface2,c) + A_fez2(:)

!  Accumulate corner volumes (VOLC)

         Geom%volc(ic) = Geom%volc(ic) + third*abs( tzl(1)*A_fep(1) +  &
                                                    tzl(2)*A_fep(2) +  &
                                                    tzl(3)*A_fep(3) )

       enddo CornerFaceLoop

!  Accumulate zone volumes (VOLZ)

       Z%Volume(c)     = Geom%volc(ic)
       Z%VolumeZone    = Z%VolumeZone + Z%Volume(c)

     enddo CornerLoop

!  Ensure that outward normals on "EZ" faces are equal and opposite

     do c=1,nCorner
       do icface1=1,nCFaces
         cez = Z%Connect(3,icface1,c)
         if (cez > c) then
           do icface2=1,nCFaces
             if (Z%Connect(3,icface2,cez) == c) then
               Z%A_ez(:,icface2,cez) = -Z%A_ez(:,icface1,c)
             endif
           enddo
         endif
       enddo
     enddo

   enddo ZoneLoop


   do i=1,nBoundary
     Bdy      => getBoundary(RadBoundary, i)
     nBdyElem =  getNumberOfBdyElements(Bdy)
     b0       =  getFirstBdyElement(Bdy) - 1
!!$     write(*,*)'rtgeom3.F90, boundary',i,' nBdyElem=',nBdyElem
     do ib=1,nBdyElem
!!$        write(6,100)A_bdy(1,ib+b0),A_bdy(2,ib+b0),A_bdy(3,ib+b0)
!!$100     format("  setting outward normal (",f8.5,",",f8.5,",",f8.5,").")
       Bdy% A_bdy(:,ib) = A_bdy(:,ib+b0) 
     enddo
   enddo

!  Release temporary arrays

   deallocate( fx )
   deallocate( A_bdy )


   return
   end subroutine rtgeom3

