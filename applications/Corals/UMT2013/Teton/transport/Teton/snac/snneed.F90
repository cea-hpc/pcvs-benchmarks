!***********************************************************************
!                        Version 1:  09/96, PFN                        *
!                                                                      *
!   SNNEED - This routine builds the NEED array which indicates the    *
!            number of incoming fluxes required to compute the         *
!            outgoing flux for a particular direction (this is the     *
!            number of incoming sides or faces the corner has for      *
!            this direction).  This routine is a decendant of SNRZANEE *
!            by MLA.                                                   *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!                                                                      *
!***********************************************************************

   subroutine snneed(omega, NEEDZ, DOWNSTREAMZ, nDSZ, MESHCYCLES) 

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use ZoneData_mod

   implicit none

!  Arguments

   integer,    intent(inout) :: needZ(Size%nzones+1)
   integer,    intent(inout) :: DownStreamZ(2*Size%maxcf,Size%nzones)
   integer,    intent(inout) :: nDSZ(Size%nzones)

   integer,    intent(inout) :: MeshCycles

   real(adqt), intent(in)    :: omega(Size%ndim)

!  Local Variables

   integer    :: ic,icfp,id,izero,nzones,ndim
   integer    :: c, c0, zone, zonefp, face, facefp, nCorner, nCFaces
   integer    :: sign(Size% maxcf,Size% maxCorner)

   real(adqt) :: afpm 

   real(adqt), allocatable       :: afpm_Face(:)
   logical (kind=1), allocatable :: doFace(:)
 
!  Constants

   parameter (izero=0)

   nzones     = Size%nzones
   ndim       = Size%ndim
   MeshCycles = 0

   allocate( afpm_Face(Size% nfaces) )
   allocate( doFace(Size% nfaces) )

   afpm_Face(:) = zero
   sign(:,:)    = izero 
   doFace(:)    = .TRUE.

!  For incoming corner-faces we increment the need array; for outgoing
!  corner-faces we put the downstream corner number into an index list.

   needZ(:)         = izero
   DownStreamZ(:,:) = izero
   nDSZ(:)          = izero

   ZoneLoop: do zone=1,nzones

     Z       => getZoneData(Geom, zone)

     nCorner = Z% nCorner
     nCFaces = Z% nCFaces
     c0      = Z% c0 

     CornerLoop: do c=1,nCorner

       ic = c0 + c 

       CornerFaceLoop: do id=1,nCFaces
 
!  Get downstream corner number

         icfp = Z%Connect(1,id,c)
         face = Geom%CToFace(id,ic)

!  Omega dot Outward normal - IMPORTANT: the dot product must be
!  coded this way to be compatible with the coding in SNSWP3D and SNSWP2D.
!  Failure to comply results in wrong answers!

!  Corner Face FP (neighboring corner, neighboring zone)
       
         if (icfp > ic) then

           if (ndim == 3) then
                                                                                                 
             afpm = omega(1)*Z%A_fp(1,id,c) +  &
                    omega(2)*Z%A_fp(2,id,c) +  &
                    omega(3)*Z%A_fp(3,id,c)
                                                                                                 
           elseif (ndim == 2) then
                                                                                                 
             afpm = omega(1)*Z%A_fp(1,id,c) +  &
                    omega(2)*Z%A_fp(2,id,c)
                                                                                                 
           endif

           afpm_Face(face) = afpm_Face(face) + afpm

           if (afpm < zero) then
             sign(id,c) = -1
           elseif (afpm > zero) then
             sign(id,c) =  1
           endif

         endif

       enddo CornerFaceLoop

     enddo CornerLoop

     do c=1,nCorner
       ic = c0 + c 
       do id=1,nCFaces

         icfp   = Z%Connect(1,id,c)

         if( icfp > 0 ) then  ! external boundary faces will have icfp = 0 
            zonefp = Geom% CToZone(icfp)
            face   = Geom% CToFace(id,ic)

            if (doFace(face)) then

               if (icfp > ic) then
                                                                                                
                  if (afpm_Face(face) < zero) then
                     if (sign(id,c) > 0) then
                        MeshCycles = MeshCycles + 1
                     else
                        needZ(zone)                      = needZ(zone)  + 1
                        nDSZ(zonefp)                     = nDSZ(zonefp) + 1
                        DownStreamZ(nDSZ(zonefp),zonefp) = zone 
                     endif !if (sign(id,c) > 0)
                                                                                                
                  elseif (afpm_Face(face) > zero) then
                     if (sign(id,c) < 0) then
                        MeshCycles = MeshCycles + 1
                     else
                        needZ(zonefp)                = needZ(zonefp) + 1
                        nDSZ(zone)                   = nDSZ(zone)    + 1
                        DownStreamZ(nDSZ(zone),zone) = zonefp 
                     endif !if (sign(id,c) < 0)

                  endif !if (afpm_Face(face) < zero)
 
               endif ! if (icfp > ic)

               doFace(face) = .FALSE.

            endif !if (doFace(face))

         endif!  if( icfp > 0

      enddo !do id=1,nCFaces
   enddo !do c=1,nCorner

   enddo ZoneLoop

   needZ(nzones+1) = 10*nzones

!  Error Check

   do zone=1,nzones
     if (nDSZ(zone) > 2*Size%maxcf) then
       call f90fatal("Memory Allocation error in SNNEED")
     endif
   enddo


!  Release memory

   deallocate( afpm_Face )
   deallocate( doFace )

 
   return
   end subroutine snneed

