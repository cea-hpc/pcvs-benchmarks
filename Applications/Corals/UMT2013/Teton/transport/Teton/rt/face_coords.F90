!=======================================================================
!                        Version 1: 01/99, MRZ
!-----------------------------------------------------------------------
! face_coords
!   This subroutine computes the coordinates of each face from the point
! coordinates.  The face coordinate is taken to be the average of all
! point coordinates associated with that face.
!
!   Routine is used only for 3D; called by rtgeom3 and assert_mesh.
!
! ncornr       number of corners
! npnts        number of points
! nfaces       number of faces
! ndim         number of spatial dimensions
!
! CToFace      maps corner face to zone face 
! px(:,ip)     coordinates of point ip
! fx(:,iface)  coordinates of face center iface
!-----------------------------------------------------------------------

   subroutine face_coords(FX)

   use kind_mod
   use Size_mod
   use Geometry_mod
   use constant_mod

!  variable declarations
   implicit none

!  passed variables
   real(adqt), intent(inout) :: fx(Size%ndim,Size%nfaces)

!  local variables
   integer :: ic, ip, icface, iface, alloc_stat
   integer :: ncornr, nfaces

   integer, allocatable :: nc_face(:)
   
!-----------------------------------------------------------------------

!  Mesh Constants

   ncornr = Size%ncornr
   nfaces = Size%nfaces

!  allocate memory
   allocate(nc_face(nfaces))

!  Sum all point coordinates associated with a face and
!  the number of corner faces on a zone face

   nc_face(:) = 0
   fx(:,:)    = zero

   do ic=1,ncornr
     ip = Geom%CToPoint(ic)
     do icface=1,Geom%nfpc(ic)
       iface          = Geom%CToFace(icface,ic)
       nc_face(iface) = nc_face(iface) + 1 
       fx(:,iface)    = fx(:,iface) + Geom%px(:,ip)
     enddo
   enddo

!  Calculate the face-center coordinates as the average of point
!  coordinates associated with that face

   do iface=1,nfaces
      if (nc_face(iface) /= 0) then
         fx(:,iface) = fx(:,iface)/real(nc_face(iface),adqt)
      else
         fx(:,iface) = zero
      endif
   enddo


!-----------------------------------------------------------------------
!  free memory
!-----------------------------------------------------------------------
   deallocate(nc_face, stat=alloc_stat)
!-----------------------------------------------------------------------

   return
   end subroutine face_coords

