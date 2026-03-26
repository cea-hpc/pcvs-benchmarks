!***********************************************************************
!                        Version 0:  02/02, MKN                        *
!                                                                      *
!   ConstructSize - Builds the F90 module containing problem           * 
!                   parameters.                                        *
!                                                                      *
!***********************************************************************

   subroutine ConstructSize(my_node, nzones, ncornr, nfaces, npnts,  &
                            nbelem, ndim, maxcf, maxCorner, ngr,     &
                            nangSN, npsi, ncomm, nbshare, nbedit,    &
                            tfloor, tmin, radForceMultiplier,        &
                            igeom, ittyp, iaccel,                    &
                            iscat, itimsrc, decomp_s)

!  Include

   use kind_mod
   use Size_mod


   implicit none

!  Arguments

!  Run Parameters
    
   integer,   intent(in) :: my_node
   integer,   intent(in) :: nzones
   integer,   intent(in) :: ncornr
   integer,   intent(in) :: nfaces
   integer,   intent(in) :: npnts
   integer,   intent(in) :: nbelem
   integer,   intent(in) :: ndim
   integer,   intent(in) :: maxcf
   integer,   intent(in) :: maxCorner
   integer,   intent(in) :: ngr
   integer,   intent(in) :: nangSN
   integer,   intent(in) :: npsi
   integer,   intent(in) :: ncomm
   integer,   intent(in) :: nbshare
   integer,   intent(in) :: nbedit
    
   real(adqt),        intent(in) :: tfloor
   real(adqt),        intent(in) :: tmin
   real(adqt),        intent(in) :: radForceMultiplier
    
   character(len=8),  intent(in) :: igeom
   character(len=8),  intent(in) :: ittyp
   character(len=8),  intent(in) :: iaccel
   character(len=8),  intent(in) :: iscat
   character(len=8),  intent(in) :: itimsrc
   character(len=8),  intent(in) :: decomp_s

!  Construct Run Parameters

   allocate (Size)

   call construct(Size, my_node=my_node,                        &
                        nzones=nzones, ncornr=ncornr,           &
                        nfaces=nfaces, npnts=npnts,             &
                        nbelem=nbelem, ndim=ndim,               &
                        maxcf=maxcf, maxCorner=maxCorner,       &
                        ngr=ngr, nangSN=nangSN,                 &
                        npsi=npsi,                              &
                        ncomm=ncomm, nbshare=nbshare,           &
                        nbedit=nbedit, tfloor=tfloor,           &
                        tmin=tmin,                              &
                        radForceMultiplier=radForceMultiplier,  &
                        igeom=igeom, ittyp=ittyp,               &
                        iaccel=iaccel,                          &
                        iscat=iscat, itimsrc=itimsrc,           &
                        decomp_s=decomp_s)


   return
   end subroutine ConstructSize

