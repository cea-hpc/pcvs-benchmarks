!***********************************************************************
!                        Version 1:  08/98, PFN                        *
!                                                                      *
!   RTBATCH - Creates batches of energy groups that can be transported *
!             together.  The only requirement is that all groups       *
!             within a "batch" must have the same quadrature set.      *
!                                                                      *
!   Input:   ngr      - number of groups                               *
!            QuadDef  - group quadrature definition                    *
!                                                                      *
!   Output:  Quad     - Quadrature module                              *
!                                                                      *
!***********************************************************************
 
   subroutine rtbatch(QuadDef, gnu) 

   use kind_mod
   use Size_mod
   use QuadratureList_mod

   implicit none

!  Arguments

   integer,    intent(in)       :: QuadDef(6,Size%ngr+1)
   real(adqt), intent(in)       :: gnu(Size%ngr+1)

!  Local

   integer    :: ig, ignext, g1, g2, ngr, QuadID
   integer    :: Type, Order, Type_set, Order_set, Groups
   integer    :: NumAngles, NumMoments, NPolar, NAzimuthal, PolarAxis
   real(adqt) :: GrpBnds(Size%ngr+1)

!  Find the number of angle sets and the number of groups in them 

   QuadID     = 1 
   Type_set   = QuadDef(1,1) 
   Order_set  = QuadDef(2,1) 
   Groups     = 0 
   NumMoments = 1 
   ngr        = Size%ngr
   g1         = 1

   do ig=1,ngr+1

      Groups = Groups + 1

      ignext = ig + 1
      if (ig < ngr) then
         Type  = QuadDef(1,ignext)
         Order = QuadDef(2,ignext)
      else
         !  This forces the last set to be completed
         Type  = 0 
         Order = 0 
      endif

     if (  Type == Type_set   .and.   &
          Order == Order_set ) then 

     else

!  Set the current quadrature definition

       NPolar     = QuadDef(3,ig)
       NAzimuthal = QuadDef(4,ig)
       PolarAxis  = QuadDef(5,ig)
       NumAngles  = QuadDef(6,ig)
       g2         = ig + 1

       if (ig < ngr+1) then
         GrpBnds(1:Groups+1) = Gnu(g1:g2)
       else
         GrpBnds(1) = Gnu(1) 
         GrpBnds(2) = Gnu(ngr+1)
       endif

       call setQuadrature(Quad,          &
                          QuadID,        &
                          Groups,        &
                          NumAngles,     &
                          NumMoments,    &
                          Order_set,     &
                          NPolar,        &
                          NAzimuthal,    &
                          PolarAxis,     &
                          Type_set,      &
                          GrpBnds)

!  Start a new batch

       if (ig < ngr+1) then
         QuadID    = QuadID + 1
         Groups    = 0 
         Type_set  = QuadDef(1,ignext) 
         Order_set = QuadDef(2,ignext) 
         g1        = ig + 1
       endif

     endif

   enddo


   return
   end subroutine rtbatch

