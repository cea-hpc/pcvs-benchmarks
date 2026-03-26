!***********************************************************************
!                        Version 1:  08/94, PFN                        *
!                                                                      *
!   BDYEDT - Computes edits of escaping and incident currents on all   *
!            boundaries.                                               *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!***********************************************************************
   subroutine bdyedt(psib) 

   use kind_mod
   use mpi_param_mod
   use mpif90_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use QuadratureList_mod
   use Quadrature_mod
   use BoundaryList_mod
   use Boundary_mod
   use Editor_mod

   implicit none

!  Arguments

   real(adqt), intent(in)    :: psib(Size%ngr,Size%nbelem,Size%nangSN)

!  Local Variables

   integer    :: n,nn,ia,ib,iedit,    &
                 ig,groups,nangles,ndim,nbelem

   integer    :: i, nVacuum, nSource, b0, nBdyElem 

   real(adqt) :: twopi,angdota,current,gfac

!  Constants

   twopi   = two*pi
   ndim    = Size%ndim
   nbelem  = Size%nbelem

   nVacuum = getNumberOfVacuum(RadBoundary)
   nSource = getNumberOfSource(RadBoundary)

   QuadSet => getQuadrature(Quad, 1)
   groups  =  QuadSet% Groups
   nangles =  QuadSet% NumAngles
   nn      =  Size%nbedit*groups

!  Initialize partial current edits

   RadEdit% RadEnergyEscRate(:) = zero
   RadEdit% RadEnergyIncRate(:) = zero

!  Accumulate exiting and incoming partial currents for all
!  non-reflecting boundaries.  Partial currents are accumulated
!  by boundary-edit index (1 -> nbedit) and also for all
!  problem boundaries (nbedit+1).

   VacuumLoop: do i=1,nVacuum

     Bdy      => getVacuum(RadBoundary, i)

     nBdyElem =  getNumberOfBdyElements(Bdy)
     iedit    =  getEditID(Bdy)
     b0       =  getFirstBdyElement(Bdy) - 1
     n        =  (iedit-1)*groups

!  Compute (unit normal) dot (omega)*area and incident/exiting currents 

     AngleLoop1: do ia=1,nangles

       BELoop1: do ib=1,nBdyElem

         if (ndim == 1 .or. ndim == 3) then
           gfac = one
         elseif (ndim == 2) then
           gfac = twopi*Bdy% Radius(ib)
         endif

         angdota = DOT_PRODUCT( QuadSet%omega(:,ia),Bdy%A_bdy(:,ib) )

         if (angdota > zero) then
           do ig=1,groups
             current = gfac*QuadSet%weight(ia)*angdota*psib(ig,b0+ib,ia)
             RadEdit% RadEnergyEscRate(nn+ig) =   &
             RadEdit% RadEnergyEscRate(nn+ig) + current
             RadEdit% RadEnergyEscRate(n+ig)  =   &
             RadEdit% RadEnergyEscRate(n+ig)  + current 
           enddo
         endif

       enddo BELoop1

     enddo AngleLoop1

   enddo VacuumLoop

!  Source Boundaries

   SourceLoop: do i=1,nSource

     Bdy      => getSource(RadBoundary, i)

     nBdyElem =  getNumberOfBdyElements(Bdy)
     iedit    =  getEditID(Bdy)
     b0       =  getFirstBdyElement(Bdy) - 1
     n        =  (iedit-1)*groups

!  Compute (unit normal) dot (omega)*area and incident/exiting currents
                                                                                                    
     AngleLoop2: do ia=1,nangles

       BELoop2: do ib=1,nBdyElem

         if (ndim == 1 .or. ndim == 3) then
           gfac = one
         elseif (ndim == 2) then
           gfac = twopi*Bdy% Radius(ib)
         endif
 
         angdota = DOT_PRODUCT( QuadSet%omega(:,ia),Bdy%A_bdy(:,ib) )
 
         if (angdota > zero) then
           do ig=1,groups
             current = gfac*QuadSet%weight(ia)*angdota*psib(ig,b0+ib,ia)
             RadEdit% RadEnergyEscRate(nn+ig) =   &
             RadEdit% RadEnergyEscRate(nn+ig) + current
             RadEdit% RadEnergyEscRate(n+ig)  =   &
             RadEdit% RadEnergyEscRate(n+ig)  + current
           enddo
         else
           do ig=1,groups
             current = gfac*QuadSet%weight(ia)*angdota*psib(ig,b0+ib,ia)
             RadEdit% RadEnergyIncRate(nn+ig) =   &
             RadEdit% RadEnergyIncRate(nn+ig) - current
             RadEdit% RadEnergyIncRate(n+ig)  =   &
             RadEdit% RadEnergyIncRate(n+ig)  - current
           enddo
         endif
                                                                                                    
       enddo BELoop2
 
     enddo AngleLoop2
          
   enddo SourceLoop


   call MPIAllReduceT(RadEdit% RadEnergyEscRate, "sum", MPI_COMM_WORLD)
   call MPIAllReduceT(RadEdit% RadEnergyIncRate, "sum", MPI_COMM_WORLD)


   return
   end subroutine bdyedt


