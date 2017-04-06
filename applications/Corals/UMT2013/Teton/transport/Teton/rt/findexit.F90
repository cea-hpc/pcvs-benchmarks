!***********************************************************************
!                        Version 1:  12/98, PFN                        *
!                                                                      *
!   FINDEXIT - Find the angles that are exiting for each boundary      *
!              element on a shared surface.                            *
!                                                                      *
!   Input:   A_bdy - outward normal vector for boundary elements   (A) *
!            omega - direction cosines                                 *
!                                                                      *
!   Output:  LASTINC/ - for each shared boundary element, points to    *
!            LASTEXIT   the last entry in LISTINC/LISTEXIT for that    *
!                       boundary element                               *
!            LISTINC  - ordered list (by boundary element) of all      *
!                       incident directions                            *
!            LISTEXIT - ordered list (by boundary element) of all      *
!                       exiting directions                             *
!            IBCOMM   - contains the following information for each    *
!                       communicator:                                  *
!                            1 = starting point in 'last' arrays       *
!                            2 = ending point in 'last' arrays         *
!                            3 = length of the send                    *
!                            4 = length of the receive                 *
!            NODEINDX - given a node number, returns the communicator  *
!                       number                                         *
!                                                                      *
!***********************************************************************

   subroutine findexit

   use kind_mod
   use constant_mod
   use Size_mod
   use Geometry_mod
   use Communicator_mod
   use QuadratureList_mod
   use Quadrature_mod
   use BoundaryList_mod
   use Boundary_mod

   implicit none

!  Include MPI

   include 'mpif.h'

!  Local

   integer    :: i,ia,ib,ishared,iset,  &
                 neighbor,HalfBdyElem,                &
                 istartL,ifinishL,istartN,ifinishN,   &
                 lensend,lenrecv,izero,ierr,ncount,   &
                 FirstBdyElem,LastBdyElem,NbdyElem,   &
                 angle,angle0,bin,NbL,b0 

   integer    :: my_node, nbshare, nbelem, nShared
   integer    :: NumQuadSets, NumAngles, NumBin, NangBin, Groups 

   integer    :: status(MPI_STATUS_SIZE,2),request(2)

   real(adqt) :: dot

!  Dynamic

   integer,  allocatable :: idot(:,:)
   integer,  allocatable :: ListSend(:)
   integer,  allocatable :: ListRecv(:) 
   integer,  allocatable :: nsendAngle(:)
   integer,  allocatable :: nrecvAngle(:)

!  Constants

   parameter (izero=0)
   parameter (ncount=2)


   DecompTest: if (Size% decomp_s == 'on') then

     my_node  = Size%my_node
     nbshare  = Size%nbshare
     nbelem   = Size%nbelem 
     nShared  = getNumberOfShared(RadBoundary)

!    Loop over angle sets

     NumQuadSets = getNumQuadSets(Quad)

     SetLoop: do iset=1,NumQuadSets

       QuadSet   => getQuadrature(Quad, iset)

       NumAngles = QuadSet% NumAngles
       NumBin    = QuadSet% NumBin
       Groups    = QuadSet% Groups

!      Allocate a buffer for communication

       allocate( idot(NumAngles,nbelem) )
       allocate( ListSend(NumAngles*nbshare) )
       allocate( ListRecv(NumAngles*nbshare) )
       allocate( nsendAngle(NumAngles) )
       allocate( nrecvAngle(NumAngles) )

       idot(:,:) = izero

!      Wait for all nodes to arrive

       call MPI_Barrier(MPI_COMM_WORLD, ierr)

!  Loop over all shared boundary elements and decide  
!  which angles are exiting and which are incident on 
!  shared surfaces (we only need to check the unique 
!  angle set).  Each process on a shared surface computes
!  dot products for half of the boundary elements and then
!  the results are exchanged.

       CommunicatorLoop1: do ishared=1,nShared

         Bdy          => getShared(RadBoundary, ishared)
         neighbor     =  getNeighborID(Bdy) 
         NbdyElem     =  getNumberOfBdyElements(Bdy) 
         FirstBdyElem =  getFirstBdyElement(Bdy) 
         b0           =  FirstBdyElem - 1
         HalfBdyElem  =  NbdyElem/2
         LastBdyElem  =  FirstBdyElem + NbdyElem - 1

         if (my_node < neighbor) then
           istartL  = FirstBdyElem 
           istartN  = FirstBdyElem + HalfBdyElem 
           ifinishL = FirstBdyElem + HalfBdyElem - 1 
           ifinishN = LastBdyElem 
         else
           istartL  = FirstBdyElem + HalfBdyElem 
           istartN  = FirstBdyElem 
           ifinishL = LastBdyElem 
           ifinishN = FirstBdyElem + HalfBdyElem - 1 
         endif

         NbL        = ifinishL - istartL + 1
         lensend    = NumAngles*NbL
         lenrecv    = NumAngles*(NbdyElem - NbL) 

!  Start receives

         call MPI_Irecv(idot(1,istartN), lenrecv, MPI_INTEGER, &
                        neighbor, 400, MPI_COMM_WORLD, request(2), ierr)

!  Loop over boundary elements

         BoundaryElements1: do ib=istartL,ifinishL

           AngleLoop1: do ia=1,NumAngles

             dot = DOT_PRODUCT( QuadSet%omega(:,ia),Bdy%A_bdy(:,ib-b0) ) 

             if (dot < zero) then
               idot(ia,ib) = -1
             elseif (dot > zero) then
               idot(ia,ib) =  1
             endif

           enddo AngleLoop1 

       enddo BoundaryElements1 

       call MPI_Isend(idot(1,istartL), lensend, MPI_INTEGER, &
                      neighbor, 400, MPI_COMM_WORLD, request(1), ierr)

       call MPI_Waitall(ncount, request, status, ierr)

!  Dot products received from neighbor have the opposite sign

       idot(:,istartN:ifinishN) = -idot(:,istartN:ifinishN)

     enddo CommunicatorLoop1


!  Now create the lists of incident and exiting boundary elements 
!  for each angle in the quadrature set

     angle0 = 0 

     AngleBin: do bin=1,NumBin

       NangBin = QuadSet% NangBinList(bin) 

       CommunicatorLoop2: do ishared=1,nShared

         Comm => getMessage(QuadSet, bin, ishared)
         Bdy  => getShared(RadBoundary, ishared)

         NBdyElem     = getNumberOfBdyElements(Bdy) 
         FirstBdyElem = getFirstBdyElement(Bdy) 
         LastBdyElem  = FirstBdyElem + NBdyElem - 1

         lensend = 0
         lenrecv = 0

         AngleLoop2: do ia=1,NangBin

           angle          = angle0 + ia
           nsendAngle(ia) = 0
           nrecvAngle(ia) = 0

           BoundaryElements2: do ib=FirstBdyElem,LastBdyElem

             if (idot(angle,ib) < izero) then
               lenrecv           = lenrecv + 1
               nrecvAngle(ia)    = nrecvAngle(ia) + 1
               ListRecv(lenrecv) = ib
             elseif (idot(angle,ib) > izero) then
               lensend           = lensend + 1
               nsendAngle(ia)    = nsendAngle(ia) + 1
               ListSend(lensend) = ib
             endif

           enddo BoundaryElements2

         enddo AngleLoop2

!  Allocate send/receive buffers for this message
         call constructBuffer(Comm, lensend, lenrecv, Groups, NangBin, &
                              nsendAngle, nrecvAngle)

         Comm%ListRecv(1:lenrecv) = ListRecv(1:lenrecv)
         Comm%ListSend(1:lensend) = ListSend(1:lensend)
 
       enddo CommunicatorLoop2

       angle0 = angle0 + NangBin 

     enddo AngleBin

!  Initialize communication handles for persistent communicators

     call initcomm

!  Use an assertion here to check that neighboring nodes agree
!  on the incident and exiting angle lists

!  Release memory

       deallocate( idot )
       deallocate( ListSend )
       deallocate( ListRecv )
       deallocate( nsendAngle )
       deallocate( nrecvAngle )

       call MPI_Barrier(MPI_COMM_WORLD, ierr)

     enddo SetLoop


   endif DecompTest


   return
   end subroutine findexit

