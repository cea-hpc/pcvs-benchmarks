!***********************************************************************
!                        Version 1:  11/98, PFN                        *
!                                                                      *
!   EXCHANGE - Sends and receives boundary fluxes on shared surfaces.  *
!                                                                      *
!   Input:   PSIB  - boundary intensities                      (E/A/t) *
!                                                                      *
!   Output:  PSIB  - boundary intensities after communication  (E/A/t) *
!                                                                      *
!***********************************************************************

   subroutine exchange(PSIB, binSend, binRecv) 

   use kind_mod
   use Size_mod
   use Quadrature_mod
   use Communicator_mod

   implicit none

!  Include MPI 

   include 'mpif.h'

!  Arguments

   integer, intent(in)       :: binSend, binRecv

   real(adqt), intent(inout) :: psib(QuadSet%Groups,Size%nbelem,QuadSet%NumAngles) 

!  Local

   integer    :: Angle,i,ia,ib,ig,ishared,message,ncomm,ngroups,  &
                 ierr,bin,mybin,nsend,nsend0,nrecv,nrecv0,NangBin 

   integer    :: binSend1, binSend2, binRecv1, binRecv2

   integer    :: status(MPI_STATUS_SIZE,2)

   real(adqt) :: time1, time2, dtime

!  Constants

   ncomm   = Size%ncomm
   ngroups = QuadSet%Groups

   if (binSend /= 0) then
      binSend1 = binSend
      binSend2 = binSend 
   else
      binSend1 = 1 
      binSend2 = QuadSet% NumBin 
   endif

   if (binRecv /= 0) then
      binRecv1 = binRecv
      binRecv2 = binRecv
   else
      binRecv1 = 1
      binRecv2 = QuadSet% NumBin
   endif

!  Exchange boundary information with neighbors
!  Loop over the number of angle bins (in most cases this is 1)

   DecompTest: if (Size% decomp_s == 'on') then

     time1 = MPI_Wtime()

     AngleBin1: do bin=binSend1,binSend2
       NangBin = QuadSet% NangBinList(bin)

!  Loop over the number of shared surfaces; for each shared surface
!  we issue one send and one receive

       SendLoop: do ishared=1,ncomm

         Comm => getMessage(QuadSet, bin, ishared)

!  Accumulate data to send

         if (Comm% lensend > 0) then

           message = 0 
           nsend0  = 0

!  Loop over exiting angle, boundary element pairs for this communicator 

           do ia=1,NangBin
             Angle = QuadSet% AngleOrder(ia,bin)
             nsend = Comm% nsend(ia)

             do i=1,nsend

               ib = Comm% ListSend(nsend0+i)

               do ig=1,ngroups
                 Comm% psibsend(message+ig) = psib(ig,ib,Angle)
               enddo

               message = message + ngroups

             enddo
             nsend0 = nsend0 + nsend
           enddo

!  Start send for this communicator (odd numbered handle)

           call MPI_Start(Comm% irequest(1), ierr)

         endif

       enddo SendLoop

     enddo AngleBin1

!  Make sure all sends are complete

     do bin=binSend1,binSend2
       do ishared=1,ncomm
         Comm => getMessage(QuadSet, bin, ishared)

         if (Comm% lensend > 0) then
           call MPI_Wait(Comm% irequest(1), status, ierr)
         endif
       enddo
     enddo

!  Process data we receive

     ReceiveLoop: do ishared=1,ncomm

       AngleBin2: do bin=binRecv1,binRecv2

         mybin   =  QuadSet% RecvOrder(bin,ishared)
         NangBin =  QuadSet% NangBinList(mybin)

         Comm    => getMessage(QuadSet, mybin, ishared)

         if (Comm% lenrecv > 0) then

!  Check for completion of the receive

           call MPI_Wait(Comm% irequest(2), status, ierr)

           message = 0 
           nrecv0  = 0

!  Loop over boundary elements that are incident for this communicator

           do ia=1,NangBin
             Angle = QuadSet% AngleOrder(ia,mybin)
             nrecv = Comm% nrecv(ia)

             do i=1,nrecv

               ib = Comm% ListRecv(nrecv0+i)

               do ig=1,ngroups
                 psib(ig,ib,Angle) = Comm% psibrecv(message+ig)
               enddo

               message = message + ngroups

             enddo
             nrecv0 = nrecv0 + nrecv
           enddo

         endif

       enddo AngleBin2

!  End loop over shared surfaces

     enddo ReceiveLoop


     time2 = MPI_Wtime()
     dtime = (time2 - time1)/60.d0

     Size%CommTimeCycle = Size%CommTimeCycle + dtime

   endif DecompTest



   return
   end subroutine exchange

