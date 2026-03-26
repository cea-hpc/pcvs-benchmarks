!***********************************************************************
!                        Version 1:  11/98, PFN                        *
!                                                                      *
!   INITEXCHANGE - Posts receives for boundary fluxes.                 *
!                                                                      *
!   Input:                                                             *
!                                                                      *
!   Output:                                                            *
!                                                                      *
!***********************************************************************

   subroutine InitExchange


   use kind_mod
   use Size_mod
   use Quadrature_mod
   use Communicator_mod

   implicit none

!  Include MPI 

   include 'mpif.h'

!  Local

   integer    :: bin, NumBin, ishared, ierr

   integer    :: status(MPI_STATUS_SIZE,2)


!  Wait for all nodes to arrive

   DecompTest: if (Size% decomp_s == 'on') then

     call MPI_Barrier(MPI_COMM_WORLD, ierr)


!  First, start all of the receives (receives use even numbered handles)

     NumBin = QuadSet% NumBin0

     do bin=1,NumBin
       if ( .not. QuadSet% Converged(bin) ) then
         do ishared=1,Size%ncomm
           Comm => getMessage(QuadSet, bin, ishared)
           if (Comm% lenrecv > 0) then 
             call MPI_Start(Comm% irequest(2), ierr)
           endif
         enddo
       endif
     enddo

     call MPI_Barrier(MPI_COMM_WORLD, ierr)

   endif DecompTest



   return
   end subroutine InitExchange

