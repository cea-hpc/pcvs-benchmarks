! ************************************************************************
!
!               miniGhost: stencil computations with boundary exchange.
!                 Copyright (2012) Sandia Corporation
!
! Under terms of Contract DE-AC04-94AL85000, there is a non-exclusive
! license for use of this work by or on behalf of the U.S. Government.
!
! This library is free software; you can redistribute it and/or modify
! it under the terms of the GNU Lesser General Public License as
! published by the Free Software Foundation; either version 2.1 of the
! License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful, but
! WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, write to the Free Software
! Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
! USA
! Questions? Contact Richard F. Barrett (rfbarre@sandia.gov) or
!                    Michael A. Heroux (maherou@sandia.gov)
!
! ************************************************************************

MODULE MG_BSPMA_MOD
 
   ! Purpose
   ! =======
   ! BSPMA boundary exchange for 5- and 7-point stencils: 
   !    pack, send, receive, unpack.
   ! Called from DRIVER_BSPMA.

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_IRECV_MOD
   USE MG_PACK_MOD
   USE MG_SEND_BSPMA_MOD
   USE MG_UNPACK_BSPMA_MOD

   IMPLICIT NONE

CONTAINS
   
   SUBROUTINE MG_BSPMA ( GRID, IERR )
   
      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(OUT) :: &
         IERR           ! Return status

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS), INTENT(INOUT) :: &
         GRID

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER(KIND=MG_INT) :: &
         IVAR          ! Counter
   
      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      CALL MG_IRECV ( IERR )

      COUNT_SEND_NORTH = 0
      COUNT_SEND_SOUTH = 0
      COUNT_SEND_EAST  = 0
      COUNT_SEND_WEST  = 0
      COUNT_SEND_BACK  = 0
      COUNT_SEND_FRONT = 0
 
      DO IVAR = 1, NVARS    ! This routine shared with SVAF, so loop over NVARS
         CALL MG_PACK ( GRID(0,0,0,IVAR),  IERR )
      END DO

      CALL MG_SEND_BSPMA ( IERR )

      CALL MG_UNPACK_BSPMA ( GRID, IERR )   ! This routine not shared with SVAF.

   END SUBROUTINE MG_BSPMA
   
END MODULE MG_BSPMA_MOD
