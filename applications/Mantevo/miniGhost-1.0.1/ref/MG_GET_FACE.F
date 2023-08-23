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

MODULE MG_GET_FACE_MOD

   ! Purpose
   ! =======
   ! Unpack received data buffers into variable faces.

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD

   IMPLICIT NONE

CONTAINS

   SUBROUTINE MG_GET_FACE ( GRID, WHICH, OFFSET, IERR )

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(IN) :: &
         WHICH

      INTEGER, INTENT(INOUT) :: &
         OFFSET

      INTEGER, INTENT(OUT) :: &
         IERR

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(INOUT) :: &
         GRID

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER :: &
         I, J, K                  ! Counters

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      SELECT CASE ( WHICH )

         CASE ( BACK )

!$OMP PARALLEL DO
            DO J = 0, NY+1
               DO I = 0, NX+1
                  GRID ( I, J, 0 ) = RECV_BUFFER_BACK(OFFSET + J*(NX+2) + I+1)
               END DO
            END DO
!$OMP END PARALLEL DO
            OFFSET = OFFSET + (NY+2)*(NX+2)

         CASE ( FRONT )

!$OMP PARALLEL DO
            DO J = 0, NY+1
               DO I = 0, NX+1
                  GRID ( I, J, NZ+1 ) = RECV_BUFFER_FRONT(OFFSET + J*(NX+2) + I+1)
               END DO
            END DO
!$OMP END PARALLEL DO
            OFFSET = OFFSET + (NY+2)*(NX+2)

         CASE ( EAST )

!$OMP PARALLEL DO
            DO K = 0, NZ+1
               DO J = 0, NY+1
                  GRID ( NX+1, J, K ) = RECV_BUFFER_EAST(OFFSET + K*(NY+2) + J+1)
               END DO
            END DO
!$OMP END PARALLEL DO
            OFFSET = OFFSET + (NZ+2)*(NY+2)

         CASE ( WEST )

!$OMP PARALLEL DO
            DO K = 0, NZ+1
               DO J = 0, NY+1
                  GRID ( 0, J, K ) = RECV_BUFFER_WEST(OFFSET + K*(NY+2) + J+1)
               END DO
            END DO
!$OMP END PARALLEL DO
            OFFSET = OFFSET + (NZ+2)*(NY+2)

         CASE ( NORTH )

!$OMP PARALLEL DO
            DO K = 0, NZ+1
               DO I = 0, NX+1
                  GRID ( I, NY+1, K ) = RECV_BUFFER_NORTH(OFFSET + K*(NX+2) + I+1)
               END DO
            END DO
!$OMP END PARALLEL DO
            OFFSET = OFFSET + (NZ+2)*(NX+2)

         CASE ( SOUTH )

!$OMP PARALLEL DO
            DO K = 0, NZ+1
               DO I = 0, NX+1
                  GRID ( I, 0, K ) = RECV_BUFFER_SOUTH(OFFSET + K*(NX+2) + I+1)
               END DO
            END DO
!$OMP END PARALLEL DO
            OFFSET = OFFSET + (NZ+2)*(NX+2)

      END SELECT

      RETURN

   END SUBROUTINE MG_GET_FACE

END MODULE MG_GET_FACE_MOD
