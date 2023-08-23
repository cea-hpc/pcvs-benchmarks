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

MODULE MG_SEND_BSPMA_MOD

   ! Purpose
   ! =======
   ! MPI transmission, via non-blocking send (MPI_ISEND), of boundary data
   ! for the BSPMA communication strategy.

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

CONTAINS
   
   SUBROUTINE MG_SEND_BSPMA ( IERR )
   
      ! -------------------------------------------------------
      ! Pack all variables for single message to each neighbor.
      ! -------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

      ! ------------------
      ! Local Declarations
      ! ------------------
   
      INTEGER :: &
         I, J,                        &  ! Counters
         MSG_REQS_OFFSET
   
      REAL(KIND=MG_REAL8) ::      &
         TIME_START,              &
         TIME_START_DIR

      ! ---------------------
      ! Executable Statements
      ! ---------------------
   
      IERR = 0

      IF ( NUMPES == 1 ) &
         RETURN

#if defined _MG_MPI

      TIME_START = MG_TIMER()

      ! ------------------------------------------------------
      ! Construct message buffers across variables, then send.
      ! ------------------------------------------------------
   
      NUM_SENDS = 0
      MSG_REQS_OFFSET = MAX_NUM_RECVS

      ! Back boundary
   
      IF ( NEIGHBORS(BACK) /= -1 ) THEN
         TIME_START_DIR = MG_TIMER ()
         NUM_SENDS = NUM_SENDS + 1
         MSG_REQS_OFFSET = MAX_NUM_RECVS + BACK
         CALL MPI_ISEND ( SEND_BUFFER_BACK, COUNT_SEND_BACK, MG_MPI_REAL, &
                          NEIGHBORS(BACK), MSG_TAGS(BACK),                &
                          MPI_COMM_MG, MSG_REQS(MSG_REQS_OFFSET), IERR )
         MG_PERF%TIME_SEND_Z_PE = MG_PERF%TIME_SEND_Z_PE + MG_TIMER () - &
                                  TIME_START_DIR

         MG_PERF%SEND_COUNT  = MG_PERF%SEND_COUNT + COUNT_SEND_BACK
         IF ( COUNT_SEND_BACK > MG_PERF%SEND_COUNT_MAX ) THEN
            MG_PERF%SEND_COUNT_MAX = COUNT_SEND_BACK
         END IF
         IF ( COUNT_SEND_BACK < MG_PERF%SEND_COUNT_MIN ) THEN
            MG_PERF%SEND_COUNT_MIN = COUNT_SEND_BACK
         END IF

      END IF
   
      ! Front boundary
   
      IF ( NEIGHBORS(FRONT) /= -1 ) THEN
         TIME_START_DIR = MG_TIMER ()
         NUM_SENDS = NUM_SENDS + 1
         MSG_REQS_OFFSET = MAX_NUM_RECVS + FRONT
         CALL MPI_ISEND ( SEND_BUFFER_FRONT, COUNT_SEND_FRONT, MG_MPI_REAL, &
                          NEIGHBORS(FRONT), MSG_TAGS(FRONT),                &
                          MPI_COMM_MG, MSG_REQS(MSG_REQS_OFFSET), IERR )
         MG_PERF%TIME_SEND_Z_PE = MG_PERF%TIME_SEND_Z_PE + MG_TIMER () - &
                                  TIME_START_DIR

         MG_PERF%SEND_COUNT  = MG_PERF%SEND_COUNT + COUNT_SEND_FRONT
         IF ( COUNT_SEND_FRONT > MG_PERF%SEND_COUNT_MAX ) THEN
            MG_PERF%SEND_COUNT_MAX = COUNT_SEND_FRONT
         END IF
         IF ( COUNT_SEND_FRONT < MG_PERF%SEND_COUNT_MIN ) THEN
            MG_PERF%SEND_COUNT_MIN = COUNT_SEND_FRONT
         END IF

      END IF
   
      ! East boundary
   
      IF ( NEIGHBORS(EAST) /= -1 ) THEN
         TIME_START_DIR = MG_TIMER ()
         NUM_SENDS = NUM_SENDS + 1
         MSG_REQS_OFFSET = MAX_NUM_RECVS + EAST
         CALL MPI_ISEND ( SEND_BUFFER_EAST, COUNT_SEND_EAST, MG_MPI_REAL, &
                          NEIGHBORS(EAST), MSG_TAGS(EAST),                &
                          MPI_COMM_MG, MSG_REQS(MSG_REQS_OFFSET), IERR )
         MG_PERF%TIME_SEND_X_PE = MG_PERF%TIME_SEND_X_PE + MG_TIMER () - &
                                  TIME_START_DIR

         MG_PERF%SEND_COUNT  = MG_PERF%SEND_COUNT + COUNT_SEND_EAST
         IF ( COUNT_SEND_EAST > MG_PERF%SEND_COUNT_MAX ) THEN
            MG_PERF%SEND_COUNT_MAX = COUNT_SEND_EAST
         END IF
         IF ( COUNT_SEND_EAST < MG_PERF%SEND_COUNT_MIN ) THEN
            MG_PERF%SEND_COUNT_MIN = COUNT_SEND_EAST
         END IF

      END IF
   
      ! West boundary
   
      IF ( NEIGHBORS(WEST) /= -1 ) THEN
         TIME_START_DIR = MG_TIMER ()
         NUM_SENDS = NUM_SENDS + 1
         MSG_REQS_OFFSET = MAX_NUM_RECVS + WEST
         CALL MPI_ISEND ( SEND_BUFFER_WEST, COUNT_SEND_WEST, MG_MPI_REAL, &
                          NEIGHBORS(WEST), MSG_TAGS(WEST),                &
                          MPI_COMM_MG, MSG_REQS(MSG_REQS_OFFSET), IERR )
         MG_PERF%TIME_SEND_X_PE = MG_PERF%TIME_SEND_X_PE + MG_TIMER () - &
                                  TIME_START_DIR

         MG_PERF%SEND_COUNT  = MG_PERF%SEND_COUNT + COUNT_SEND_WEST
         IF ( COUNT_SEND_WEST > MG_PERF%SEND_COUNT_MAX ) THEN
            MG_PERF%SEND_COUNT_MAX = COUNT_SEND_WEST
         END IF
         IF ( COUNT_SEND_WEST < MG_PERF%SEND_COUNT_MIN ) THEN
            MG_PERF%SEND_COUNT_MIN = COUNT_SEND_WEST
         END IF

      END IF
   
      ! North boundary
   
      IF ( NEIGHBORS(NORTH) /= -1 ) THEN
         TIME_START_DIR = MG_TIMER ()
         NUM_SENDS = NUM_SENDS + 1
         MSG_REQS_OFFSET = MAX_NUM_RECVS + NORTH
         CALL MPI_ISEND ( SEND_BUFFER_NORTH, COUNT_SEND_NORTH, MG_MPI_REAL, &
                          NEIGHBORS(NORTH), MSG_TAGS(NORTH),                &
                          MPI_COMM_MG, MSG_REQS(MSG_REQS_OFFSET), IERR )
         MG_PERF%TIME_SEND_Y_PE = MG_PERF%TIME_SEND_Y_PE + MG_TIMER () - &
                                  TIME_START_DIR

         MG_PERF%SEND_COUNT  = MG_PERF%SEND_COUNT + COUNT_SEND_NORTH
         IF ( COUNT_SEND_NORTH > MG_PERF%SEND_COUNT_MAX ) THEN
            MG_PERF%SEND_COUNT_MAX = COUNT_SEND_NORTH
         END IF
         IF ( COUNT_SEND_NORTH < MG_PERF%SEND_COUNT_MIN ) THEN
            MG_PERF%SEND_COUNT_MIN = COUNT_SEND_NORTH
         END IF

      END IF
   
      ! South boundary
   
      IF ( NEIGHBORS(SOUTH) /= -1 ) THEN
         TIME_START_DIR = MG_TIMER ()
         NUM_SENDS = NUM_SENDS + 1
         MSG_REQS_OFFSET = MAX_NUM_RECVS + SOUTH
         CALL MPI_ISEND ( SEND_BUFFER_SOUTH, COUNT_SEND_SOUTH, MG_MPI_REAL, &
                          NEIGHBORS(SOUTH), MSG_TAGS(SOUTH),                &
                          MPI_COMM_MG, MSG_REQS(MSG_REQS_OFFSET), IERR )
         MG_PERF%TIME_SEND_Y_PE = MG_PERF%TIME_SEND_Y_PE + MG_TIMER () - &
                                  TIME_START_DIR

         MG_PERF%SEND_COUNT  = MG_PERF%SEND_COUNT + COUNT_SEND_SOUTH
         IF ( COUNT_SEND_SOUTH > MG_PERF%SEND_COUNT_MAX ) THEN
            MG_PERF%SEND_COUNT_MAX = COUNT_SEND_SOUTH
         END IF
         IF ( COUNT_SEND_SOUTH < MG_PERF%SEND_COUNT_MIN ) THEN
            MG_PERF%SEND_COUNT_MIN = COUNT_SEND_SOUTH
         END IF

      END IF

      COUNT_SEND_NORTH = 0
      COUNT_SEND_SOUTH = 0
      COUNT_SEND_EAST  = 0
      COUNT_SEND_WEST  = 0
      COUNT_SEND_BACK  = 0
      COUNT_SEND_FRONT = 0

      MG_PERF%TIME_SEND_PE = MG_PERF%TIME_SEND_PE + MG_TIMER() - TIME_START

      MG_PERF%NUM_SENDS = MG_PERF%NUM_SENDS + NUM_SENDS

#endif _MG_MPI

      RETURN

   END SUBROUTINE MG_SEND_BSPMA
   
END MODULE MG_SEND_BSPMA_MOD
