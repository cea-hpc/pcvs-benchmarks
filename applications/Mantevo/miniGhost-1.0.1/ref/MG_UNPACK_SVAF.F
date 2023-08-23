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

MODULE MG_UNPACK_SVAF_MOD

   ! Purpose
   ! =======
   ! Complete incoming non-blocking MPI messages, and unpack data
   ! into appropriate variables.

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD
   USE MG_GET_FACE_MOD
   USE MG_PROFILING_MOD

   IMPLICIT NONE

CONTAINS
   
   SUBROUTINE MG_UNPACK_SVAF ( GRID, IERR )
   
      ! -------------------------------------
      ! Unpack neighbor recvd data into GRID.
      ! -------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1), INTENT(INOUT) :: &
         GRID

      ! ------------------
      ! Local Declarations
      ! ------------------
   
      INTEGER ::        &
         IWHICH,                      &  ! MPI_Wait_any SRC process
         I, L,                        &  ! Counters
         NUM_RECVS_OUTSTANDING,       &  ! Keep track of progress
         NUM_SENDS_OUTSTANDING,       &
         OFFSET
   
#if defined _MG_MPI
      INTEGER ::     &
         ISTAT(MPI_STATUS_SIZE)
#endif

      REAL(KIND=MG_REAL8) ::         &
         TIME_START,              &
         TIME_START_DIR,          &
         TIME_WAIT,               &
         TIME_WAIT_X,             &
         TIME_WAIT_Y,             &
         TIME_WAIT_Z

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      IF ( NUMPES == 1 ) &
         RETURN

#if defined _MG_MPI

      TIME_WAIT_X = 0.0
      TIME_WAIT_Y = 0.0
      TIME_WAIT_Z = 0.0

      TIME_START = MG_TIMER()

      NUM_SENDS_OUTSTANDING = NUM_SENDS
      NUM_RECVS_OUTSTANDING = NUM_RECVS

      ! Complete sends and receives. 
      ! If recv, unpack into user space (domain face).

      DO L = 1, NUM_RECVS + NUM_SENDS

         TIME_START_DIR = MG_TIMER ()
         CALL MPI_WAITANY ( MAX_NUM_SENDS + MAX_NUM_RECVS, MSG_REQS, IWHICH, ISTAT, IERR )
         CALL MG_ASSERT ( IERR, 'MG_UNPACK_SVAF: MPI_WAITANY', ISTAT(MPI_ERROR) )

         IF ( IWHICH > MAX_NUM_RECVS ) THEN

            NUM_SENDS_OUTSTANDING = NUM_SENDS_OUTSTANDING - 1
            I = IWHICH - MAX_NUM_RECVS
            IF (I.EQ.BACK .OR. I.EQ.FRONT) THEN
               TIME_WAIT_Z = TIME_WAIT_Z + MG_TIMER () - TIME_START_DIR
            ELSE IF (I.EQ.EAST .OR. I.EQ.WEST) THEN
               TIME_WAIT_X = TIME_WAIT_X + MG_TIMER () - TIME_START_DIR
            ELSE IF (I.EQ.NORTH .OR. I.EQ.SOUTH) THEN
               TIME_WAIT_Y = TIME_WAIT_Y + MG_TIMER () - TIME_START_DIR
            END IF

         ELSE 

            IF (IWHICH.EQ.BACK .OR. IWHICH.EQ.FRONT) THEN
               TIME_WAIT_Z = TIME_WAIT_Z + MG_TIMER () - TIME_START_DIR
            ELSE IF (IWHICH.EQ.EAST .OR. IWHICH.EQ.WEST) THEN
               TIME_WAIT_X = TIME_WAIT_X + MG_TIMER () - TIME_START_DIR
            ELSE IF (IWHICH.EQ.NORTH .OR. IWHICH.EQ.SOUTH) THEN
               TIME_WAIT_Y = TIME_WAIT_Y + MG_TIMER () - TIME_START_DIR
            END IF
            TIME_START_DIR = MG_TIMER ()

            OFFSET = 0
            CALL MG_GET_FACE ( GRID, IWHICH, OFFSET, IERR )
            NUM_RECVS_OUTSTANDING = NUM_RECVS_OUTSTANDING - 1
            IF (IWHICH.EQ.BACK .OR. IWHICH.EQ.FRONT) THEN
               MG_PERF%TIME_UNPACK_Z_PE = MG_PERF%TIME_UNPACK_Z_PE +     &
                                          MG_TIMER () - TIME_START_DIR
            ELSE IF (IWHICH.EQ.EAST .OR. IWHICH.EQ.WEST) THEN
               MG_PERF%TIME_UNPACK_X_PE = MG_PERF%TIME_UNPACK_X_PE +     &
                                          MG_TIMER () - TIME_START_DIR
            ELSE IF (IWHICH.EQ.NORTH .OR. IWHICH.EQ.SOUTH) THEN
               MG_PERF%TIME_UNPACK_Y_PE = MG_PERF%TIME_UNPACK_Y_PE +     &
                                          MG_TIMER () - TIME_START_DIR
            END IF

         END IF
      END DO

      CALL MG_ASSERT ( NUM_SENDS_OUTSTANDING, 'MG_UNPACK_SVAF: NUM_SENDS_OUTSTANDING',  &
                         NUM_SENDS )
      CALL MG_ASSERT ( NUM_RECVS_OUTSTANDING, 'MG_UNPACK_SVAF: NUM_RECVS_OUTSTANDING',  &
                         NUM_RECVS )

      NUM_RECVS = 0
      NUM_SENDS = 0

      TIME_WAIT = TIME_WAIT_X +  TIME_WAIT_Y + TIME_WAIT_Z
      MG_PERF%TIME_UNPACK_PE = MG_PERF%TIME_UNPACK_PE + MG_TIMER() - &
                               TIME_START - TIME_WAIT
      MG_PERF%TIME_WAIT_PE = MG_PERF%TIME_WAIT_PE + TIME_WAIT
      MG_PERF%TIME_WAIT_X_PE = MG_PERF%TIME_WAIT_X_PE + TIME_WAIT_X
      MG_PERF%TIME_WAIT_Y_PE = MG_PERF%TIME_WAIT_Y_PE + TIME_WAIT_Y
      MG_PERF%TIME_WAIT_Z_PE = MG_PERF%TIME_WAIT_Z_PE + TIME_WAIT_Z

#endif _MG_MPI

      RETURN
   
   END SUBROUTINE MG_UNPACK_SVAF
   
END MODULE MG_UNPACK_SVAF_MOD
