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

MODULE MG_BUFINIT_MOD

   ! Purpose
   ! =======
   ! Driver for selecting buffer requirements based on communication 
   ! strategy and other choices.
   ! Called from DRIVER.

   USE MG_CONSTANTS_MOD
   USE MG_UTILS_MOD

   IMPLICIT NONE

CONTAINS
   
   SUBROUTINE MG_BUFINIT ( IERR )
   
      ! ----------------------------------------------------------------
      ! Pack boundary data into send buffer for subsequent transmission.
      ! ----------------------------------------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR           ! Return status

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER :: &
         I                      ! Counter

      REAL :: &
         NUM,               &  ! Tmp random number.
         PERCENT_SUM_REAL      ! Requested percentage of GRIDS_TO_SUM
   
      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      ! ---------------------------
      ! Reduction sum across GRIDs.
      ! ---------------------------

      ALLOCATE ( GRIDS_TO_SUM(NVARS), STAT=IERR )
      CALL MG_ASSERT ( IERR, 'MG_BUFINFO: ALLOCATE(GRIDS_TO_SUM)', NVARS )

      IF ( DEBUG_GRID == 1 ) THEN
         PERCENT_SUM = 100 ! If we're checking, we're checking them all.
      END IF

      IF ( PERCENT_SUM == 100 ) THEN

         GRIDS_TO_SUM = .TRUE.    ! Every GRID is summed.
         NUM_SUM_GRID = NVARS

      ELSE IF ( PERCENT_SUM == 0 ) THEN

         GRIDS_TO_SUM = .FALSE.   ! No GRIDs are summed.
         NUM_SUM_GRID = 0

      ELSE

         PERCENT_SUM_REAL = REAL(PERCENT_SUM) / 100.0
         NUM_SUM_GRID = 0

         CALL MG_RANDOM_SEED ( IERR )

         DO I = 1, NVARS
            CALL RANDOM_NUMBER ( NUM )
            IF ( NUM < PERCENT_SUM ) THEN
               GRIDS_TO_SUM(I) = .TRUE.
               NUM_SUM_GRID = NUM_SUM_GRID + 1
            ELSE
               GRIDS_TO_SUM(I) = .FALSE.
            END IF
         END DO

      END IF

      ALLOCATE ( FLUX_OUT(NVARS), STAT=IERR )
      CALL MG_ASSERT ( IERR, 'MG_BUFINFO: ALLOCATE(FLUX_OUT)', NVARS )
      FLUX_OUT = 0.0

      IF ( NUMPES == 1 ) &
         RETURN

#if defined _MG_MPI

      SELECT CASE ( COMM_METHOD )

         CASE ( COMM_METHOD_BSPMA )

            CALL MG_BUF_BSPMA ( IERR )

         CASE ( COMM_METHOD_SVAF )

            CALL MG_BUF_SVAF ( IERR )

         CASE DEFAULT

            CALL MG_ASSERT ( -1, 'MG_BUFINIT: Unknown COMM_METHOD', COMM_METHOD )

      END SELECT

      ! -----------------
      ! Set message tags.
      ! -----------------

      ALLOCATE ( MSG_TAGS(MAX_NUM_NEIGHBORS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_BUFINFO: ALLOCATE(MSG_TAGS)', MAX_NUM_NEIGHBORS )

      MSG_TAGS(NORTH) = 1000
      MSG_TAGS(SOUTH) = 2000
      MSG_TAGS(EAST)  = 3000
      MSG_TAGS(WEST)  = 4000
      MSG_TAGS(BACK)  = 5000
      MSG_TAGS(FRONT) = 6000

      ! -----------------------------
      ! Set msg request handle space.
      ! -----------------------------

      ALLOCATE ( MSG_REQS(MAX_NUM_SENDS+MAX_NUM_RECVS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_BUFINFO: ALLOCATE(MSG_REQS)', &
                         MAX_NUM_SENDS+MAX_NUM_RECVS )

      MSG_REQS(1:MAX_NUM_SENDS+MAX_NUM_RECVS) = MPI_REQUEST_NULL

#endif

      RETURN

   END SUBROUTINE MG_BUFINIT

!  ========================================================================================
   
   SUBROUTINE MG_BUF_BSPMA ( IERR )
   
      ! -------------------------------
      ! Buffer space specific to BSPMA.
      ! -------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(OUT) :: &
         IERR           ! Return status

      ! ------------------
      ! Local Declarations
      ! ------------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      IF ( NUMPES == 1 ) &
         RETURN

      IF ( NEIGHBORS(BACK) /= -1 ) THEN

         SEND_BUFFER_BACK_SIZE = NVARS * (NX+2)*(NY+2)
         ALLOCATE ( SEND_BUFFER_BACK( SEND_BUFFER_BACK_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_BACK', SEND_BUFFER_BACK_SIZE )

         RECV_BUFFER_BACK_SIZE = NVARS * (NX+2)*(NY+2)
         ALLOCATE ( RECV_BUFFER_BACK( RECV_BUFFER_BACK_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_BACK', RECV_BUFFER_BACK_SIZE )

      END IF

      IF ( NEIGHBORS(FRONT) /= -1 ) THEN

         SEND_BUFFER_FRONT_SIZE = NVARS * (NX+2)*(NY+2)
         ALLOCATE ( SEND_BUFFER_FRONT( SEND_BUFFER_FRONT_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_FRONT', SEND_BUFFER_FRONT_SIZE )

         RECV_BUFFER_FRONT_SIZE = NVARS * (NX+2)*(NY+2)
         ALLOCATE ( RECV_BUFFER_FRONT( RECV_BUFFER_FRONT_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_FRONT', RECV_BUFFER_FRONT_SIZE )

      END IF

      IF ( NEIGHBORS(EAST) /= -1 ) THEN

         SEND_BUFFER_EAST_SIZE = NVARS * (NY+2)*(NZ+2)
         ALLOCATE ( SEND_BUFFER_EAST( SEND_BUFFER_EAST_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_EAST', SEND_BUFFER_EAST_SIZE )

         RECV_BUFFER_EAST_SIZE = NVARS * (NY+2)*(NZ+2)
         ALLOCATE ( RECV_BUFFER_EAST( RECV_BUFFER_EAST_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_EAST', RECV_BUFFER_EAST_SIZE )

      END IF

      IF ( NEIGHBORS(WEST) /= -1 ) THEN

         SEND_BUFFER_WEST_SIZE = NVARS * (NY+2)*(NZ+2)
         ALLOCATE ( SEND_BUFFER_WEST( SEND_BUFFER_WEST_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_WEST', SEND_BUFFER_WEST_SIZE )

         RECV_BUFFER_WEST_SIZE = NVARS * (NY+2)*(NZ+2)
         ALLOCATE ( RECV_BUFFER_WEST( RECV_BUFFER_WEST_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_WEST', RECV_BUFFER_WEST_SIZE )

      END IF

      IF ( NEIGHBORS(NORTH) /= -1 ) THEN

         SEND_BUFFER_NORTH_SIZE = NVARS * (NX+2)*(NZ+2)
         ALLOCATE ( SEND_BUFFER_NORTH( SEND_BUFFER_NORTH_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_NORTH', SEND_BUFFER_NORTH_SIZE )

         RECV_BUFFER_NORTH_SIZE = NVARS * (NX+2)*(NZ+2)
         ALLOCATE ( RECV_BUFFER_NORTH( RECV_BUFFER_NORTH_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_NORTH', RECV_BUFFER_NORTH_SIZE )

      END IF

      IF ( NEIGHBORS(SOUTH) /= -1 ) THEN

         SEND_BUFFER_SOUTH_SIZE = NVARS * (NX+2)*(NZ+2)
         ALLOCATE ( SEND_BUFFER_SOUTH( SEND_BUFFER_SOUTH_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: SEND_BUFFER_SOUTH', SEND_BUFFER_SOUTH_SIZE )

         RECV_BUFFER_SOUTH_SIZE = NVARS * (NX+2)*(NZ+2)
         ALLOCATE ( RECV_BUFFER_SOUTH( RECV_BUFFER_SOUTH_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_BSPMA: RECV_BUFFER_SOUTH', RECV_BUFFER_SOUTH_SIZE )

      END IF

      MAX_NUM_SENDS = MAX_NUM_NEIGHBORS
      MAX_NUM_RECVS = MAX_NUM_NEIGHBORS

      RETURN

   END SUBROUTINE MG_BUF_BSPMA

!  ========================================================================================
   
   SUBROUTINE MG_BUF_SVAF ( IERR )
   
      ! -------------------------------
      ! Buffer space specific to BSPMA.
      ! -------------------------------

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(OUT) :: &
         IERR           ! Return status

      ! ------------------
      ! Local Declarations
      ! ------------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      IF ( NUMPES == 1 ) &
         RETURN

      IF ( NEIGHBORS(BACK) /= -1 ) THEN

         SEND_BUFFER_BACK_SIZE = (NX+2)*(NY+2)
         ALLOCATE ( SEND_BUFFER_BACK( SEND_BUFFER_BACK_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_BACK', SEND_BUFFER_BACK_SIZE )

         RECV_BUFFER_BACK_SIZE = (NX+2)*(NY+2)
         ALLOCATE ( RECV_BUFFER_BACK( RECV_BUFFER_BACK_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_BACK', RECV_BUFFER_BACK_SIZE )

      END IF

      IF ( NEIGHBORS(FRONT) /= -1 ) THEN

         SEND_BUFFER_FRONT_SIZE = (NX+2)*(NY+2)
         ALLOCATE ( SEND_BUFFER_FRONT( SEND_BUFFER_FRONT_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_FRONT', SEND_BUFFER_FRONT_SIZE )

         RECV_BUFFER_FRONT_SIZE = (NX+2)*(NY+2)
         ALLOCATE ( RECV_BUFFER_FRONT( RECV_BUFFER_FRONT_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_FRONT', RECV_BUFFER_FRONT_SIZE )

      END IF

      IF ( NEIGHBORS(EAST) /= -1 ) THEN

         SEND_BUFFER_EAST_SIZE =  (NY+2)*(NZ+2)
         ALLOCATE ( SEND_BUFFER_EAST( SEND_BUFFER_EAST_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_EAST', SEND_BUFFER_EAST_SIZE )

         RECV_BUFFER_EAST_SIZE =  (NY+2)*(NZ+2)
         ALLOCATE ( RECV_BUFFER_EAST( RECV_BUFFER_EAST_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_EAST', RECV_BUFFER_EAST_SIZE )

      END IF

      IF ( NEIGHBORS(WEST) /= -1 ) THEN

         SEND_BUFFER_WEST_SIZE =  (NY+2)*(NZ+2)
         ALLOCATE ( SEND_BUFFER_WEST( SEND_BUFFER_WEST_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_WEST', SEND_BUFFER_WEST_SIZE )

         RECV_BUFFER_WEST_SIZE =  (NY+2)*(NZ+2)
         ALLOCATE ( RECV_BUFFER_WEST( RECV_BUFFER_WEST_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_WEST', RECV_BUFFER_WEST_SIZE )

      END IF

      IF ( NEIGHBORS(NORTH) /= -1 ) THEN

         SEND_BUFFER_NORTH_SIZE = (NX+2)*(NZ+2)
         ALLOCATE ( SEND_BUFFER_NORTH( SEND_BUFFER_NORTH_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_NORTH', SEND_BUFFER_NORTH_SIZE )

         RECV_BUFFER_NORTH_SIZE = (NX+2)*(NZ+2)
         ALLOCATE ( RECV_BUFFER_NORTH( RECV_BUFFER_NORTH_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_NORTH', RECV_BUFFER_NORTH_SIZE )

      END IF

      IF ( NEIGHBORS(SOUTH) /= -1 ) THEN

         SEND_BUFFER_SOUTH_SIZE = (NX+2)*(NZ+2)
         ALLOCATE ( SEND_BUFFER_SOUTH( SEND_BUFFER_SOUTH_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: SEND_BUFFER_SOUTH', SEND_BUFFER_SOUTH_SIZE )

         RECV_BUFFER_SOUTH_SIZE = (NX+2)*(NZ+2)
         ALLOCATE ( RECV_BUFFER_SOUTH( RECV_BUFFER_SOUTH_SIZE ), STAT = IERR )
         CALL MG_ASSERT ( IERR, 'MG_BUF_SVAF: RECV_BUFFER_SOUTH', RECV_BUFFER_SOUTH_SIZE )

      END IF

      MAX_NUM_SENDS = MAX_NUM_NEIGHBORS
      MAX_NUM_RECVS = MAX_NUM_NEIGHBORS

      RETURN

   END SUBROUTINE MG_BUF_SVAF

!  ========================================================================================

END MODULE MG_BUFINIT_MOD

