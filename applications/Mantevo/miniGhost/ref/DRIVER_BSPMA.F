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

MODULE DRIVER_BSPMA_MOD

CONTAINS

   SUBROUTINE DRIVER_BSPMA ( GRID, IERR )

   ! Purpose
   ! =======
   ! Driver for the bulk synchronous parallel with message aggregation
   ! communication strategy. Called by the Fortran driver subroutine MINI_GHOST.

   USE MG_CONSTANTS_MOD
   USE MG_CHECKPOINT_MOD
   USE MG_BSPMA_MOD
   USE MG_BSPMA_DIAGS_MOD
   USE MG_STENCIL_MOD
   USE MG_PROFILING_MOD
   USE MG_SUM_GRID_MOD

   IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) :: &
         IERR                       ! Return status

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS), INTENT(INOUT) :: &
         GRID

      ! ------------------
      ! Local Declarations
      ! ------------------

      LOGICAL :: RESTART_FIRST_PASS=.FALSE.

      INTEGER ::                  &
         IVAR,                    &  ! Counter (over variables)
         ISPIKE,                  &  ! Counter (over NSPIKES)
         NUM_ERRORS,              &  ! Final check of the answer.
         STARTING_TSTEP=1,        &
         STARTING_SPIKE=1,        &
         TSTEP                       ! Time step counter.

      REAL(KIND=MG_REAL) ::       &
         ERROR_ITER,              &  ! Difference between new and old GRIDi sum.
         GSUM                        ! Global SUM across GRIDs.

      REAL(KIND=MG_REAL8) ::      &
         TIME_START,              &  ! Timing variable
         TIME_START_2,            &  ! Timing variable
         TIME_START_ALL              ! Timing variable

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      ! -------------------
      ! Begin time stepping
      ! -------------------

      CALL MG_CP_RESTART_GRID (GRID, RESTART_FIRST_PASS, STARTING_SPIKE, STARTING_TSTEP, GSUM, IERR )

      TIME_START_ALL = MG_TIMER()

      DO ISPIKE = STARTING_SPIKE, NSPIKES

         IF (RESTART_FIRST_PASS .EQV. .FALSE.) THEN
            CALL MG_INSERT_SPIKE ( GRID, ISPIKE, IERR )
         END IF

         DO TSTEP = STARTING_TSTEP, NTSTEPS

#if defined _MG_MPI
            TIME_START = MG_TIMER()
            IF ( STENCIL == STENCIL_2D5PT .OR. STENCIL == STENCIL_3D7PT ) THEN
               CALL MG_BSPMA ( GRID, IERR )
               CALL MG_ASSERT ( IERR, 'DRIVER_BSPMA: MG_BSPMA', TSTEP )
            ELSE IF ( STENCIL == STENCIL_2D9PT .OR. STENCIL == STENCIL_3D27PT ) THEN
               CALL MG_BSPMA_DIAGS ( GRID, IERR )
               CALL MG_ASSERT ( IERR, 'DRIVER_BSPMA: MG_BSPMA_DIAG', TSTEP )
            ELSE
               IERR = -1
               CALL MG_ASSERT ( IERR, 'DRIVER_BSPMA: Unknown stencil for boundary exchange', STENCIL )
            END IF
            MG_PERF%TIME_BSPMA_PE = MG_PERF%TIME_BSPMA_PE + MG_TIMER() - TIME_START
#endif

            IF ( STENCIL /= STENCIL_NONE ) THEN
               DO IVAR = 1, NVARS
                  TIME_START = MG_TIMER()
                  CALL MG_STENCIL ( GRID(0,0,0,IVAR), IVAR, IERR )
                  CALL MG_ASSERT ( IERR, 'DRIVER_BSPMA:MG_STENCIL', IVAR )
                  MG_PERF%TIME_STENCIL_PE = MG_PERF%TIME_STENCIL_PE + MG_TIMER() - TIME_START

                  ! Reduction across GRID option
                  IF ( GRIDS_TO_SUM(IVAR) ) THEN
                     TIME_START_2 = MG_TIMER ()
                     CALL MG_SUM_GRID ( GRID(0,0,0,IVAR), IVAR, GSUM, IERR )
                     CALL MG_ASSERT ( IERR, 'DRIVER_BSPMA:MG_SUM_GRID', IVAR )
                     MG_PERF%TIME_SUMGRID_PE = MG_PERF%TIME_SUMGRID_PE + MG_TIMER () - TIME_START_2

                     IF ( MYPE == ROOTPE ) THEN
                        ERROR_ITER = ABS ( SOURCE_TOTAL(IVAR) - GSUM ) / SOURCE_TOTAL(IVAR)
                        IF ( ERROR_ITER > ERROR_TOL ) THEN
                           WRITE(*,99) TSTEP, ISPIKE, IVAR, ERROR_ITER, ERROR_TOL
                           IERR = -1
                           CALL MG_ASSERT ( IERR, 'DRIVER_BSPMA', IVAR )
                        END IF
                        IF ( MOD ( TSTEP, REPORT_DIFFUSION ) == 0 ) THEN
                           WRITE(*,99) TSTEP, ISPIKE, IVAR, ERROR_ITER, ERROR_TOL
                        END IF
                     END IF
                  END IF
               END DO
            END IF

            CALL MG_CP_CHECKPOINT ( GRID, TSTEP, ISPIKE, GSUM, IERR )

         END DO ! Time step

         STARTING_TSTEP=1
         RESTART_FIRST_PASS=.FALSE.

      END DO    ! SPIKE insertion.

      MG_PERF%TIME_WALL_PE = MG_TIMER() - TIME_START_ALL

      ! Final correctness check
      NUM_ERRORS = 0
      DO IVAR = 1, NVARS
         CALL MG_SUM_GRID ( GRID(0,0,0,IVAR), IVAR, GSUM, IERR )
         CALL MG_ASSERT ( IERR, 'DRIVER_BSPMA:MG_SUM_GRID(Final check)', IVAR )
         MG_PERF%TIME_SUMGRID_PE = MG_PERF%TIME_SUMGRID_PE + MG_TIMER () - TIME_START_2

         IF ( MYPE == ROOTPE ) THEN
            ERROR_ITER = ABS ( SOURCE_TOTAL(IVAR) - GSUM ) / SOURCE_TOTAL(IVAR)
            IF ( ERROR_ITER > ERROR_TOL ) THEN
               WRITE(*,99) TSTEP, ISPIKE, IVAR, ERROR_ITER, ERROR_TOL
               NUM_ERRORS = NUM_ERRORS + 1
            END IF
         END IF
      END DO

      IF ( MYPE == ROOTPE ) THEN
         IF ( NUM_ERRORS == 0 ) THEN
            WRITE(*,*)
            WRITE(*,*) 'Computation within error tolerance.'
            WRITE(*,*)
         END IF
      END IF

      RETURN

 99   FORMAT ( 'Time step ', I4, ' for spike ', I3, ' for variable ', I4, &
               ' the error is ', 1PE12.5 '; error tolerance is ', 1PE12.5, '.' )

   END SUBROUTINE DRIVER_BSPMA

END MODULE DRIVER_BSPMA_MOD
