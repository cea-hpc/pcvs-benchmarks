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

   SUBROUTINE MINI_GHOST ( SCALING_IN,          &
                           NX_IN,               &
                           NY_IN,               &
                           NZ_IN,               &
                           NVARS_IN,            &
                           PERCENT_SUM_IN,      &
                           NSPIKES_IN,          &
                           NTSTEPS_IN,          &
                           STENCIL_IN,          &
                           COMM_METHOD_IN,      &
                           BC_IN,               &
                           ERROR_TOL_IN,        &
                           REPORT_DIFFUSION_IN, &
                           NPX_IN,              &
                           NPY_IN,              &
                           NPZ_IN,              &
                           REPORT_PERF_IN,      &
                           CP_METHOD_IN,        &
                           CP_INTERVAL_IN,      &
                           CP_FILE_IN,          &
                           RESTART_CP_NUM_IN,   &
                           RESTART_FILE_IN,     &
                           DEBUG_GRID_IN        &
                           )

   ! Called from main(), this procedure serves as the Fortran driver for miniGhost.

   USE MG_CONSTANTS_MOD
   USE MG_OPTIONS_MOD
   USE MG_UTILS_MOD
   USE MG_BUFINIT_MOD
   USE DRIVER_BSPMA_MOD
   USE DRIVER_SVAF_MOD
   USE MG_STENCIL_MOD
   USE MG_PROFILING_MOD
   USE MG_CHECKPOINT_MOD

   IMPLICIT NONE

      INTEGER(KIND=MG_INT), INTENT(IN) :: &
         SCALING_IN,          &
         NX_IN,               &
         NY_IN,               &
         NZ_IN,               &
         NVARS_IN,            &
         PERCENT_SUM_IN,      &
         NSPIKES_IN,          &
         NTSTEPS_IN,          &
         STENCIL_IN,          &
         COMM_METHOD_IN,      &
         BC_IN,               &
         ERROR_TOL_IN,        &
         REPORT_DIFFUSION_IN, &
         NPX_IN,              &
         NPY_IN,              &
         NPZ_IN,              &
         REPORT_PERF_IN,      &
         CP_METHOD_IN,        &
         CP_INTERVAL_IN,      &
         RESTART_CP_NUM_IN,   &
         DEBUG_GRID_IN

      CHARACTER*(1024), INTENT(IN) :: &
         CP_FILE_IN,     &
         RESTART_FILE_IN

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER :: &
         IERR,                    &  ! Return status
         I,                       &  ! Counter
         IVAR                        ! Counter (over variables)

      REAL(KIND=MG_REAL), DIMENSION(:,:,:,:), ALLOCATABLE :: &
         GRID

      REAL(KIND=MG_REAL8) ::      &
         TIME_START,              &
         TIME_START_2,            &
         TIME_START_ALL

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      ! Set C input parameters to Fortran-stored parameters
      SCALING          = SCALING_IN
      NX               = NX_IN
      NY               = NY_IN
      NZ               = NZ_IN
      NVARS            = NVARS_IN
      PERCENT_SUM      = PERCENT_SUM_IN
      NSPIKES          = NSPIKES_IN
      NTSTEPS          = NTSTEPS_IN
      STENCIL          = STENCIL_IN
      COMM_METHOD      = COMM_METHOD_IN
      BC               = BC_IN
      ERROR_TOL        = 1.0 / (10.0**ERROR_TOL_IN)

      IF ( REPORT_DIFFUSION_IN == 0 ) THEN
         REPORT_DIFFUSION = NTSTEPS
      ELSE
         REPORT_DIFFUSION = REPORT_DIFFUSION_IN
      END IF

      NPX = NPX_IN
      NPY = NPY_IN
      NPZ = NPZ_IN

      REPORT_PERF = REPORT_PERF_IN

      DEBUG_GRID = DEBUG_GRID_IN

      CP_METHOD      = CP_METHOD_IN
      CP_INTERVAL    = CP_INTERVAL_IN
      CP_FILE        = CP_FILE_IN
      RESTART_CP_NUM = RESTART_CP_NUM_IN
      RESTART_FILE   = RESTART_FILE_IN
      CALL MG_CP_RESTART_CMDLINE ( IERR )

      ! Set up computing environment (e.g. MPI if selected).
      ! Set 3d processor grid, position in processor grid.
      ! Set neighbors, message tag initialization, spikes (for testing).
      CALL MG_INIT ( IERR )

      ! Initialize performance capture
      CALL MG_PERF_INIT ( IERR )

      ! Allocate local domain, including space for all variables, including ghosts:
      ALLOCATE ( GRID( 0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS ), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'GRID_INIT: ALLOCATE ( GRID )', (NX+2)*(NY+2)*(NZ+2)*NVARS )

      CALL MG_GRID_INIT ( GRID, IERR )

      ! Allocate workspace needed by the specified computation.
      CALL MG_BUFINIT ( IERR )
      CALL MG_ASSERT ( IERR, 'MINIGHOST: MG_BUFINIT', IERR )

      CALL MG_PRINT_HEADER ( COMM_METHOD, STENCIL, IERR )

      ! ---------------------------------
      ! Call COMM_METHOD specific DRIVER.
      ! ---------------------------------

      SELECT CASE ( COMM_METHOD )

         CASE ( COMM_METHOD_BSPMA )

            CALL DRIVER_BSPMA ( GRID, IERR )

         CASE ( COMM_METHOD_SVAF )

            CALL DRIVER_SVAF ( GRID, IERR )

      END SELECT ! COMM_METHOD

      !  Report results.

      CALL MG_PERF_REPORT ( COMM_METHOD, STENCIL, IERR )
      CALL MG_ASSERT ( IERR, 'MINIGHOST: PERF_REPORT', COMM_METHOD )

      ! Done. Deallocate memory and terminate.

      IF ( ALLOCATED ( GRID ) ) &
         DEALLOCATE ( GRID )

      IF ( ALLOCATED ( SEND_BUFFER_BACK ) )  &
         DEALLOCATE ( SEND_BUFFER_BACK )
      IF ( ALLOCATED ( RECV_BUFFER_BACK ) )  &
         DEALLOCATE ( RECV_BUFFER_BACK )
      IF ( ALLOCATED ( SEND_BUFFER_FRONT ) ) &
         DEALLOCATE ( SEND_BUFFER_FRONT )
      IF ( ALLOCATED ( RECV_BUFFER_FRONT ) ) &
         DEALLOCATE ( RECV_BUFFER_FRONT )
      IF ( ALLOCATED ( SEND_BUFFER_EAST ) )  &
         DEALLOCATE ( SEND_BUFFER_EAST )
      IF ( ALLOCATED ( RECV_BUFFER_EAST ) )  &
         DEALLOCATE ( RECV_BUFFER_EAST )
      IF ( ALLOCATED ( SEND_BUFFER_WEST ) )  &
         DEALLOCATE ( SEND_BUFFER_WEST )
      IF ( ALLOCATED ( RECV_BUFFER_WEST ) )  &
         DEALLOCATE ( RECV_BUFFER_WEST )
      IF ( ALLOCATED ( SEND_BUFFER_NORTH ) ) &
         DEALLOCATE ( SEND_BUFFER_NORTH )
      IF ( ALLOCATED ( RECV_BUFFER_NORTH ) ) &
         DEALLOCATE ( RECV_BUFFER_NORTH )
      IF ( ALLOCATED ( SEND_BUFFER_SOUTH ) ) &
         DEALLOCATE ( SEND_BUFFER_SOUTH )
      IF ( ALLOCATED ( RECV_BUFFER_SOUTH ) ) &
         DEALLOCATE ( RECV_BUFFER_SOUTH )

      IF ( ALLOCATED ( MSG_REQS ) ) &
         DEALLOCATE ( MSG_REQS )
      IF ( ALLOCATED ( MSG_TAGS ) ) &
         DEALLOCATE ( MSG_TAGS )

      RETURN

   END SUBROUTINE MINI_GHOST
