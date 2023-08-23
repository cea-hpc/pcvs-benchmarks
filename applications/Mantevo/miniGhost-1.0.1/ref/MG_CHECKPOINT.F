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


!#define _DEBUG_CHECKPOINT


MODULE MG_CHECKPOINT_MOD

   USE MG_CONSTANTS_MOD
   USE MG_OPTIONS_MOD
   USE MG_UTILS_MOD
#if defined _MG_CHECKPT_MPIIO
   USE MG_CHECKPOINT_MPIIO_MOD
#endif
#if defined _MG_CHECKPT_H5PART
   USE MG_CHECKPOINT_H5PART_MOD
#endif

   IMPLICIT NONE

CONTAINS

   SUBROUTINE MG_CP_CHECKPOINT ( GRID, TSTEP, SPIKE_NUM, GSUM, IERR )

      ! Current implementation based on MPI IO functionality.
      ! Compile type decision to include.
      ! Called from DRIVER_BSPMA or DRIVER_SVAF.

      IMPLICIT NONE

      REAL(KIND=MG_REAL), INTENT(INOUT) ::     &
         GRID(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS)
      REAL(KIND=MG_REAL), INTENT(IN) ::     &
         GSUM

      INTEGER, INTENT(IN) ::  &
         TSTEP, SPIKE_NUM

      INTEGER, INTENT(OUT) ::  &
         IERR                     ! Return status.

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      SELECT CASE ( CP_METHOD )

         CASE ( CP_METHOD_MPIIO )

#if defined _MG_CHECKPT_MPIIO
            CALL MG_CP_MPIIO_CHECKPOINT ( GRID, TSTEP, SPIKE_NUM, GSUM, IERR )
#endif


         CASE ( CP_METHOD_H5PART )

#if defined _MG_CHECKPT_H5PART
            CALL MG_CP_H5PART_CHECKPOINT ( GRID, TSTEP, SPIKE_NUM, GSUM, IERR )
#endif

      END SELECT ! CP_METHOD

      RETURN

   END SUBROUTINE MG_CP_CHECKPOINT

   SUBROUTINE MG_CP_RESTART_CMDLINE ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      SELECT CASE ( CP_METHOD )

         CASE ( CP_METHOD_MPIIO )

#if defined _MG_CHECKPT_MPIIO
            CALL MG_CP_MPIIO_RESTART_CMDLINE ( IERR )
#endif

         CASE ( CP_METHOD_H5PART )

#if defined _MG_CHECKPT_H5PART
            CALL MG_CP_H5PART_RESTART_CMDLINE ( IERR )
#endif

      END SELECT ! CP_METHOD

      RETURN

   END SUBROUTINE MG_CP_RESTART_CMDLINE

   SUBROUTINE MG_CP_RESTART_GRID ( GRID, RESTART_FIRST_PASS, STARTING_SPIKE, STARTING_TSTEP, GSUM, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      REAL(KIND=MG_REAL), INTENT(INOUT) ::     &
         GRID(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS)
      REAL(KIND=MG_REAL), INTENT(OUT) ::     &
         GSUM
      LOGICAL, INTENT(INOUT) ::  &
         RESTART_FIRST_PASS
      INTEGER(KIND=MG_INT), INTENT(OUT) ::  &
         STARTING_SPIKE, & !
         STARTING_TSTEP    !
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      SELECT CASE ( CP_METHOD )

         CASE ( CP_METHOD_MPIIO )

#if defined _MG_CHECKPT_MPIIO
            CALL MG_CP_MPIIO_RESTART_GRID ( GRID, RESTART_FIRST_PASS, STARTING_SPIKE, STARTING_TSTEP, GSUM, IERR )
#endif

         CASE ( CP_METHOD_H5PART )

#if defined _MG_CHECKPT_H5PART
            CALL MG_CP_H5PART_RESTART_GRID ( GRID, RESTART_FIRST_PASS, STARTING_SPIKE, STARTING_TSTEP, GSUM, IERR )
#endif

      END SELECT ! CP_METHOD

      RETURN

   END SUBROUTINE MG_CP_RESTART_GRID

END MODULE MG_CHECKPOINT_MOD
