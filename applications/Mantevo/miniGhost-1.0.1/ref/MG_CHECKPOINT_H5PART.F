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


MODULE MG_CHECKPOINT_H5PART_MOD

   USE MG_CONSTANTS_MOD
   USE MG_OPTIONS_MOD
   USE MG_UTILS_MOD

   USE H5PART_MOD
   USE H5PART_ATTRIB_MOD
   USE H5BLOCK_MOD
   USE H5BLOCK_READWRITE_MOD

   IMPLICIT NONE

   LOGICAL :: &
         CP_INITIALIZED=.FALSE., &
         RESTARTED=.FALSE.,      &
         RESTARTED_WITH_NEW_CP_FILE=.FALSE.

   INTEGER*8 cp_filehandle
   INTEGER*8 restart_filehandle

CONTAINS

   ! Procedures included:
   !

!  =================================================================================

   SUBROUTINE MG_CP_H5PART_CHECKPOINT ( GRID, TSTEP, SPIKE_NUM, GSUM, IERR )

      IMPLICIT NONE

      REAL(KIND=MG_REAL), INTENT(INOUT) ::     &
         GRID(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS)
      REAL(KIND=MG_REAL), INTENT(IN) ::     &
         GSUM

      INTEGER, INTENT(IN) ::  &
         TSTEP, SPIKE_NUM

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      INTEGER*8           :: last_cp_num, cp_num

      INTEGER(KIND=MG_INT) IVAR, restart_spike_num

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _MG_CHECKPT_H5PART

#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'CP_INTERVAL==', CP_INTERVAL, 'TSTEP==', TSTEP, 'CP_INITIALIZED==', CP_INITIALIZED
      END IF
#endif

      IF ( CP_INTERVAL == 0 ) THEN
         ! A checkpoint interval of zero means no checkpointing
         RETURN
      END IF

      IF ( MOD(TSTEP,CP_INTERVAL) /= 0 ) THEN
         RETURN
      END IF

#if defined _DEBUG_CHECKPOINT
      ! this enables level 4 ("debug") messages to be
      ! printed by the H5Part library
      ! (4_8 is the literal for an integer*8 with value 4)
      rc = h5pt_set_verbosity_level (9_8)
#endif

      cp_filehandle     =-1
      restart_filehandle=-1
#if defined _MG_MPI
      cp_filehandle = h5pt_opena_par (CP_FILE, MPI_COMM_WORLD)
#else
      cp_filehandle = h5pt_opena (CP_FILE)
#endif

      cp_num=-1
      last_cp_num=0
      IF ( CP_INITIALIZED .EQV. .FALSE. ) THEN
         ! This is the first checkpoint.
         IF ((RESTARTED .EQV. .FALSE.) .OR. (RESTARTED_WITH_NEW_CP_FILE .EQV. .TRUE.)) THEN
         ! Initialize the checkpoint file.
            cp_num=1
            rc = h5pt_setstep (cp_filehandle, cp_num)
            CALL MG_CP_H5PART_FILE_INIT ( IERR )
         END IF

         CP_INITIALIZED=.TRUE.
      END IF
      IF (cp_num == -1) THEN
         last_cp_num = h5pt_getnsteps(cp_filehandle)
         cp_num=last_cp_num+1
         rc = h5pt_setstep (cp_filehandle, cp_num)
      END IF

#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'last_cp_num==', last_cp_num
         WRITE (*,*) 'cp_num==', cp_num
      END IF
#endif

      ! Before writing the grid vars, write a small header.
      CALL MG_CP_H5PART_WRITE_TSTEP_HEADER ( TSTEP, SPIKE_NUM, cp_num, GSUM, IERR )

      DO IVAR = 1, NVARS
         CALL MG_CP_H5PART_WRITE_TSTEP_VAR ( cp_num, GRID, IVAR, IERR )
      END DO

      rc = h5pt_close (cp_filehandle)

      IF ( (TSTEP == CP_INTERVAL) .AND. (RESTARTED .EQV. .FALSE.) ) THEN
#if defined _MG_MPI
         cp_filehandle = h5pt_openr_par (CP_FILE, MPI_COMM_WORLD)
#else
         cp_filehandle = h5pt_openr (CP_FILE)
#endif
         rc = h5pt_setstep (cp_filehandle, cp_num)
         CALL MG_CP_H5PART_VERIFY ( cp_num, GRID, TSTEP, SPIKE_NUM, GSUM, IERR )
         rc = h5pt_close (cp_filehandle)
      END IF

#endif _MG_CHECKPT_H5PART

      RETURN

   END SUBROUTINE MG_CP_H5PART_CHECKPOINT

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_FILE_INIT ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER ::            &
         I, J, K

      INTEGER*8           :: rc
      CHARACTER(LEN = 50) :: FIELD_NAME
      CHARACTER(LEN = 8)  :: NUMSTR

      INTEGER(KIND=MG_INT), DIMENSION(:), ALLOCATABLE   ::  MY_GRIDS_TO_SUM ! boolean

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _DEBUG_CHECKPOINT
      WRITE (*,*) "initializing checkpoint file"
#endif

      CALL MG_CP_H5PART_WRITE_FILE_FLOAT_ATTR(cp_filehandle, "ERROR_TOL",      ERROR_TOL,        IERR)

      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "REPORT_PERF",      REPORT_PERF,      IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "REPORT_DIFFUSION", REPORT_DIFFUSION, IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "SCALING",          SCALING,          IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "COMM_METHOD",      COMM_METHOD,      IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "STENCIL",          STENCIL,          IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "NSPIKES",          NSPIKES,          IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "NPX",              NPX,              IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "NPY",              NPY,              IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "NPZ",              NPZ,              IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "NX",               NX,               IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "NY",               NY,               IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "NZ",               NZ,               IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "NVARS",            NVARS,            IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "NSTEPS",           NTSTEPS,          IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "DEBUG_GRID",       DEBUG_GRID,       IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "PERCENT_SUM",      PERCENT_SUM,      IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "CP_INTERVAL",      CP_INTERVAL,      IERR)
      CALL MG_CP_H5PART_WRITE_FILE_INT_ATTR(cp_filehandle, "RESTART_CP_NUM",   RESTART_CP_NUM,   IERR)

      rc = h5pt_writefileattrib_string(cp_filehandle,"CP_FILE",      CP_FILE)
      rc = h5pt_writefileattrib_string(cp_filehandle,"RESTART_FILE", RESTART_FILE)

      ALLOCATE ( MY_GRIDS_TO_SUM(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_H5PART_FILE_INIT: ALLOCATE ( MY_GRIDS_TO_SUM )', NVARS )
      MY_GRIDS_TO_SUM = 0.0

      DO I = 1, NVARS
         MY_GRIDS_TO_SUM(I)=MG_CP_H5PART_LOGICAL_TO_INTEGER(GRIDS_TO_SUM(I))
      END DO

      CALL MG_CP_H5PART_WRITE_FILE_INTARRAY_ATTR(cp_filehandle, "GRIDS_TO_SUM", MY_GRIDS_TO_SUM, INT(NVARS,8), IERR)

      DEALLOCATE ( MY_GRIDS_TO_SUM )

      rc = h5bl_define3dlayout(cp_filehandle,        &
                               1_8, 1_8,   &
                               1_8, 1_8, &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NX_START", MY_GLOBAL_NX_START, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NY_START", MY_GLOBAL_NY_START, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NZ_START", MY_GLOBAL_NZ_START, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NX_END",   MY_GLOBAL_NX_END, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NY_END",   MY_GLOBAL_NY_END, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NZ_END",   MY_GLOBAL_NZ_END, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "NUM_NEIGHS",         NUM_NEIGHS, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "NUM_SUM_GRID",       NUM_SUM_GRID, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "NUMPES",             NUMPES, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MYPE",               MYPE, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MYPX",               MYPX, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MYPY",               MYPY, IERR)
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD(cp_filehandle, "MYPZ",               MYPZ, IERR)


#if defined _DEBUG_CHECKPOINT
      DO K = 0, NUMPES
        IF ( MYPE == K ) THEN
          DO I = 1, NVARS
             DO J = 1, NSPIKES
                WRITE (*,301) K, I, J, SPIKES(I,J)
             END DO
          END DO
          DO I = 0, 3
             DO J = 1, NSPIKES
                WRITE (*,302) K, I, J, SPIKE_LOC(I,J)
             END DO
          END DO
        END IF
        CALL MPI_BARRIER( MPI_COMM_WORLD, IERR )
      END DO
 301  FORMAT ( 'RANK(', I3, ')', 'SPIKES(', I2, ',', I2, ') == ', 1PE9.2 )
 302  FORMAT ( 'RANK(', I3, ')', 'SPIKE_LOC(', I2, ',', I2, ') == ', I5 )
#endif

      rc = h5bl_define3dlayout(cp_filehandle,        &
                               1_8, INT(NVARS,8),   &
                               1_8, INT(NSPIKES,8), &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      rc = h5bl_3d_write_scalar_field_r8(cp_filehandle, "SPIKES", SPIKES(1,1))

      rc = h5bl_define3dlayout(cp_filehandle,        &
                               1_8, 4_8,   &
                               1_8, INT(NSPIKES,8), &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      rc = h5bl_3d_write_scalar_field_i4(cp_filehandle, "SPIKE_LOC", SPIKE_LOC(0,1))

      RETURN

   END SUBROUTINE MG_CP_H5PART_FILE_INIT

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_HEADER ( TSTEP, SPIKE_NUM, CP_NUM, GSUM, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         TSTEP,                            &
         SPIKE_NUM
      INTEGER*8, INTENT(IN) ::  &
         CP_NUM                  ! the current checkpoint number (0 based)
      REAL(KIND=MG_REAL), INTENT(IN) ::     &
         GSUM

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER ::  &
         I,       &
         status

      INTEGER*8           :: rc
      CHARACTER(LEN = 14) :: FIELD_NAME
      CHARACTER(LEN = 8)  :: NUMSTR


      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'CP_NUM==', CP_NUM
      END IF
#endif

      CALL MG_CP_H5PART_WRITE_TSTEP_INT_ATTR(cp_filehandle, "SPIKE_NUM", SPIKE_NUM, IERR)
      CALL MG_CP_H5PART_WRITE_TSTEP_INT_ATTR(cp_filehandle, "TSTEP",     TSTEP,     IERR)
!      CALL MG_CP_H5PART_WRITE_TSTEP_FLOAT_ATTR("GSUM",    GSUM,      IERR)
      rc = h5bl_define3dlayout(cp_filehandle,        &
                               1_8, 1_8,   &
                               1_8, 1_8, &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      CALL MG_CP_H5PART_WRITE_3D_SCALAR_FLOAT_FIELD(cp_filehandle, "GSUM",     GSUM, IERR)

      rc = h5bl_define3dlayout(cp_filehandle,        &
                               1_8, INT(NVARS,8),   &
                               1_8, 1_8, &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      rc = h5bl_3d_write_scalar_field_r8(cp_filehandle, "FLUX_OUT",     FLUX_OUT)
      rc = h5bl_3d_write_scalar_field_r8(cp_filehandle, "SOURCE_TOTAL", SOURCE_TOTAL)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_HEADER

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_VAR ( CP_NUM, GRID, IVAR, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      REAL(KIND=MG_REAL), INTENT(INOUT) ::     &
         GRID(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS)
      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         IVAR
      INTEGER*8, INTENT(IN) ::  &
         CP_NUM                  ! the current checkpoint number (0 based)
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER ::            &
         I,                 &
         status

      INTEGER*8           :: rc
      CHARACTER(LEN = 14) :: FIELD_NAME
      CHARACTER(LEN = 8)  :: NUMSTR


      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'CP_NUM==', CP_NUM
      END IF
#endif

      rc = h5bl_define3dlayout(cp_filehandle,                        &
                               INT(MY_GLOBAL_NX_START,8), INT(MY_GLOBAL_NX_END+2,8), &
                               INT(MY_GLOBAL_NY_START,8), INT(MY_GLOBAL_NY_END+2,8), &
                               INT(MY_GLOBAL_NZ_START,8), INT(MY_GLOBAL_NZ_END+2,8))

      WRITE(NUMSTR, '(I4)') IVAR
      FIELD_NAME = 'GRID'//TRIM(ADJUSTL(NUMSTR))
      rc = h5bl_3d_write_scalar_field_r8(cp_filehandle, FIELD_NAME, GRID(0,0,0,IVAR))

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_VAR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_FILE_FLOAT_ATTR ( filehandle, ATTR_NAME, ATTR_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      REAL(KIND=MG_REAL), INTENT(IN) :: &
         ATTR_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      REAL*8              :: REAL_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      REAL_ARRAY(1)=ATTR_VALUE
#if defined _DEBUG_CHECKPOINT
 121  FORMAT ( A20, A50, ' == ', 1PE13.6 )
      WRITE (*,121) 'FILE_FLOAT_ATTR: ', ATTR_NAME, REAL_ARRAY(1)
#endif
      rc = h5pt_writefileattrib_r8(filehandle, ATTR_NAME, REAL_ARRAY, 1_8)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_FILE_FLOAT_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_FILE_INT_ATTR ( filehandle, ATTR_NAME, ATTR_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      INTEGER(KIND=MG_INT), INTENT(IN) ::     &
         ATTR_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      INTEGER*4           :: INT_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      INT_ARRAY(1)=ATTR_VALUE
#if defined _DEBUG_CHECKPOINT
 121  FORMAT ( A20, A50, ' == ', I5 )
      WRITE (*,121) 'FILE_INT_ATTR: ', ATTR_NAME, INT_ARRAY(1)
#endif
      rc = h5pt_writefileattrib_i4(filehandle, ATTR_NAME, INT_ARRAY, 1_8)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_FILE_INT_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_FILE_INTARRAY_ATTR ( filehandle, ATTR_NAME, ATTR_ARRAY, NUM_VALUES, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      INTEGER(KIND=MG_INT), INTENT(IN) ::     &
         ATTR_ARRAY(*)
      INTEGER*8, INTENT(IN)          :: &
         NUM_VALUES
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_writefileattrib_i4(filehandle, ATTR_NAME, ATTR_ARRAY, NUM_VALUES)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_FILE_INTARRAY_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_FLOAT_ATTR ( filehandle, ATTR_NAME, ATTR_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      REAL(KIND=MG_REAL), INTENT(IN) ::     &
         ATTR_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      REAL*8              :: REAL_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      REAL_ARRAY(1)=ATTR_VALUE
#if defined _DEBUG_CHECKPOINT
 121  FORMAT ( A20, A50, ' == ', 1PE13.6 )
      WRITE (*,121) 'TSTEP_FLOAT_ATTR: ', ATTR_NAME, REAL_ARRAY(1)
#endif
      rc = h5pt_writestepattrib_r8(filehandle, ATTR_NAME, REAL_ARRAY, 1_8)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_FLOAT_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_FLOATARRAY_ATTR ( filehandle, ATTR_NAME, ATTR_ARRAY, NUM_VALUES, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      REAL(KIND=MG_REAL), INTENT(IN) ::     &
         ATTR_ARRAY(*)
      INTEGER*8, INTENT(IN)          :: &
         NUM_VALUES
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_writestepattrib_r8(filehandle, ATTR_NAME, ATTR_ARRAY, NUM_VALUES)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_FLOATARRAY_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_INT_ATTR ( filehandle, ATTR_NAME, ATTR_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      INTEGER(KIND=MG_INT), INTENT(IN) ::     &
         ATTR_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      INTEGER*4           :: INT_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      INT_ARRAY(1)=ATTR_VALUE
#if defined _DEBUG_CHECKPOINT
 121  FORMAT ( A20, A50, ' == ', I5 )
      WRITE (*,121) 'TSTEP_INT_ATTR: ', ATTR_NAME, INT_ARRAY(1)
#endif
      rc = h5pt_writestepattrib_i4(filehandle, ATTR_NAME, INT_ARRAY, 1_8)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_INT_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_INTARRAY_ATTR ( filehandle, ATTR_NAME, ATTR_ARRAY, NUM_VALUES, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      INTEGER(KIND=MG_INT), INTENT(IN) ::     &
         ATTR_ARRAY(*)
      INTEGER*8, INTENT(IN)          :: &
         NUM_VALUES
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_writestepattrib_i4(filehandle, ATTR_NAME, ATTR_ARRAY, NUM_VALUES)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_TSTEP_INTARRAY_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_READ_FILE_FLOAT_ATTR ( filehandle, ATTR_NAME, ATTR_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      REAL(KIND=MG_REAL), INTENT(OUT) :: &
         ATTR_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      REAL*8              :: REAL_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_readfileattrib_r8(filehandle, ATTR_NAME, REAL_ARRAY)
      ATTR_VALUE=REAL_ARRAY(1)
#if defined _DEBUG_CHECKPOINT
 121  FORMAT ( A20, A50, ' == ', 1PE13.6 )
      WRITE (*,121) 'FILE_FLOAT_ATTR: ', ATTR_NAME, REAL_ARRAY(1)
#endif

      RETURN

   END SUBROUTINE MG_CP_H5PART_READ_FILE_FLOAT_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_READ_FILE_INT_ATTR ( filehandle, ATTR_NAME, ATTR_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      INTEGER(KIND=MG_INT), INTENT(OUT) ::     &
         ATTR_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      INTEGER*4           :: INT_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_readfileattrib_i4(filehandle, ATTR_NAME, INT_ARRAY)
      ATTR_VALUE=INT_ARRAY(1)
#if defined _DEBUG_CHECKPOINT
 121  FORMAT ( A20, A50, ' == ', I5 )
      WRITE (*,121) 'FILE_INT_ATTR: ', ATTR_NAME, INT_ARRAY(1)
#endif

      RETURN

   END SUBROUTINE MG_CP_H5PART_READ_FILE_INT_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_READ_FILE_INTARRAY_ATTR ( filehandle, ATTR_NAME, ATTR_ARRAY, NUM_VALUES, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      INTEGER(KIND=MG_INT), INTENT(OUT) ::     &
         ATTR_ARRAY(*)
      INTEGER*8, INTENT(IN)          :: &
         NUM_VALUES
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_readfileattrib_i4(filehandle, ATTR_NAME, ATTR_ARRAY)

      RETURN

   END SUBROUTINE MG_CP_H5PART_READ_FILE_INTARRAY_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_READ_TSTEP_FLOAT_ATTR ( filehandle, ATTR_NAME, ATTR_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      REAL(KIND=MG_REAL), INTENT(OUT) ::     &
         ATTR_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      REAL*8              :: REAL_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_readstepattrib_r8(filehandle, ATTR_NAME, REAL_ARRAY)
      ATTR_VALUE=REAL_ARRAY(1)
#if defined _DEBUG_CHECKPOINT
 121  FORMAT ( A20, A50, ' == ', 1PE13.6 )
      WRITE (*,121) 'TSTEP_FLOAT_ATTR: ', ATTR_NAME, REAL_ARRAY(1)
#endif

      RETURN

   END SUBROUTINE MG_CP_H5PART_READ_TSTEP_FLOAT_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_READ_TSTEP_FLOATARRAY_ATTR ( filehandle, ATTR_NAME, ATTR_ARRAY, NUM_VALUES, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      REAL(KIND=MG_REAL), INTENT(OUT) ::     &
         ATTR_ARRAY(*)
      INTEGER*8, INTENT(IN)          :: &
         NUM_VALUES
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_readstepattrib_r8(filehandle, ATTR_NAME, ATTR_ARRAY)

      RETURN

   END SUBROUTINE MG_CP_H5PART_READ_TSTEP_FLOATARRAY_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_READ_TSTEP_INT_ATTR ( filehandle, ATTR_NAME, ATTR_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      INTEGER(KIND=MG_INT), INTENT(OUT) ::     &
         ATTR_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      INTEGER*4           :: INT_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_readstepattrib_i4(filehandle, ATTR_NAME, INT_ARRAY)
      ATTR_VALUE=INT_ARRAY(1)
#if defined _DEBUG_CHECKPOINT
 121  FORMAT ( A20, A50, ' == ', I5 )
      WRITE (*,121) 'TSTEP_INT_ATTR: ', ATTR_NAME, INT_ARRAY(1)
#endif

      RETURN

   END SUBROUTINE MG_CP_H5PART_READ_TSTEP_INT_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_READ_TSTEP_INTARRAY_ATTR ( filehandle, ATTR_NAME, ATTR_ARRAY, NUM_VALUES, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         ATTR_NAME
      INTEGER(KIND=MG_INT), INTENT(OUT) ::     &
         ATTR_ARRAY(*)
      INTEGER*8, INTENT(IN)          :: &
         NUM_VALUES
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5pt_readstepattrib_i4(filehandle, ATTR_NAME, ATTR_ARRAY)

      RETURN

   END SUBROUTINE MG_CP_H5PART_READ_TSTEP_INTARRAY_ATTR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD ( filehandle, FIELD_NAME, FIELD_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         FIELD_NAME
      INTEGER(KIND=MG_INT), INTENT(IN) ::     &
         FIELD_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      INTEGER*4           :: INT_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      INT_ARRAY(1)=FIELD_VALUE
      rc = h5bl_3d_write_scalar_field_i4(filehandle, FIELD_NAME, INT_ARRAY)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_3D_SCALAR_INT_FIELD

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_WRITE_3D_SCALAR_FLOAT_FIELD ( filehandle, FIELD_NAME, FIELD_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         FIELD_NAME
      REAL(KIND=MG_REAL), INTENT(IN) ::     &
         FIELD_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      REAL*8              :: REAL_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      REAL_ARRAY(1)=FIELD_VALUE
      rc = h5bl_3d_write_scalar_field_r8(filehandle, FIELD_NAME, REAL_ARRAY)

      RETURN

   END SUBROUTINE MG_CP_H5PART_WRITE_3D_SCALAR_FLOAT_FIELD

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD ( filehandle, FIELD_NAME, FIELD_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         FIELD_NAME
      INTEGER(KIND=MG_INT), INTENT(OUT) ::     &
         FIELD_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      INTEGER*4           :: INT_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5bl_3d_read_scalar_field_i4(filehandle, FIELD_NAME, INT_ARRAY)
      FIELD_VALUE=INT_ARRAY(1)

      RETURN

   END SUBROUTINE MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_READ_3D_SCALAR_FLOAT_FIELD ( filehandle, FIELD_NAME, FIELD_VALUE, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER*8, INTENT(IN) :: filehandle
      CHARACTER(LEN = *), INTENT(IN) :: &
         FIELD_NAME
      REAL(KIND=MG_REAL), INTENT(OUT) ::     &
         FIELD_VALUE
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc
      REAL*8              :: REAL_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      rc = h5bl_3d_read_scalar_field_r8(filehandle, FIELD_NAME, REAL_ARRAY)
      FIELD_VALUE=REAL_ARRAY(1)

      RETURN

   END SUBROUTINE MG_CP_H5PART_READ_3D_SCALAR_FLOAT_FIELD

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_VERIFY ( CP_NUM, GRID, TSTEP, SPIKE_NUM, GSUM, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      REAL(KIND=MG_REAL), INTENT(INOUT) ::     &
         GRID(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS)
      REAL(KIND=MG_REAL), INTENT(IN) ::     &
         GSUM
      INTEGER*8, INTENT(IN) ::  &
         CP_NUM                  ! the current checkpoint number (0 based)
      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         TSTEP,  &
         SPIKE_NUM

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER(KIND=MG_INT) :: MAX_CP_NUM  ! the current checkpoint number (0 based)

      INTEGER ::  &
         I,       &
         J,       &
         K,       &
         IVAR

      INTEGER*8           :: rc
      CHARACTER(LEN = 14) :: FIELD_NAME
      CHARACTER(LEN = 8)  :: NUMSTR
      INTEGER*4           :: INT_ARRAY(1)
      REAL*8              :: REAL_ARRAY(1)

      REAL(KIND=MG_REAL)   ::  VERIFY_ERROR_TOL

      INTEGER(KIND=MG_INT) ::  VERIFY_REPORT_PERF      ! 0, 1, 2
      INTEGER(KIND=MG_INT) ::  VERIFY_REPORT_DIFFUSION ! boolean
      INTEGER(KIND=MG_INT) ::  VERIFY_SCALING
      INTEGER(KIND=MG_INT) ::  VERIFY_COMM_METHOD
      INTEGER(KIND=MG_INT) ::  VERIFY_STENCIL
      INTEGER(KIND=MG_INT) ::  VERIFY_NSPIKES
      INTEGER(KIND=MG_INT) ::  VERIFY_NPX
      INTEGER(KIND=MG_INT) ::  VERIFY_NPY
      INTEGER(KIND=MG_INT) ::  VERIFY_NPZ
      INTEGER(KIND=MG_INT) ::  VERIFY_NX
      INTEGER(KIND=MG_INT) ::  VERIFY_NY
      INTEGER(KIND=MG_INT) ::  VERIFY_NZ
      INTEGER(KIND=MG_INT) ::  VERIFY_NVARS
      INTEGER(KIND=MG_INT) ::  VERIFY_NTSTEPS
      INTEGER(KIND=MG_INT) ::  VERIFY_PERCENT_SUM
      INTEGER(KIND=MG_INT) ::  VERIFY_DEBUG_GRID
      INTEGER(KIND=MG_INT) ::  VERIFY_CP_INTERVAL
      INTEGER(KIND=MG_INT) ::  VERIFY_RESTART_CP_NUM

      CHARACTER(1024)      ::  VERIFY_CP_FILE
      CHARACTER(1024)      ::  VERIFY_RESTART_FILE

      INTEGER(KIND=MG_INT) ::  VERIFY_MY_GLOBAL_NX_START
      INTEGER(KIND=MG_INT) ::  VERIFY_MY_GLOBAL_NY_START
      INTEGER(KIND=MG_INT) ::  VERIFY_MY_GLOBAL_NZ_START
      INTEGER(KIND=MG_INT) ::  VERIFY_MY_GLOBAL_NX_END
      INTEGER(KIND=MG_INT) ::  VERIFY_MY_GLOBAL_NY_END
      INTEGER(KIND=MG_INT) ::  VERIFY_MY_GLOBAL_NZ_END
      INTEGER(KIND=MG_INT) ::  VERIFY_NUM_NEIGHS
      INTEGER(KIND=MG_INT) ::  VERIFY_NUM_SUM_GRID
      INTEGER(KIND=MG_INT) ::  VERIFY_NUMPES
      INTEGER(KIND=MG_INT) ::  VERIFY_MYPE
      INTEGER(KIND=MG_INT) ::  VERIFY_MYPX
      INTEGER(KIND=MG_INT) ::  VERIFY_MYPY
      INTEGER(KIND=MG_INT) ::  VERIFY_MYPZ

      REAL(KIND=MG_REAL), DIMENSION(:,:), ALLOCATABLE   ::  VERIFY_SPIKES
      INTEGER(KIND=MG_INT), DIMENSION(:,:), ALLOCATABLE ::  VERIFY_SPIKE_LOC

      INTEGER(KIND=MG_INT) ::  VERIFY_SPIKE_NUM
      INTEGER(KIND=MG_INT) ::  VERIFY_TSTEP
      REAL(KIND=MG_REAL)   ::  VERIFY_GSUM

      INTEGER(KIND=MG_INT), DIMENSION(:), ALLOCATABLE   ::  VERIFY_GRIDS_TO_SUM ! boolean
      REAL(KIND=MG_REAL), DIMENSION(:), ALLOCATABLE     ::  VERIFY_FLUX_OUT
      REAL(KIND=MG_REAL), DIMENSION(:), ALLOCATABLE     ::  VERIFY_SOURCE_TOTAL

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1) :: VERIFY_GRID

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'CP_NUM==', CP_NUM
      END IF
#endif

      MAX_CP_NUM = h5pt_getnsteps(cp_filehandle)
      IF (MAX_CP_NUM /= CP_NUM) THEN
         WRITE (*,*) 'MAX_CP_NUM != CP_NUM', MAX_CP_NUM, '!=', CP_NUM
      END IF

      CALL MG_CP_H5PART_READ_FILE_FLOAT_ATTR(cp_filehandle, "ERROR_TOL",        VERIFY_ERROR_TOL, IERR)

      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "REPORT_PERF",      VERIFY_REPORT_PERF, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "REPORT_DIFFUSION", VERIFY_REPORT_DIFFUSION, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "SCALING",          VERIFY_SCALING, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "COMM_METHOD",      VERIFY_COMM_METHOD, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "STENCIL",          VERIFY_STENCIL, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "NSPIKES",          VERIFY_NSPIKES, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "NPX",              VERIFY_NPX, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "NPY",              VERIFY_NPY, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "NPZ",              VERIFY_NPZ, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "NX",               VERIFY_NX, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "NY",               VERIFY_NY, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "NZ",               VERIFY_NZ, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "NVARS",            VERIFY_NVARS, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "NSTEPS",           VERIFY_NTSTEPS, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "DEBUG_GRID",       VERIFY_DEBUG_GRID, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "PERCENT_SUM",      VERIFY_PERCENT_SUM, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "CP_INTERVAL",      VERIFY_CP_INTERVAL, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(cp_filehandle, "RESTART_CP_NUM",   VERIFY_RESTART_CP_NUM, IERR)

      rc = h5pt_readfileattrib_string(cp_filehandle,"CP_FILE",       VERIFY_CP_FILE)
      rc = h5pt_readfileattrib_string(cp_filehandle,"RESTART_FILE",  VERIFY_RESTART_FILE)

      IF (VERIFY_ERROR_TOL /= ERROR_TOL )     THEN
         WRITE (*,*) 'VERIFY_ERROR_TOL != ERROR_TOL'
      END IF
      IF (VERIFY_REPORT_PERF /= REPORT_PERF ) THEN
         WRITE (*,*) 'VERIFY_REPORT_PERF != REPORT_PERF'
      END IF
      IF (VERIFY_REPORT_DIFFUSION /= REPORT_DIFFUSION ) THEN
         WRITE (*,*) 'VERIFY_REPORT_DIFFUSION != REPORT_DIFFUSION'
      END IF
      IF (VERIFY_SCALING /= SCALING )         THEN
         WRITE (*,*) 'VERIFY_SCALING != SCALING', VERIFY_SCALING, ' !=', SCALING
      END IF
      IF (VERIFY_COMM_METHOD /= COMM_METHOD )     THEN
         WRITE (*,*) 'VERIFY_COMM_METHOD != COMM_METHOD', VERIFY_COMM_METHOD, ' !=', COMM_METHOD
      END IF
      IF (VERIFY_STENCIL /= STENCIL )         THEN
         WRITE (*,*) 'VERIFY_STENCIL != STENCIL'
      END IF
      IF (VERIFY_NSPIKES /= NSPIKES )      THEN
         WRITE (*,*) 'VERIFY_NSPIKES != NSPIKES', VERIFY_NSPIKES, ' !=', NSPIKES
      END IF
      IF (VERIFY_NPX /= NPX )             THEN
         WRITE (*,*) 'VERIFY_NPX != NPX'
      END IF
      IF (VERIFY_NPY /= NPY )             THEN
         WRITE (*,*) 'VERIFY_NPY != NPY'
      END IF
      IF (VERIFY_NPZ /= NPZ )             THEN
         WRITE (*,*) 'VERIFY_NPZ != NPZ'
      END IF
      IF (VERIFY_NX /= NX )              THEN
         WRITE (*,*) 'VERIFY_NX != NX'
      END IF
      IF (VERIFY_NY /= NY )              THEN
         WRITE (*,*) 'VERIFY_NY != NY'
      END IF
      IF (VERIFY_NZ /= NZ )              THEN
         WRITE (*,*) 'VERIFY_NZ != NZ'
      END IF
      IF (VERIFY_NVARS /= NVARS )        THEN
         WRITE (*,*) 'VERIFY_NVARS != NVARS'
      END IF
      IF (VERIFY_NTSTEPS /= NTSTEPS )      THEN
         WRITE (*,*) 'VERIFY_NTSTEPS != NTSTEPS'
      END IF
      IF (VERIFY_PERCENT_SUM /= PERCENT_SUM )     THEN
         WRITE (*,*) 'VERIFY_PERCENT_SUM != PERCENT_SUM'
      END IF
      IF (VERIFY_DEBUG_GRID /= DEBUG_GRID )     THEN
         WRITE (*,*) 'VERIFY_DEBUG_GRID != DEBUG_GRID'
      END IF
      IF (VERIFY_CP_INTERVAL /= CP_INTERVAL )     THEN
         WRITE (*,*) 'VERIFY_CP_INTERVAL != CP_INTERVAL'
      END IF
!      IF ( TRIM(VERIFY_CP_FILE) /= TRIM(CP_FILE) )     THEN
!         WRITE (*,121) 'VERIFY_CP_FILE != CP_FILE', TRIM(VERIFY_CP_FILE), TRIM(CP_FILE)
!      END IF
      IF (VERIFY_RESTART_CP_NUM /= RESTART_CP_NUM )     THEN
         WRITE (*,*) 'VERIFY_RESTART_CP_NUM != RESTART_CP_NUM'
      END IF
!      IF ( TRIM(VERIFY_RESTART_FILE) /= TRIM(RESTART_FILE) )     THEN
!         WRITE (*,121) 'VERIFY_RESTART_FILE != RESTART_FILE', TRIM(VERIFY_RESTART_FILE), TRIM(RESTART_FILE)
!      END IF
      IF (VERIFY_NX * VERIFY_NPX /= NX * NPX ) THEN
         WRITE (*,*) 'VERIFY_NX * VERIFY_NPX != NX * NPX'
      END IF
      IF (VERIFY_NY * VERIFY_NPY /= NY * NPY ) THEN
         WRITE (*,*) 'VERIFY_NY * VERIFY_NPY != NY * NPY'
      END IF
      IF (VERIFY_NZ * VERIFY_NPZ /= NZ * NPZ ) THEN
         WRITE (*,*) 'VERIFY_NZ * VERIFY_NPZ != NZ * NPZ'
      END IF

 121  FORMAT ( A50, A20, ' != ', A20 )

      ALLOCATE ( VERIFY_GRIDS_TO_SUM(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_H5PART_FILE_INIT: ALLOCATE ( VERIFY_GRIDS_TO_SUM )', NVARS )
      VERIFY_GRIDS_TO_SUM = 0.0

      rc = h5pt_readfileattrib_i4(cp_filehandle, "GRIDS_TO_SUM", VERIFY_GRIDS_TO_SUM)

      DO I = 1, NVARS
         IF ( VERIFY_GRIDS_TO_SUM(I) /= MG_CP_H5PART_LOGICAL_TO_INTEGER(GRIDS_TO_SUM(I)) ) THEN
            WRITE (*,*) 'VERIFY_GRIDS_TO_SUM(', I, ') != GRIDS_TO_SUM(', MG_CP_H5PART_LOGICAL_TO_INTEGER(GRIDS_TO_SUM(I)), ')'
         END IF
      END DO

      DEALLOCATE ( VERIFY_GRIDS_TO_SUM )

      IF ( cp_num == 1 ) THEN
          rc = h5bl_define3dlayout(cp_filehandle,        &
                                   1_8, 1_8,   &
                                   1_8, 1_8, &
                                   INT(MYPE+1,8), INT(MYPE+1,8))
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NX_START", VERIFY_MY_GLOBAL_NX_START, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NY_START", VERIFY_MY_GLOBAL_NY_START, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NZ_START", VERIFY_MY_GLOBAL_NZ_START, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NX_END",   VERIFY_MY_GLOBAL_NX_END, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NY_END",   VERIFY_MY_GLOBAL_NY_END, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MY_GLOBAL_NZ_END",   VERIFY_MY_GLOBAL_NZ_END, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "NUM_NEIGHS",         VERIFY_NUM_NEIGHS, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "NUM_SUM_GRID",       VERIFY_NUM_SUM_GRID, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "NUMPES",             VERIFY_NUMPES, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MYPE",               VERIFY_MYPE, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MYPX",               VERIFY_MYPX, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MYPY",               VERIFY_MYPY, IERR)
          CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(cp_filehandle, "MYPZ",               VERIFY_MYPZ, IERR)

          IF ( VERIFY_MY_GLOBAL_NX_START /= MY_GLOBAL_NX_START ) THEN
             WRITE (*,*) ' VERIFY_MY_GLOBAL_NX_START /= MY_GLOBAL_NX_START'
          END IF
          IF ( VERIFY_MY_GLOBAL_NY_START /= MY_GLOBAL_NY_START ) THEN
             WRITE (*,*) ' VERIFY_MY_GLOBAL_NY_START /= MY_GLOBAL_NY_START'
          END IF
          IF ( VERIFY_MY_GLOBAL_NZ_START /= MY_GLOBAL_NZ_START ) THEN
             WRITE (*,*) ' VERIFY_MY_GLOBAL_NZ_START /= MY_GLOBAL_NZ_START'
          END IF
          IF ( VERIFY_MY_GLOBAL_NX_END /= MY_GLOBAL_NX_END ) THEN
             WRITE (*,*) ' VERIFY_MY_GLOBAL_NX_END /= MY_GLOBAL_NX_END'
          END IF
          IF ( VERIFY_MY_GLOBAL_NY_END /= MY_GLOBAL_NY_END ) THEN
             WRITE (*,*) ' VERIFY_MY_GLOBAL_NY_END /= MY_GLOBAL_NY_END'
          END IF
          IF ( VERIFY_MY_GLOBAL_NZ_END /= MY_GLOBAL_NZ_END ) THEN
             WRITE (*,*) ' VERIFY_MY_GLOBAL_NZ_END /= MY_GLOBAL_NZ_END'
          END IF
          IF ( VERIFY_NUM_NEIGHS /= NUM_NEIGHS ) THEN
             WRITE (*,*) ' VERIFY_NUM_NEIGHS /= NUM_NEIGHS'
          END IF
          IF ( VERIFY_NUM_SUM_GRID /= NUM_SUM_GRID ) THEN
             WRITE (*,*) ' VERIFY_NUM_SUM_GRID /= NUM_SUM_GRID'
          END IF
          IF ( VERIFY_NUMPES /= NUMPES ) THEN
             WRITE (*,*) ' VERIFY_NUMPES /= NUMPES'
          END IF
          IF ( VERIFY_MYPE /= MYPE ) THEN
             WRITE (*,*) ' VERIFY_MYPE /= MYPE'
          END IF
          IF ( VERIFY_MYPX /= MYPX ) THEN
             WRITE (*,*) ' VERIFY_MYPX /= MYPX'
          END IF
          IF ( VERIFY_MYPY /= MYPY ) THEN
             WRITE (*,*) ' VERIFY_MYPY /= MYPY'
          END IF
          IF ( VERIFY_MYPZ /= MYPZ ) THEN
             WRITE (*,*) ' VERIFY_MYPZ /= MYPZ'
          END IF

          ALLOCATE ( VERIFY_SPIKES(NVARS,NSPIKES), STAT = IERR )
          CALL MG_ASSERT ( IERR, 'MG_CP_H5PART_FILE_INIT: ALLOCATE ( VERIFY_SPIKES )', NVARS*NSPIKES )
          VERIFY_SPIKES = -1.0
          ALLOCATE ( VERIFY_SPIKE_LOC(0:3,NSPIKES), STAT = IERR )
          CALL MG_ASSERT ( IERR, 'MG_CP_H5PART_FILE_INIT: ALLOCATE ( VERIFY_SPIKE_LOC )', 4*NSPIKES )
          VERIFY_SPIKE_LOC = -1

          rc = h5bl_define3dlayout(cp_filehandle,        &
                                   1_8, INT(NVARS,8),   &
                                   1_8, INT(NSPIKES,8), &
                                   INT(MYPE+1,8), INT(MYPE+1,8))
          rc = h5bl_3d_read_scalar_field_r8(cp_filehandle, "SPIKES", VERIFY_SPIKES(1,1))

          rc = h5bl_define3dlayout(cp_filehandle,        &
                                   1_8, 4_8,   &
                                   1_8, INT(NSPIKES,8), &
                                   INT(MYPE+1,8), INT(MYPE+1,8))
          rc = h5bl_3d_read_scalar_field_i4(cp_filehandle, "SPIKE_LOC", VERIFY_SPIKE_LOC(0,1))

          DO I = 1, NVARS
             DO J = 1, NSPIKES
                IF (VERIFY_SPIKES(I,J) /= SPIKES(I,J) ) THEN
                      WRITE (*,*) 'VERIFY_SPIKES(I,J) != SPIKES(I,J)', VERIFY_SPIKES(I,J), '!=', SPIKES(I,J)
                END IF
             END DO
          END DO
          DO I = 0, 3
             DO J = 1, NSPIKES
                IF (VERIFY_SPIKE_LOC(I,J) /= SPIKE_LOC(I,J) ) THEN
                      WRITE (*,*) 'VERIFY_SPIKE_LOC(I,J) != SPIKE_LOC(I,J)', VERIFY_SPIKE_LOC(I,J), '!=', SPIKE_LOC(I,J)
                END IF
             END DO
          END DO

          DEALLOCATE ( VERIFY_SPIKES )
          DEALLOCATE ( VERIFY_SPIKE_LOC )
      END IF

      CALL MG_CP_H5PART_READ_TSTEP_INT_ATTR(cp_filehandle, "SPIKE_NUM", VERIFY_SPIKE_NUM, IERR)
      CALL MG_CP_H5PART_READ_TSTEP_INT_ATTR(cp_filehandle, "TSTEP",     VERIFY_TSTEP, IERR)
      rc = h5bl_define3dlayout(cp_filehandle,        &
                               1_8, 1_8,   &
                               1_8, 1_8, &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      CALL MG_CP_H5PART_READ_3D_SCALAR_FLOAT_FIELD(cp_filehandle, "GSUM",     VERIFY_GSUM, IERR)

      IF ( VERIFY_SPIKE_NUM /= SPIKE_NUM ) THEN
         WRITE (*,*) ' VERIFY_SPIKE_NUM != SPIKE_NUM', VERIFY_SPIKE_NUM, '!=', SPIKE_NUM
      END IF
      IF ( VERIFY_TSTEP /= TSTEP ) THEN
         WRITE (*,*) ' VERIFY_TSTEP != TSTEP', VERIFY_TSTEP, '!=', TSTEP
      END IF
      IF ( VERIFY_GSUM /= GSUM ) THEN
         WRITE (*,*) ' VERIFY_GSUM != GSUM', VERIFY_GSUM, '!=', GSUM
      END IF


      ALLOCATE ( VERIFY_FLUX_OUT(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_H5PART_VERIFY: ALLOCATE ( VERIFY_FLUX_OUT )', NVARS )
      VERIFY_FLUX_OUT = 0.0
      ALLOCATE ( VERIFY_SOURCE_TOTAL(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_H5PART_VERIFY: ALLOCATE ( VERIFY_SOURCE_TOTAL )', NVARS )
      VERIFY_SOURCE_TOTAL = 0.0

      rc = h5bl_define3dlayout(cp_filehandle,        &
                               1_8, INT(NVARS,8),   &
                               1_8, 1_8, &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      rc = h5bl_3d_read_scalar_field_r8(cp_filehandle, "FLUX_OUT",     VERIFY_FLUX_OUT)
      rc = h5bl_3d_read_scalar_field_r8(cp_filehandle, "SOURCE_TOTAL", VERIFY_SOURCE_TOTAL)

      DO I = 1, NVARS
         IF ( VERIFY_FLUX_OUT(I) /= FLUX_OUT(I) ) THEN
            WRITE (*,*) 'RANK', MYPE, 'VERIFY_FLUX_OUT(I) != FLUX_OUT(I)', VERIFY_FLUX_OUT(I), '!=', FLUX_OUT(I)
         END IF
      END DO
      DO I = 1, NVARS
         IF ( VERIFY_SOURCE_TOTAL(I) /= SOURCE_TOTAL(I) ) THEN
            WRITE (*,*) 'RANK', MYPE, 'I', I, 'VERIFY_SOURCE_TOTAL(I) != SOURCE_TOTAL(I)', VERIFY_SOURCE_TOTAL(I), '!=', SOURCE_TOTAL(I)
         END IF
      END DO

      DEALLOCATE ( VERIFY_FLUX_OUT )
      DEALLOCATE ( VERIFY_SOURCE_TOTAL )


      rc = h5bl_define3dlayout(cp_filehandle,                        &
                               INT(MY_GLOBAL_NX_START,8), INT(MY_GLOBAL_NX_END,8)+2, &
                               INT(MY_GLOBAL_NY_START,8), INT(MY_GLOBAL_NY_END,8)+2, &
                               INT(MY_GLOBAL_NZ_START,8), INT(MY_GLOBAL_NZ_END,8)+2)
      IVAR = 1
      WRITE(NUMSTR, '(I4)') IVAR
      FIELD_NAME = 'GRID'//TRIM(ADJUSTL(NUMSTR))
      rc = h5bl_3d_read_scalar_field_r8(cp_filehandle, FIELD_NAME, VERIFY_GRID(0,0,0))

      DO I = 1, NX
         DO J = 1, NY
            DO K = 1, NZ
               IF ( VERIFY_GRID ( I, J, K ) /= GRID ( I, J, K, IVAR ) ) THEN
                  WRITE (*,122) I, J, K, I, J, K, IVAR, VERIFY_GRID (I,J,K), GRID(I,J,K,IVAR)
 122  FORMAT ( 'VERIFY_GRID ( ',I3, ',', I3, ',', I3 ' ) != GRID ( ',I3, ',', I3, ',', I3 ', ', I3, ' )', 1PE9.2, ' != ', 1PE9.2 )
               END IF
            END DO
         END DO
      END DO

      RETURN

   END SUBROUTINE MG_CP_H5PART_VERIFY


!  ===================================================================================

   SUBROUTINE MG_CP_H5PART_RESTART_CMDLINE ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER*8           :: rc

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _MG_CHECKPT_H5PART

      ! if RESTART_CP_NUM <  -1 then don't restart
      ! if RESTART_CP_NUM == -1 then restart from last checkpoint
      ! if RESTART_CP_NUM >=  0 then restart from requested checkpoint
      IF (RESTART_CP_NUM < -1) THEN
         RETURN
      END IF

#if defined _MG_MPI
      restart_filehandle = h5pt_openr_par (RESTART_FILE, MPI_COMM_WORLD)
#else
      restart_filehandle = h5pt_openr (RESTART_FILE)
#endif
!      rc = h5pt_setstep (restart_filehandle, INT(RESTART_CP_NUM,8))

      CALL MG_CP_H5PART_READ_FILE_FLOAT_ATTR(restart_filehandle,   "ERROR_TOL",        ERROR_TOL, IERR)

      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "REPORT_PERF",      REPORT_PERF, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "REPORT_DIFFUSION", REPORT_DIFFUSION, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "SCALING",          SCALING, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "COMM_METHOD",      COMM_METHOD, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "STENCIL",          STENCIL, IERR)
!      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "NSPIKES",          NSPIKES, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "NPX",              NPX, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "NPY",              NPY, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "NPZ",              NPZ, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "NX",               NX, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "NY",               NY, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "NZ",               NZ, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "NVARS",            NVARS, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "NSTEPS",           NTSTEPS, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "DEBUG_GRID",       DEBUG_GRID, IERR)
      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,     "PERCENT_SUM",      PERCENT_SUM, IERR)
!      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,    "CP_INTERVAL",       CP_INTERVAL, IERR)
!      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle,    "RESTART_CP_NUM",    RESTART_CP_NUM, IERR)

!      rc = h5pt_readfileattrib_string(restart_filehandle,"CP_FILE",           CP_FILE)
!      rc = h5pt_readfileattrib_string(restart_filehandle,"RESTART_FILE",      RESTART_FILE)

!      CALL MG_CP_H5PART_CHECK_CMDLINE ( CMDLINE_OPTIONS, IERR )

      rc = h5pt_close (restart_filehandle)

#endif

      RETURN

   END SUBROUTINE MG_CP_H5PART_RESTART_CMDLINE

   SUBROUTINE MG_CP_H5PART_RESTART_GRID ( GRID, RESTART_FIRST_PASS, STARTING_SPIKE, STARTING_TSTEP, GSUM, IERR )

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

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER ::  &
         I,       &
         J,       &
         K

      INTEGER(KIND=MG_INT) ::  &
         max_cp_num,            & ! the last checkpoint written.  the restart can't be greater than this.
         CP_NUM

      INTEGER(KIND=MG_INT) :: IVAR

      INTEGER(KIND=MG_INT), DIMENSION(:), ALLOCATABLE   ::  MY_GRIDS_TO_SUM ! boolean
      CHARACTER(1024)      :: ORIG_CP_FILE
      INTEGER(KIND=MG_INT) :: ORIG_NSPIKES

      INTEGER*8           :: rc
      CHARACTER(LEN = 14) :: FIELD_NAME
      CHARACTER(LEN = 8)  :: NUMSTR
      INTEGER*4           :: INT_ARRAY(1)
      REAL*8              :: REAL_ARRAY(1)

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _MG_CHECKPT_H5PART

      ! if RESTART_CP_NUM <  -1 then don't restart
      ! if RESTART_CP_NUM == -1 then restart from last checkpoint
      ! if RESTART_CP_NUM >=  0 then restart from requested checkpoint
      IF (RESTART_CP_NUM < -1) THEN
         RETURN
      END IF

#if defined _MG_MPI
      restart_filehandle = h5pt_openr_par (RESTART_FILE, MPI_COMM_WORLD)
#else
      restart_filehandle = h5pt_openr (RESTART_FILE)
#endif

      max_cp_num = h5pt_getnsteps(restart_filehandle)

      CP_NUM=RESTART_CP_NUM
      IF ( (CP_NUM == -1) .OR. (CP_NUM > max_cp_num) ) THEN
         CP_NUM = max_cp_num
      END IF

      rc = h5pt_setstep (restart_filehandle, INT(CP_NUM,8))

      rc = h5pt_readfileattrib_string(restart_filehandle,"CP_FILE", ORIG_CP_FILE)
      IF (TRIM(CP_FILE) .NE. TRIM(ORIG_CP_FILE)) THEN
         RESTARTED_WITH_NEW_CP_FILE=.TRUE.
      END IF

      ALLOCATE ( MY_GRIDS_TO_SUM(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_H5PART_RESTART_GRID: ALLOCATE ( MY_GRIDS_TO_SUM )', NVARS )
      MY_GRIDS_TO_SUM = 0.0

      rc = h5pt_readfileattrib_i4(restart_filehandle, "GRIDS_TO_SUM", MY_GRIDS_TO_SUM)
      DO I = 1, NVARS
         GRIDS_TO_SUM(I)=MG_CP_H5PART_INTEGER_TO_LOGICAL(MY_GRIDS_TO_SUM(I))
      END DO

      DEALLOCATE ( MY_GRIDS_TO_SUM )

      rc = h5pt_setstep (restart_filehandle, 1_8)
      rc = h5bl_define3dlayout(restart_filehandle,        &
                               1_8, 1_8,   &
                               1_8, 1_8, &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MY_GLOBAL_NX_START", MY_GLOBAL_NX_START, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MY_GLOBAL_NY_START", MY_GLOBAL_NY_START, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MY_GLOBAL_NZ_START", MY_GLOBAL_NZ_START, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MY_GLOBAL_NX_END",   MY_GLOBAL_NX_END, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MY_GLOBAL_NY_END",   MY_GLOBAL_NY_END, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MY_GLOBAL_NZ_END",   MY_GLOBAL_NZ_END, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "NUM_NEIGHS",         NUM_NEIGHS, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "NUM_SUM_GRID",       NUM_SUM_GRID, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "NUMPES",             NUMPES, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MYPE",               MYPE, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MYPX",               MYPX, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MYPY",               MYPY, IERR)
      CALL MG_CP_H5PART_READ_3D_SCALAR_INT_FIELD(restart_filehandle, "MYPZ",               MYPZ, IERR)


      CALL MG_CP_H5PART_READ_FILE_INT_ATTR(restart_filehandle, "NSPIKES", ORIG_NSPIKES, IERR)

      rc = h5bl_define3dlayout(restart_filehandle,        &
                               1_8, INT(NVARS,8),   &
                               1_8, INT(ORIG_NSPIKES,8), &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      rc = h5bl_3d_read_scalar_field_r8(restart_filehandle, "SPIKES", SPIKES(1,1))

      rc = h5bl_define3dlayout(restart_filehandle,        &
                               1_8, 4_8,   &
                               1_8, INT(ORIG_NSPIKES,8), &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      rc = h5bl_3d_read_scalar_field_i4(restart_filehandle, "SPIKE_LOC", SPIKE_LOC(0,1))

      rc = h5pt_setstep (restart_filehandle, INT(CP_NUM,8))

      CALL MG_CP_H5PART_READ_TSTEP_INT_ATTR(restart_filehandle, "SPIKE_NUM", STARTING_SPIKE, IERR)
      CALL MG_CP_H5PART_READ_TSTEP_INT_ATTR(restart_filehandle, "TSTEP",     STARTING_TSTEP, IERR)
      STARTING_TSTEP = STARTING_TSTEP+1
      rc = h5bl_define3dlayout(restart_filehandle,        &
                               1_8, 1_8,   &
                               1_8, 1_8, &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      CALL MG_CP_H5PART_READ_3D_SCALAR_FLOAT_FIELD(restart_filehandle, "GSUM",     GSUM, IERR)

      rc = h5bl_define3dlayout(restart_filehandle,        &
                               1_8, INT(NVARS,8),   &
                               1_8, 1_8, &
                               INT(MYPE+1,8), INT(MYPE+1,8))
      rc = h5bl_3d_read_scalar_field_r8(restart_filehandle, "FLUX_OUT",     FLUX_OUT)
      rc = h5bl_3d_read_scalar_field_r8(restart_filehandle, "SOURCE_TOTAL", SOURCE_TOTAL)

      rc = h5bl_define3dlayout(restart_filehandle,                        &
                               INT(MY_GLOBAL_NX_START,8), INT(MY_GLOBAL_NX_END,8)+2, &
                               INT(MY_GLOBAL_NY_START,8), INT(MY_GLOBAL_NY_END,8)+2, &
                               INT(MY_GLOBAL_NZ_START,8), INT(MY_GLOBAL_NZ_END,8)+2)
      DO IVAR = 1, NVARS
         WRITE(NUMSTR, '(I4)') IVAR
         FIELD_NAME = 'GRID'//TRIM(ADJUSTL(NUMSTR))
         rc = h5bl_3d_read_scalar_field_r8(restart_filehandle, FIELD_NAME, GRID(0,0,0,IVAR))
      END DO

      rc = h5pt_close (restart_filehandle)

      RESTART_FIRST_PASS=.TRUE.
      RESTARTED=.TRUE.

#endif _MG_CHECKPT_H5PART

      RETURN

   END SUBROUTINE MG_CP_H5PART_RESTART_GRID

!  ===================================================================================

!  ===================================================================================

   INTEGER*4 FUNCTION MG_CP_H5PART_LOGICAL_TO_INTEGER ( L )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      LOGICAL, INTENT(IN) ::  &
         L

      ! ---------------
      ! Local Variables
      ! ---------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IF ( L ) THEN
         MG_CP_H5PART_LOGICAL_TO_INTEGER=1
      ELSE
         MG_CP_H5PART_LOGICAL_TO_INTEGER=0
      END IF

#if defined _DEBUG_CHECKPOINT
      WRITE (*,*) "L==", L, "INT==", MG_CP_H5PART_LOGICAL_TO_INTEGER
#endif

      RETURN

   END FUNCTION MG_CP_H5PART_LOGICAL_TO_INTEGER

!  ===================================================================================

!  ===================================================================================

   LOGICAL FUNCTION MG_CP_H5PART_INTEGER_TO_LOGICAL ( I )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         I

      ! ---------------
      ! Local Variables
      ! ---------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IF ( I == 0 ) THEN
         MG_CP_H5PART_INTEGER_TO_LOGICAL=.FALSE.
      ELSE
         MG_CP_H5PART_INTEGER_TO_LOGICAL=.TRUE.
      END IF

#if defined _DEBUG_CHECKPOINT
      WRITE (*,*) "I==", I, "LOG==", MG_CP_H5PART_INTEGER_TO_LOGICAL
#endif

      RETURN

   END FUNCTION MG_CP_H5PART_INTEGER_TO_LOGICAL

!  ===================================================================================


END MODULE MG_CHECKPOINT_H5PART_MOD
