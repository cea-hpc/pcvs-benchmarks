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


MODULE MG_CHECKPOINT_MPIIO_MOD

   USE MG_CONSTANTS_MOD
   USE MG_OPTIONS_MOD
   USE MG_UTILS_MOD

   IMPLICIT NONE

#if defined _MG_SERIAL

CONTAINS

   SUBROUTINE MG_CP_MPIIO_CHECKPOINT ( GRID, TSTEP, SPIKE_NUM, GSUM, IERR )

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
         IERR                    ! Return status.

      RETURN

   END SUBROUTINE MG_CP_MPIIO_CHECKPOINT

#elif defined _MG_MPI

   ! Procedures included:
   !
   !   CHECKPOINT

   LOGICAL :: &
         CP_INITIALIZED=.FALSE., &
         RESTARTED=.FALSE.,      &
         RESTARTED_WITH_NEW_CP_FILE=.FALSE.

   INTEGER(KIND=MPI_ADDRESS_KIND) ::  &
      SIZEOFCHAR,       &
      SIZEOFINT,        &
      SIZEOFREAL

   TYPE :: CMDLINE_OPTIONS_TYPE

      REAL(KIND=MG_REAL)   ::  ERROR_TOL

      INTEGER(KIND=MG_INT) ::  REPORT_PERF      ! 0, 1, 2
      INTEGER(KIND=MG_INT) ::  REPORT_DIFFUSION ! boolean
      INTEGER(KIND=MG_INT) ::  SCALING
      INTEGER(KIND=MG_INT) ::  COMM_METHOD
      INTEGER(KIND=MG_INT) ::  STENCIL
      INTEGER(KIND=MG_INT) ::  NSPIKES
      INTEGER(KIND=MG_INT) ::  NPX
      INTEGER(KIND=MG_INT) ::  NPY
      INTEGER(KIND=MG_INT) ::  NPZ
      INTEGER(KIND=MG_INT) ::  NX
      INTEGER(KIND=MG_INT) ::  NY
      INTEGER(KIND=MG_INT) ::  NZ
      INTEGER(KIND=MG_INT) ::  NVARS
      INTEGER(KIND=MG_INT) ::  NTSTEPS
      INTEGER(KIND=MG_INT) ::  PERCENT_SUM
      INTEGER(KIND=MG_INT) ::  DEBUG_GRID
      INTEGER(KIND=MG_INT) ::  CP_INTERVAL
      INTEGER(KIND=MG_INT) ::  RESTART_CP_NUM

      CHARACTER(1024)      ::  CP_FILE
      CHARACTER(1024)      ::  RESTART_FILE

   END TYPE CMDLINE_OPTIONS_TYPE

   TYPE :: PE_COORDS_TYPE
      INTEGER(KIND=MG_INT) ::  MY_GLOBAL_NX_START
      INTEGER(KIND=MG_INT) ::  MY_GLOBAL_NY_START
      INTEGER(KIND=MG_INT) ::  MY_GLOBAL_NZ_START
      INTEGER(KIND=MG_INT) ::  MY_GLOBAL_NX_END
      INTEGER(KIND=MG_INT) ::  MY_GLOBAL_NY_END
      INTEGER(KIND=MG_INT) ::  MY_GLOBAL_NZ_END
      INTEGER(KIND=MG_INT) ::  NUM_NEIGHS
      INTEGER(KIND=MG_INT) ::  NUM_SUM_GRID
      INTEGER(KIND=MG_INT) ::  NUMPES
      INTEGER(KIND=MG_INT) ::  MYPE
      INTEGER(KIND=MG_INT) ::  MYPX
      INTEGER(KIND=MG_INT) ::  MYPY
      INTEGER(KIND=MG_INT) ::  MYPZ
   END TYPE PE_COORDS_TYPE

   TYPE :: SPIKES_TYPE
         REAL(KIND=MG_REAL), DIMENSION(:,:), ALLOCATABLE   ::  SPIKES
         INTEGER(KIND=MG_INT), DIMENSION(:,:), ALLOCATABLE ::  SPIKE_LOC
   END TYPE SPIKES_TYPE

   TYPE :: TSHEADER_TYPE
      INTEGER(KIND=MG_INT) ::  CURRENT_SPIKE
      INTEGER(KIND=MG_INT) ::  TSTEP
      REAL(KIND=MG_REAL)   ::  GSUM
   END TYPE TSHEADER_TYPE

   TYPE :: DISPLACEMENT_TYPE
      INTEGER(KIND=MPI_ADDRESS_KIND) :: CP_NUM
      INTEGER(KIND=MPI_ADDRESS_KIND) :: CMDLINE
      INTEGER(KIND=MPI_ADDRESS_KIND) :: GRIDSTOSUM
      INTEGER(KIND=MPI_ADDRESS_KIND) :: PECOORDS
      INTEGER(KIND=MPI_ADDRESS_KIND) :: SPIKES_READ
      INTEGER(KIND=MPI_ADDRESS_KIND) :: SPIKELOC_READ
      INTEGER(KIND=MPI_ADDRESS_KIND) :: SPIKES_WRITE
      INTEGER(KIND=MPI_ADDRESS_KIND) :: SPIKELOC_WRITE
      INTEGER(KIND=MPI_ADDRESS_KIND) :: CP0_READ
      INTEGER(KIND=MPI_ADDRESS_KIND) :: CP0_WRITE
   END TYPE DISPLACEMENT_TYPE

   INTEGER CP_CMDLINE_TYPE
   INTEGER CP_GRIDSTOSUM_TYPE
   INTEGER CP_PECOORDS_TYPE
   INTEGER CP_PECOORDSARRAY_TYPE
   INTEGER CP_SPIKES_READ_TYPE
   INTEGER CP_GLOBALSPIKES_READ_TYPE
   INTEGER CP_SPIKELOC_READ_TYPE
   INTEGER CP_GLOBALSPIKELOC_READ_TYPE
   INTEGER CP_SPIKES_WRITE_TYPE
   INTEGER CP_GLOBALSPIKES_WRITE_TYPE
   INTEGER CP_SPIKELOC_WRITE_TYPE
   INTEGER CP_GLOBALSPIKELOC_WRITE_TYPE
   INTEGER CP_FLUXOUT_TYPE
   INTEGER CP_FLUXOUTARRAY_TYPE
   INTEGER CP_SOURCETOTAL_TYPE
   INTEGER CP_SOURCETOTALARRAY_TYPE
   INTEGER CP_TSHEADER_TYPE
   INTEGER CP_NOGHOST_TYPE
   INTEGER CP_TSGRID_TYPE

   INTEGER(KIND=MPI_ADDRESS_KIND) CP_CMDLINE_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_GRIDSTOSUM_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_PECOORDS_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_PECOORDSARRAY_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_SPIKES_READ_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_GLOBALSPIKES_READ_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_SPIKELOC_READ_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_GLOBALSPIKELOC_READ_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_SPIKES_WRITE_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_GLOBALSPIKES_WRITE_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_SPIKELOC_WRITE_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_GLOBALSPIKELOC_WRITE_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_FLUXOUT_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_FLUXOUTARRAY_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_SOURCETOTAL_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_SOURCETOTALARRAY_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_TSHEADER_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_NOGHOST_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_TSGRID_EXTENT
   INTEGER(KIND=MPI_ADDRESS_KIND) CP_LB

   TYPE ( DISPLACEMENT_TYPE ) :: DISPLACEMENT

   INTEGER cp_filehandle
   INTEGER restart_filehandle
CONTAINS
!  =================================================================================

   SUBROUTINE MG_CP_MPIIO_CHECKPOINT ( GRID, TSTEP, SPIKE_NUM, GSUM, IERR )

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

      INTEGER(KIND=MG_INT) IVAR, last_cp_num, cp_num, restart_spike_num

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _MG_CHECKPT_MPIIO

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

      CALL MPI_FILE_OPEN(MPI_COMM_WORLD, CP_FILE, MPI_MODE_CREATE+MPI_MODE_RDWR, MPI_INFO_NULL, cp_filehandle, IERR)

      cp_num=-1
      last_cp_num=0
      IF ( CP_INITIALIZED .EQV. .FALSE. ) THEN
         ! This is the first checkpoint.
         ! Initialize the MPI types.
         CALL MG_CP_MPIIO_CREATE_CMDLINE_TYPE ( IERR )
         CALL MG_CP_MPIIO_CREATE_GRID_TYPES ( IERR )
         IF ((RESTARTED .EQV. .FALSE.) .OR. (RESTARTED_WITH_NEW_CP_FILE .EQV. .TRUE.)) THEN
         ! Initialize the checkpoint file.
            CALL MG_CP_MPIIO_FILE_INIT ( IERR )
            cp_num=0
         END IF

         CP_INITIALIZED=.TRUE.
      END IF
      IF (cp_num == -1) THEN
         CALL MPI_FILE_SET_VIEW(cp_filehandle,       &
                                DISPLACEMENT%CP_NUM, &
                                MG_MPI_INT,          &
                                MG_MPI_INT,          &
                                'native',            &
                                MPI_INFO_NULL,       &
                                IERR)
         CALL MPI_FILE_READ_ALL (cp_filehandle, last_cp_num, 1, MG_MPI_INT, MPI_STATUS_IGNORE, IERR)
         cp_num=last_cp_num+1
      END IF

#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'last_cp_num==', last_cp_num
         WRITE (*,*) 'cp_num==', cp_num
      END IF
#endif

      ! Before writing the grid vars, write a small header.
      CALL MG_CP_MPIIO_WRITE_TSTEP_HEADER ( TSTEP, SPIKE_NUM, cp_num, GSUM, IERR )

      DO IVAR = 1, NVARS
         CALL MG_CP_MPIIO_WRITE_TSTEP_VAR ( cp_num, GRID, IVAR, IERR )
      END DO

      ! The checkpoint is complete.  Write the checkpoint number.
      CALL MG_CP_MPIIO_WRITE_CHECKPOINT_NUMBER ( cp_num, IERR )

      IF ( (TSTEP == CP_INTERVAL) .AND. (RESTARTED .EQV. .FALSE.) ) THEN
         CALL MG_CP_MPIIO_VERIFY ( cp_num, GRID, TSTEP, IERR )
      END IF

      CALL MPI_FILE_CLOSE(cp_filehandle, IERR)

#endif _MG_CHECKPT_MPIIO

      RETURN

   END SUBROUTINE MG_CP_MPIIO_CHECKPOINT

#if defined _MG_CHECKPT_MPIIO

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_CREATE_CMDLINE_TYPE ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER(KIND=MPI_ADDRESS_KIND) ::  &
         disp(4)
      INTEGER ::            &
         blocklen(4),       &
         type(4),           &
         gsizes(3),         &
         psizes(3),         &
         lsizes(3),         &
         global_offsets(3), &
         memsizes(3),       &
         offsets(3),        &
         I,                 &
         status


      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      CALL MPI_TYPE_GET_EXTENT( MPI_CHARACTER, CP_LB, SIZEOFCHAR, IERR)
      CALL MPI_TYPE_GET_EXTENT( MG_MPI_INT,    CP_LB, SIZEOFINT,  IERR)
      CALL MPI_TYPE_GET_EXTENT( MG_MPI_REAL,   CP_LB, SIZEOFREAL, IERR)

      !
      ! Setup an MPI derived type for writing cmdline options
      !
      blocklen(1) = 1
      blocklen(2) = 18
      blocklen(3) = 1024*2
      blocklen(4) = 1

      disp(1) = 0
      disp(2) = (SIZEOFREAL*1)
      disp(3) = disp(2) + (SIZEOFINT*18)
      disp(4) = disp(3) + (SIZEOFCHAR*1024*2)

      type(1) = MG_MPI_REAL
      type(2) = MG_MPI_INT
      type(3) = MPI_CHARACTER
      type(4) = MPI_UB

      CALL MPI_TYPE_CREATE_STRUCT( 4, blocklen, disp, type, CP_CMDLINE_TYPE, IERR)
      CALL MPI_TYPE_COMMIT( CP_CMDLINE_TYPE, IERR)
      CALL MPI_TYPE_GET_EXTENT ( CP_CMDLINE_TYPE, CP_LB, CP_CMDLINE_EXTENT, IERR )

      CALL MG_CP_MPIIO_CALCULATE_DISPLACEMENTS ( IERR )

      RETURN

   END SUBROUTINE MG_CP_MPIIO_CREATE_CMDLINE_TYPE

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_CREATE_SPIKES_TYPES ( CMDLINE_OPTIONS, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      TYPE ( CMDLINE_OPTIONS_TYPE ), INTENT(IN) :: &
         CMDLINE_OPTIONS

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER(KIND=MPI_ADDRESS_KIND) ::  &
         disp(4)
      INTEGER ::            &
         blocklen(4),       &
         type(4),           &
         gsizes(3),         &
         psizes(3),         &
         lsizes(3),         &
         global_offsets(3), &
         memsizes(3),       &
         offsets(3),        &
         I,                 &
         status


      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      CALL MPI_TYPE_GET_EXTENT( MPI_CHARACTER, CP_LB, SIZEOFCHAR, IERR)
      CALL MPI_TYPE_GET_EXTENT( MG_MPI_INT,    CP_LB, SIZEOFINT,  IERR)
      CALL MPI_TYPE_GET_EXTENT( MG_MPI_REAL,   CP_LB, SIZEOFREAL, IERR)

      !
      ! Setup an MPI derived type for reading my array of spikes
      !
      gsizes(1) = NVARS
      gsizes(2) = CMDLINE_OPTIONS%NSPIKES

      lsizes(1) = NVARS
      lsizes(2) = CMDLINE_OPTIONS%NSPIKES

      global_offsets(1) = 0
      global_offsets(2) = 0

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    MG_MPI_REAL, CP_SPIKES_READ_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_SPIKES_READ_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_SPIKES_READ_TYPE, CP_SPIKES_READ_EXTENT, IERR )

      !
      ! Setup an MPI derived type for reading the global array of spikes
      !
      gsizes(1) = 1
      gsizes(2) = NUMPES

      lsizes(1) = 1
      lsizes(2) = 1

      global_offsets(1) = 0
      global_offsets(2) = MYPE

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    CP_SPIKES_READ_TYPE, CP_GLOBALSPIKES_READ_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_GLOBALSPIKES_READ_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_GLOBALSPIKES_READ_TYPE, CP_GLOBALSPIKES_READ_EXTENT, IERR )

      !
      ! Setup an MPI derived type for reading my spike locations
      !
      gsizes(1) = 4
      gsizes(2) = CMDLINE_OPTIONS%NSPIKES

      lsizes(1) = 4
      lsizes(2) = CMDLINE_OPTIONS%NSPIKES

      global_offsets(1) = 0
      global_offsets(2) = 0

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    MG_MPI_INT, CP_SPIKELOC_READ_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_SPIKELOC_READ_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_SPIKELOC_READ_TYPE, CP_SPIKELOC_READ_EXTENT, IERR )

      !
      ! Setup an MPI derived type for reading the global array of spike locations
      !
      gsizes(1) = 1
      gsizes(2) = NUMPES

      lsizes(1) = 1
      lsizes(2) = 1

      global_offsets(1) = 0
      global_offsets(2) = MYPE

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    CP_SPIKELOC_READ_TYPE, CP_GLOBALSPIKELOC_READ_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_GLOBALSPIKELOC_READ_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_GLOBALSPIKELOC_READ_TYPE, CP_GLOBALSPIKELOC_READ_EXTENT, IERR )

      !
      ! Setup an MPI derived type for writing my array of spikes
      !
      gsizes(1) = NVARS
      gsizes(2) = NSPIKES

      lsizes(1) = NVARS
      lsizes(2) = NSPIKES

      global_offsets(1) = 0
      global_offsets(2) = 0

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    MG_MPI_REAL, CP_SPIKES_WRITE_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_SPIKES_WRITE_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_SPIKES_WRITE_TYPE, CP_SPIKES_WRITE_EXTENT, IERR )

      !
      ! Setup an MPI derived type for writing the global array of spikes
      !
      gsizes(1) = 1
      gsizes(2) = NUMPES

      lsizes(1) = 1
      lsizes(2) = 1

      global_offsets(1) = 0
      global_offsets(2) = MYPE

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    CP_SPIKES_WRITE_TYPE, CP_GLOBALSPIKES_WRITE_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_GLOBALSPIKES_WRITE_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_GLOBALSPIKES_WRITE_TYPE, CP_GLOBALSPIKES_WRITE_EXTENT, IERR )

      !
      ! Setup an MPI derived type for writing my spike locations
      !
      gsizes(1) = 4
      gsizes(2) = NSPIKES

      lsizes(1) = 4
      lsizes(2) = NSPIKES

      global_offsets(1) = 0
      global_offsets(2) = 0

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    MG_MPI_INT, CP_SPIKELOC_WRITE_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_SPIKELOC_WRITE_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_SPIKELOC_WRITE_TYPE, CP_SPIKELOC_WRITE_EXTENT, IERR )

      !
      ! Setup an MPI derived type for writing the global array of spike locations
      !
      gsizes(1) = 1
      gsizes(2) = NUMPES

      lsizes(1) = 1
      lsizes(2) = 1

      global_offsets(1) = 0
      global_offsets(2) = MYPE

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    CP_SPIKELOC_WRITE_TYPE, CP_GLOBALSPIKELOC_WRITE_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_GLOBALSPIKELOC_WRITE_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_GLOBALSPIKELOC_WRITE_TYPE, CP_GLOBALSPIKELOC_WRITE_EXTENT, IERR )

      CALL MG_CP_MPIIO_CALCULATE_DISPLACEMENTS ( IERR )

      RETURN

   END SUBROUTINE MG_CP_MPIIO_CREATE_SPIKES_TYPES

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_CREATE_GRID_TYPES ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER(KIND=MPI_ADDRESS_KIND) ::  &
         disp(4)
      INTEGER ::            &
         blocklen(4),       &
         type(4),           &
         gsizes(3),         &
         psizes(3),         &
         lsizes(3),         &
         global_offsets(3), &
         memsizes(3),       &
         offsets(3),        &
         I,                 &
         status


      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      CALL MPI_TYPE_GET_EXTENT( MPI_CHARACTER, CP_LB, SIZEOFCHAR, IERR)
      CALL MPI_TYPE_GET_EXTENT( MG_MPI_INT,    CP_LB, SIZEOFINT,  IERR)
      CALL MPI_TYPE_GET_EXTENT( MG_MPI_REAL,   CP_LB, SIZEOFREAL, IERR)

      !
      ! Setup an MPI derived type for writing cmdline options
      !
      blocklen(1) = 1
      blocklen(2) = 18
      blocklen(3) = 1024*2
      blocklen(4) = 1

      disp(1) = 0
      disp(2) = (SIZEOFREAL*1)
      disp(3) = disp(2) + (SIZEOFINT*18)
      disp(4) = disp(3) + (SIZEOFCHAR*1024*2)

      type(1) = MG_MPI_REAL
      type(2) = MG_MPI_INT
      type(3) = MPI_CHARACTER
      type(4) = MPI_UB

      CALL MPI_TYPE_CREATE_STRUCT( 4, blocklen, disp, type, CP_CMDLINE_TYPE, IERR)
      CALL MPI_TYPE_COMMIT( CP_CMDLINE_TYPE, IERR)
      CALL MPI_TYPE_GET_EXTENT ( CP_CMDLINE_TYPE, CP_LB, CP_CMDLINE_EXTENT, IERR )

      !
      ! Setup an MPI derived type for writing static config options
      !
      CALL MPI_TYPE_CONTIGUOUS( NVARS, MG_MPI_INT, CP_GRIDSTOSUM_TYPE, IERR)
      CALL MPI_TYPE_COMMIT( CP_GRIDSTOSUM_TYPE, IERR)
      CALL MPI_TYPE_GET_EXTENT ( CP_GRIDSTOSUM_TYPE, CP_LB, CP_GRIDSTOSUM_EXTENT, IERR )

      CALL MPI_TYPE_CONTIGUOUS( 13, MG_MPI_INT, CP_PECOORDS_TYPE, IERR)
      CALL MPI_TYPE_COMMIT( CP_PECOORDS_TYPE, IERR)
      CALL MPI_TYPE_GET_EXTENT ( CP_PECOORDS_TYPE, CP_LB, CP_PECOORDS_EXTENT, IERR )

      !
      ! Setup an MPI derived type for writing an array of PE coordinates
      !
      gsizes(1) = 1
      gsizes(2) = NUMPES

      lsizes(1) = 1
      lsizes(2) = 1

      global_offsets(1) = 0
      global_offsets(2) = MYPE

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    CP_PECOORDS_TYPE, CP_PECOORDSARRAY_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_PECOORDSARRAY_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_PECOORDSARRAY_TYPE, CP_PECOORDSARRAY_EXTENT, IERR )

      !
      ! Setup an MPI derived type for writing a timestep header
      !
      blocklen(1) = 2
      blocklen(2) = 1
      blocklen(3) = 1

      disp(1) = 0
      disp(2) = (SIZEOFINT*2)
      disp(3) = disp(2)+(SIZEOFREAL)

      type(1) = MG_MPI_INT
      type(2) = MG_MPI_REAL
      type(3) = MPI_UB

      CALL MPI_TYPE_CREATE_STRUCT( 3, blocklen, disp, type, CP_TSHEADER_TYPE, IERR)
      CALL MPI_TYPE_COMMIT( CP_TSHEADER_TYPE, IERR)
      CALL MPI_TYPE_GET_EXTENT ( CP_TSHEADER_TYPE, CP_LB, CP_TSHEADER_EXTENT, IERR )

      CALL MPI_TYPE_CONTIGUOUS( NVARS, MG_MPI_REAL, CP_FLUXOUT_TYPE, IERR)
      CALL MPI_TYPE_COMMIT( CP_FLUXOUT_TYPE, IERR)
      CALL MPI_TYPE_GET_EXTENT ( CP_FLUXOUT_TYPE, CP_LB, CP_FLUXOUT_EXTENT, IERR )

      !
      ! Setup an MPI derived type for writing an array of FLUX_OUT
      !
      gsizes(1) = 1
      gsizes(2) = NUMPES

      lsizes(1) = 1
      lsizes(2) = 1

      global_offsets(1) = 0
      global_offsets(2) = MYPE

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    CP_FLUXOUT_TYPE, CP_FLUXOUTARRAY_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_FLUXOUTARRAY_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_FLUXOUTARRAY_TYPE, CP_FLUXOUTARRAY_EXTENT, IERR )

      CALL MPI_TYPE_CONTIGUOUS( NVARS, MG_MPI_REAL, CP_SOURCETOTAL_TYPE, IERR)
      CALL MPI_TYPE_COMMIT( CP_SOURCETOTAL_TYPE, IERR)
      CALL MPI_TYPE_GET_EXTENT ( CP_SOURCETOTAL_TYPE, CP_LB, CP_SOURCETOTAL_EXTENT, IERR )
      !
      ! Setup an MPI derived type for writing an array of SOURCE_TOTAL
      !
      gsizes(1) = 1
      gsizes(2) = NUMPES

      lsizes(1) = 1
      lsizes(2) = 1

      global_offsets(1) = 0
      global_offsets(2) = MYPE

      CALL MPI_TYPE_CREATE_SUBARRAY(2, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, &
                                    CP_SOURCETOTAL_TYPE, CP_SOURCETOTALARRAY_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_SOURCETOTALARRAY_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_SOURCETOTALARRAY_TYPE, CP_SOURCETOTALARRAY_EXTENT, IERR )


      !
      ! Setup an MPI derived type for writing a grid variable without the ghost cells
      !
      gsizes(1) = NX*NPX
      gsizes(2) = NY*NPY
      gsizes(3) = NZ*NPZ

      psizes(1) = NPX
      psizes(2) = NPY
      psizes(3) = NPZ

      lsizes(1) = NX
      lsizes(2) = NY
      lsizes(3) = NZ

      global_offsets(1) = MYPX*NX
      global_offsets(2) = MYPY*NY
      global_offsets(3) = MYPZ*NZ

      memsizes(1) = lsizes(1)+2
      memsizes(2) = lsizes(2)+2
      memsizes(3) = lsizes(3)+2

      offsets(1) = 1
      offsets(2) = 1
      offsets(3) = 1

      CALL MPI_TYPE_CREATE_SUBARRAY(3, memsizes, lsizes, offsets, MPI_ORDER_FORTRAN, MG_MPI_REAL, CP_NOGHOST_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_NOGHOST_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_NOGHOST_TYPE, CP_NOGHOST_EXTENT, IERR )

      CALL MPI_TYPE_CREATE_SUBARRAY(3, gsizes, lsizes, global_offsets, MPI_ORDER_FORTRAN, MG_MPI_REAL, CP_TSGRID_TYPE, IERR)
      CALL MPI_TYPE_COMMIT(CP_TSGRID_TYPE, IERR)
      CALL MPI_TYPE_EXTENT ( CP_TSGRID_TYPE, CP_TSGRID_EXTENT, IERR )

      CALL MG_CP_MPIIO_CALCULATE_DISPLACEMENTS ( IERR )

      RETURN

   END SUBROUTINE MG_CP_MPIIO_CREATE_GRID_TYPES

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_CALCULATE_DISPLACEMENTS ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      DISPLACEMENT%CP_NUM        =0
      DISPLACEMENT%CMDLINE       =SIZEOFINT
      DISPLACEMENT%GRIDSTOSUM    =SIZEOFINT+       &
                                  CP_CMDLINE_EXTENT
      DISPLACEMENT%PECOORDS      =SIZEOFINT+          &
                                  CP_CMDLINE_EXTENT+  &
                                  CP_GRIDSTOSUM_EXTENT
      DISPLACEMENT%SPIKES_READ   =SIZEOFINT+              &
                                  CP_CMDLINE_EXTENT+      &
                                  CP_GRIDSTOSUM_EXTENT+   &
                                  CP_PECOORDSARRAY_EXTENT
      DISPLACEMENT%SPIKELOC_READ =SIZEOFINT+                  &
                                  CP_CMDLINE_EXTENT+          &
                                  CP_GRIDSTOSUM_EXTENT+       &
                                  CP_PECOORDSARRAY_EXTENT+    &
                                  CP_GLOBALSPIKES_READ_EXTENT
      DISPLACEMENT%SPIKES_WRITE  =SIZEOFINT+              &
                                  CP_CMDLINE_EXTENT+      &
                                  CP_GRIDSTOSUM_EXTENT+   &
                                  CP_PECOORDSARRAY_EXTENT
      DISPLACEMENT%SPIKELOC_WRITE=SIZEOFINT+                   &
                                  CP_CMDLINE_EXTENT+           &
                                  CP_GRIDSTOSUM_EXTENT+        &
                                  CP_PECOORDSARRAY_EXTENT+     &
                                  CP_GLOBALSPIKES_WRITE_EXTENT
      DISPLACEMENT%CP0_READ      =SIZEOFINT+                    &
                                  CP_CMDLINE_EXTENT+            &
                                  CP_GRIDSTOSUM_EXTENT+         &
                                  CP_PECOORDSARRAY_EXTENT+      &
                                  CP_GLOBALSPIKES_READ_EXTENT+  &
                                  CP_GLOBALSPIKELOC_READ_EXTENT
      DISPLACEMENT%CP0_WRITE     =SIZEOFINT+                     &
                                  CP_CMDLINE_EXTENT+             &
                                  CP_GRIDSTOSUM_EXTENT+          &
                                  CP_PECOORDSARRAY_EXTENT+       &
                                  CP_GLOBALSPIKES_WRITE_EXTENT+  &
                                  CP_GLOBALSPIKELOC_WRITE_EXTENT

      RETURN

   END SUBROUTINE MG_CP_MPIIO_CALCULATE_DISPLACEMENTS

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_FREE_CMDLINE_TYPE ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------


      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      CALL MPI_TYPE_FREE ( CP_CMDLINE_TYPE, IERR)

      RETURN

   END SUBROUTINE MG_CP_MPIIO_FREE_CMDLINE_TYPE

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_FREE_GRID_TYPES ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------


      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      CALL MPI_TYPE_FREE ( CP_GRIDSTOSUM_TYPE, IERR)
      CALL MPI_TYPE_FREE ( CP_PECOORDS_TYPE, IERR)
      CALL MPI_TYPE_FREE ( CP_PECOORDSARRAY_TYPE, IERR)

      CALL MPI_TYPE_FREE ( CP_TSHEADER_TYPE, IERR )

      CALL MPI_TYPE_FREE ( CP_FLUXOUT_TYPE, IERR )
      CALL MPI_TYPE_FREE ( CP_FLUXOUTARRAY_TYPE, IERR )
      CALL MPI_TYPE_FREE ( CP_SOURCETOTAL_TYPE, IERR )
      CALL MPI_TYPE_FREE ( CP_SOURCETOTALARRAY_TYPE, IERR )

      CALL MPI_TYPE_FREE ( CP_TSGRID_TYPE, IERR )
      CALL MPI_TYPE_FREE ( CP_NOGHOST_TYPE, IERR )

      RETURN

   END SUBROUTINE MG_CP_MPIIO_FREE_GRID_TYPES

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_FREE_SPIKES_TYPES ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------


      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      CALL MPI_TYPE_FREE ( CP_SPIKES_READ_TYPE, IERR)
      CALL MPI_TYPE_FREE ( CP_GLOBALSPIKES_READ_TYPE, IERR)
      CALL MPI_TYPE_FREE ( CP_SPIKELOC_READ_TYPE, IERR)
      CALL MPI_TYPE_FREE ( CP_GLOBALSPIKELOC_READ_TYPE, IERR)
      CALL MPI_TYPE_FREE ( CP_SPIKES_WRITE_TYPE, IERR)
      CALL MPI_TYPE_FREE ( CP_GLOBALSPIKES_WRITE_TYPE, IERR)
      CALL MPI_TYPE_FREE ( CP_SPIKELOC_WRITE_TYPE, IERR)
      CALL MPI_TYPE_FREE ( CP_GLOBALSPIKELOC_WRITE_TYPE, IERR)

      RETURN

   END SUBROUTINE MG_CP_MPIIO_FREE_SPIKES_TYPES

!  ===================================================================================

!  ===================================================================================

   INTEGER(KIND=MPI_ADDRESS_KIND) FUNCTION MG_CP_MPIIO_HEADER_DISPLACEMENT ( CP_NUM, USE_READ_TYPES )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         CP_NUM

      LOGICAL, INTENT(IN) ::  &
         USE_READ_TYPES

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER(KIND=MG_INT) :: cp_size

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      ! the size of one timestep checkpoint
      cp_size=(CP_TSHEADER_EXTENT+CP_FLUXOUTARRAY_EXTENT+CP_SOURCETOTALARRAY_EXTENT+(CP_TSGRID_EXTENT*NVARS))

      IF ( USE_READ_TYPES .EQV. .TRUE. ) THEN
         MG_CP_MPIIO_HEADER_DISPLACEMENT=DISPLACEMENT%CP0_READ+ &
                                   (CP_NUM*cp_size)
      ELSE
         MG_CP_MPIIO_HEADER_DISPLACEMENT=DISPLACEMENT%CP0_WRITE+ &
                                   (CP_NUM*cp_size)
      END IF

#if defined _DEBUG_CHECKPOINT
      WRITE (*,*) "HDR_DISP==", MG_CP_MPIIO_HEADER_DISPLACEMENT
#endif

      RETURN

   END FUNCTION MG_CP_MPIIO_HEADER_DISPLACEMENT

!  ===================================================================================

!  ===================================================================================

   INTEGER(KIND=MPI_ADDRESS_KIND) FUNCTION MG_CP_MPIIO_GRID_DISPLACEMENT ( CP_NUM, VAR_NUM, USE_READ_TYPES )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         CP_NUM,                           &
         VAR_NUM

      LOGICAL, INTENT(IN) ::  &
         USE_READ_TYPES

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER(KIND=MG_INT) :: cp_size

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      ! the size of one timestep checkpoint
      cp_size=CP_TSHEADER_EXTENT+CP_FLUXOUTARRAY_EXTENT+CP_SOURCETOTALARRAY_EXTENT+(CP_TSGRID_EXTENT*NVARS)

      IF ( USE_READ_TYPES .EQV. .TRUE. ) THEN
         MG_CP_MPIIO_GRID_DISPLACEMENT=DISPLACEMENT%CP0_READ+       &
                                (CP_NUM*cp_size)+             &
                                CP_TSHEADER_EXTENT+           &
                                CP_FLUXOUTARRAY_EXTENT+       &
                                CP_SOURCETOTALARRAY_EXTENT+   &
                                (CP_TSGRID_EXTENT*(VAR_NUM-1))
      ELSE
         MG_CP_MPIIO_GRID_DISPLACEMENT=DISPLACEMENT%CP0_WRITE+      &
                                (CP_NUM*cp_size)+             &
                                CP_TSHEADER_EXTENT+           &
                                CP_FLUXOUTARRAY_EXTENT+       &
                                CP_SOURCETOTALARRAY_EXTENT+   &
                                (CP_TSGRID_EXTENT*(VAR_NUM-1))
      END IF


#if defined _DEBUG_CHECKPOINT
      WRITE (*,*) "GRID_DISP==", MG_CP_MPIIO_GRID_DISPLACEMENT
#endif

      RETURN

   END FUNCTION MG_CP_MPIIO_GRID_DISPLACEMENT

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_FILE_INIT ( IERR )

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

      TYPE ( CMDLINE_OPTIONS_TYPE ) :: CMDLINE_OPTIONS
      TYPE ( PE_COORDS_TYPE )       :: PE_COORDS
      TYPE ( SPIKES_TYPE )          :: MY_SPIKES

      INTEGER(KIND=MG_INT), DIMENSION(:), ALLOCATABLE   ::  MY_GRIDS_TO_SUM ! boolean

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _DEBUG_CHECKPOINT
      WRITE (*,*) "initializing checkpoint file"
#endif

      CMDLINE_OPTIONS%ERROR_TOL       =ERROR_TOL
      CMDLINE_OPTIONS%REPORT_PERF     =REPORT_PERF
      CMDLINE_OPTIONS%REPORT_DIFFUSION=REPORT_DIFFUSION
      CMDLINE_OPTIONS%SCALING         =SCALING
      CMDLINE_OPTIONS%COMM_METHOD     =COMM_METHOD
      CMDLINE_OPTIONS%STENCIL         =STENCIL
      CMDLINE_OPTIONS%NSPIKES         =NSPIKES
      CMDLINE_OPTIONS%NPX             =NPX
      CMDLINE_OPTIONS%NPY             =NPY
      CMDLINE_OPTIONS%NPZ             =NPZ
      CMDLINE_OPTIONS%NX              =NX
      CMDLINE_OPTIONS%NY              =NY
      CMDLINE_OPTIONS%NZ              =NZ
      CMDLINE_OPTIONS%NVARS           =NVARS
      CMDLINE_OPTIONS%NTSTEPS         =NTSTEPS
      CMDLINE_OPTIONS%DEBUG_GRID      =DEBUG_GRID
      CMDLINE_OPTIONS%PERCENT_SUM     =PERCENT_SUM
      CMDLINE_OPTIONS%CP_INTERVAL     =CP_INTERVAL
      CMDLINE_OPTIONS%CP_FILE         =CP_FILE
      CMDLINE_OPTIONS%RESTART_CP_NUM  =RESTART_CP_NUM
      CMDLINE_OPTIONS%RESTART_FILE    =RESTART_FILE

      ALLOCATE ( MY_GRIDS_TO_SUM(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_FILE_INIT: ALLOCATE ( MY_GRIDS_TO_SUM )', NVARS )
      MY_GRIDS_TO_SUM = 0.0

      DO I = 1, NVARS
         MY_GRIDS_TO_SUM(I)=MG_CP_MPIIO_LOGICAL_TO_INTEGER(GRIDS_TO_SUM(I))
      END DO

      PE_COORDS%MY_GLOBAL_NX_START=MY_GLOBAL_NX_START
      PE_COORDS%MY_GLOBAL_NY_START=MY_GLOBAL_NY_START
      PE_COORDS%MY_GLOBAL_NZ_START=MY_GLOBAL_NZ_START
      PE_COORDS%MY_GLOBAL_NX_END  =MY_GLOBAL_NX_END
      PE_COORDS%MY_GLOBAL_NY_END  =MY_GLOBAL_NY_END
      PE_COORDS%MY_GLOBAL_NZ_END  =MY_GLOBAL_NZ_END
      PE_COORDS%NUM_NEIGHS        =NUM_NEIGHS
      PE_COORDS%NUM_SUM_GRID      =NUM_SUM_GRID
      PE_COORDS%NUMPES            =NUMPES
      PE_COORDS%MYPE              =MYPE
      PE_COORDS%MYPX              =MYPX
      PE_COORDS%MYPY              =MYPY
      PE_COORDS%MYPZ              =MYPZ

      ALLOCATE ( MY_SPIKES%SPIKES(NVARS,NSPIKES), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_FILE_INIT: ALLOCATE ( SPIKES )', NVARS*NSPIKES )
      MY_SPIKES%SPIKES = -1.0

      ALLOCATE ( MY_SPIKES%SPIKE_LOC(0:3,NSPIKES), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_FILE_INIT: ALLOCATE ( SPIKE_LOC )', 4*NSPIKES )
      MY_SPIKES%SPIKE_LOC = -1

      DO I = 1, NVARS
         DO J = 1, NSPIKES
            MY_SPIKES%SPIKES(I,J)=SPIKES(I,J)
         END DO
      END DO
      DO I = 1, NSPIKES
         MY_SPIKES%SPIKE_LOC(0:3,I)=SPIKE_LOC(0:3,I)
      END DO

      CALL MPI_FILE_SET_VIEW(cp_filehandle,        &
                             DISPLACEMENT%CMDLINE, &
                             CP_CMDLINE_TYPE,      &
                             CP_CMDLINE_TYPE,      &
                             'native',             &
                             MPI_INFO_NULL,        &
                             IERR)
      IF ( MYPE == ROOTPE ) THEN
         CALL MPI_FILE_WRITE (cp_filehandle, CMDLINE_OPTIONS, CP_CMDLINE_EXTENT, MPI_BYTE, MPI_STATUS_IGNORE, IERR)
      END IF

      CALL MPI_FILE_SET_VIEW(cp_filehandle,           &
                             DISPLACEMENT%GRIDSTOSUM, &
                             CP_GRIDSTOSUM_TYPE,      &
                             CP_GRIDSTOSUM_TYPE,      &
                             'native',                &
                             MPI_INFO_NULL,           &
                             IERR)
      IF ( MYPE == ROOTPE ) THEN
         CALL MPI_FILE_WRITE (cp_filehandle, MY_GRIDS_TO_SUM, CP_GRIDSTOSUM_EXTENT, MPI_BYTE, MPI_STATUS_IGNORE, IERR)
      END IF

      CALL MPI_FILE_SET_VIEW(cp_filehandle,         &
                             DISPLACEMENT%PECOORDS, &
                             CP_PECOORDS_TYPE,      &
                             CP_PECOORDSARRAY_TYPE, &
                             'native',              &
                             MPI_INFO_NULL,         &
                             IERR)
      CALL MPI_FILE_WRITE_ALL ( cp_filehandle, PE_COORDS, 1, CP_PECOORDS_TYPE, MPI_STATUS_IGNORE, IERR)

      CALL MG_CP_MPIIO_CREATE_SPIKES_TYPES ( CMDLINE_OPTIONS, IERR )

      CALL MPI_FILE_SET_VIEW(cp_filehandle,        &
                             DISPLACEMENT%SPIKES_WRITE,  &
                             CP_SPIKES_WRITE_TYPE,       &
                             CP_GLOBALSPIKES_WRITE_TYPE, &
                             'native',             &
                             MPI_INFO_NULL,        &
                             IERR)
      CALL MPI_FILE_WRITE_ALL (cp_filehandle, MY_SPIKES%SPIKES, 1, CP_SPIKES_WRITE_TYPE, MPI_STATUS_IGNORE, IERR)

      CALL MPI_FILE_SET_VIEW(cp_filehandle,          &
                             DISPLACEMENT%SPIKELOC_WRITE,  &
                             CP_SPIKELOC_WRITE_TYPE,       &
                             CP_GLOBALSPIKELOC_WRITE_TYPE, &
                             'native',               &
                             MPI_INFO_NULL,          &
                             IERR)
      CALL MPI_FILE_WRITE_ALL (cp_filehandle, MY_SPIKES%SPIKE_LOC, 1, CP_SPIKELOC_WRITE_TYPE, MPI_STATUS_IGNORE, IERR)

#if defined _DEBUG_CHECKPOINT
      DO K = 0, NUMPES
        IF ( MYPE == K ) THEN
          DO I = 1, NVARS
             DO J = 1, CMDLINE_OPTIONS%NSPIKES
                IF ( SPIKES(I,J) /= MY_SPIKES%SPIKES(I,J) ) THEN
                   WRITE (*,301) K, I, J, SPIKES(I,J), MY_SPIKES%SPIKES(I,J)
                END IF
             END DO
          END DO
          DO I = 0, 3
             DO J = 1, CMDLINE_OPTIONS%NSPIKES
                IF ( SPIKE_LOC(I,J) /= MY_SPIKES%SPIKE_LOC(I,J) ) THEN
                   WRITE (*,302) K, I, J, SPIKE_LOC(I,J), MY_SPIKES%SPIKE_LOC(I,J)
                END IF
             END DO
          END DO
        END IF
        CALL MPI_BARRIER( MPI_COMM_WORLD, IERR )
      END DO
 301  FORMAT ( 'RANK(', I3, ')', 'SPIKES(', I2, ',', I2, ') == ORIG(', 1PE9.2, ') CP(', 1PE9.2, ')' )
 302  FORMAT ( 'RANK(', I3, ')', 'SPIKE_LOC(', I2, ',', I2, ') == ORIG(', I5, ') CP(', I5, ')' )
#endif

      DEALLOCATE ( MY_GRIDS_TO_SUM )
      DEALLOCATE ( MY_SPIKES%SPIKES )
      DEALLOCATE ( MY_SPIKES%SPIKE_LOC )

      CALL MG_CP_MPIIO_FREE_SPIKES_TYPES (  IERR )

      RETURN

   END SUBROUTINE MG_CP_MPIIO_FILE_INIT


!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_WRITE_TSTEP_HEADER ( TSTEP, SPIKE_NUM, CP_NUM, GSUM, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         TSTEP,                            &
         SPIKE_NUM,                        &
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

      TYPE ( TSHEADER_TYPE )        :: TSHEADER

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _DEBUG_CHECKPOINT
!      IF ( MYPE == ROOTPE ) THEN
!         WRITE(*,*) 'CMDLINE_EXTENT     =', CP_CMDLINE_EXTENT
!         WRITE(*,*) 'GRIDSTOSUM_EXTENT  =', CP_GRIDSTOSUM_EXTENT
!         WRITE(*,*) 'FLUXOUT_EXTENT     =', CP_FLUXOUT_EXTENT
!         WRITE(*,*) 'SOURCETOTAL_EXTENT =', CP_SOURCETOTAL_EXTENT
!         WRITE(*,*) 'SPIKES_EXTENT      =', CP_SPIKES_EXTENT
!         WRITE(*,*) 'SPIKELOC_EXTENT    =', CP_SPIKELOC_EXTENT
!         WRITE(*,*) 'LOCALSPIKES_EXTENT =', CP_LOCALSPIKES_EXTENT
!         WRITE(*,*) 'GLOBALSPIKES_EXTENT=', CP_GLOBALSPIKES_EXTENT
!         WRITE(*,*) 'TSHEADER_EXTENT    =', CP_TSHEADER_EXTENT
!      END IF
!
!      IF ( MYPE == ROOTPE ) THEN
!         WRITE (*,*) 'CP_NUM==', CP_NUM
!      END IF
#endif

      TSHEADER%CURRENT_SPIKE=SPIKE_NUM
      TSHEADER%TSTEP        =TSTEP
      TSHEADER%GSUM         =GSUM

      CALL MPI_FILE_SET_VIEW(cp_filehandle,                     &
                             MG_CP_MPIIO_HEADER_DISPLACEMENT(CP_NUM, .FALSE.), &
                             CP_TSHEADER_TYPE,                  &
                             CP_TSHEADER_TYPE,                  &
                             'native',                          &
                             MPI_INFO_NULL,                     &
                             IERR)
      IF ( MYPE == ROOTPE ) THEN
         CALL MPI_FILE_WRITE (cp_filehandle, TSHEADER, 1, CP_TSHEADER_TYPE, MPI_STATUS_IGNORE, IERR)
      END IF

      CALL MPI_FILE_SET_VIEW(cp_filehandle,                     &
                             MG_CP_MPIIO_HEADER_DISPLACEMENT(CP_NUM, .FALSE.)+CP_TSHEADER_EXTENT, &
                             CP_FLUXOUT_TYPE,                   &
                             CP_FLUXOUTARRAY_TYPE,              &
                             'native',                          &
                             MPI_INFO_NULL,                     &
                             IERR)
      CALL MPI_FILE_WRITE_ALL (cp_filehandle, FLUX_OUT, 1, CP_FLUXOUT_TYPE, MPI_STATUS_IGNORE, IERR)

      CALL MPI_FILE_SET_VIEW(cp_filehandle,                     &
                             MG_CP_MPIIO_HEADER_DISPLACEMENT(CP_NUM, .FALSE.)+CP_TSHEADER_EXTENT+CP_FLUXOUTARRAY_EXTENT, &
                             CP_SOURCETOTAL_TYPE,               &
                             CP_SOURCETOTALARRAY_TYPE,          &
                             'native',                          &
                             MPI_INFO_NULL,                     &
                             IERR)
      CALL MPI_FILE_WRITE_ALL (cp_filehandle, SOURCE_TOTAL, 1, CP_SOURCETOTAL_TYPE, MPI_STATUS_IGNORE, IERR)


      RETURN

   END SUBROUTINE MG_CP_MPIIO_WRITE_TSTEP_HEADER

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_WRITE_TSTEP_VAR ( CP_NUM, GRID, IVAR, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      REAL(KIND=MG_REAL), INTENT(INOUT) ::     &
         GRID(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS)
      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         CP_NUM, &
         IVAR
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER ::            &
         I,                 &
         status

      ! ---------------------
      ! Executable Statements
      ! ---------------------

#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'CP_NUM==', CP_NUM
      END IF
#endif

      CALL MPI_FILE_SET_VIEW(cp_filehandle,                         &
                             MG_CP_MPIIO_GRID_DISPLACEMENT(CP_NUM, IVAR, .FALSE.), &
                             MG_MPI_REAL,                           &
                             CP_TSGRID_TYPE,                        &
                             'native',                              &
                             MPI_INFO_NULL,                         &
                             IERR)
      CALL MPI_FILE_WRITE_ALL ( cp_filehandle, GRID(0,0,0,IVAR), 1, CP_NOGHOST_TYPE, MPI_STATUS_IGNORE, IERR)

      RETURN

   END SUBROUTINE MG_CP_MPIIO_WRITE_TSTEP_VAR

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_WRITE_CHECKPOINT_NUMBER ( CP_NUM, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         CP_NUM

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      CALL MPI_FILE_SET_VIEW(cp_filehandle,       &
                             DISPLACEMENT%CP_NUM, &
                             MG_MPI_INT,          &
                             MG_MPI_INT,          &
                             'native',            &
                             MPI_INFO_NULL,       &
                             IERR)
      IF ( MYPE == ROOTPE ) THEN
#if defined _DEBUG_CHECKPOINT
         WRITE (*,*) 'CP_NUM==', CP_NUM
#endif
         CALL MPI_FILE_WRITE (cp_filehandle, CP_NUM, 1, MG_MPI_INT, MPI_STATUS_IGNORE, IERR)
      END IF

      RETURN

   END SUBROUTINE MG_CP_MPIIO_WRITE_CHECKPOINT_NUMBER

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_VERIFY ( CP_NUM, GRID, TSTEP, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      REAL(KIND=MG_REAL), INTENT(INOUT) ::     &
         GRID(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS)

      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         CP_NUM, &
         TSTEP

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

      TYPE ( CMDLINE_OPTIONS_TYPE ) :: CMDLINE_OPTIONS

      INTEGER(KIND=MG_INT), DIMENSION(:), ALLOCATABLE   ::  MY_GRIDS_TO_SUM ! boolean
      REAL(KIND=MG_REAL), DIMENSION(:), ALLOCATABLE     ::  MY_FLUX_OUT
      REAL(KIND=MG_REAL), DIMENSION(:), ALLOCATABLE     ::  MY_SOURCE_TOTAL

      TYPE ( PE_COORDS_TYPE )       :: PE_COORDS
      TYPE ( SPIKES_TYPE )          :: MY_SPIKES
      TYPE ( TSHEADER_TYPE )        :: TSHEADER

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1) :: MYGRID

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'CP_NUM==', CP_NUM
      END IF
#endif

      CALL MPI_FILE_SET_VIEW(cp_filehandle,       &
                             DISPLACEMENT%CP_NUM, &
                             MG_MPI_INT,          &
                             MG_MPI_INT,          &
                             'native',            &
                             MPI_INFO_NULL,       &
                             IERR)
      CALL MPI_FILE_READ_ALL (cp_filehandle, MAX_CP_NUM, 1, MG_MPI_INT, MPI_STATUS_IGNORE, IERR)
      IF (MAX_CP_NUM /= CP_NUM) THEN
         WRITE (*,*) 'MAX_CP_NUM != CP_NUM', MAX_CP_NUM, '!=', CP_NUM
      END IF


      CALL MPI_FILE_SET_VIEW(cp_filehandle,        &
                             DISPLACEMENT%CMDLINE, &
                             CP_CMDLINE_TYPE,      &
                             CP_CMDLINE_TYPE,      &
                             'native',             &
                             MPI_INFO_NULL,        &
                             IERR)
      CALL MPI_FILE_READ_ALL (cp_filehandle, CMDLINE_OPTIONS, CP_CMDLINE_EXTENT, MPI_BYTE, MPI_STATUS_IGNORE, IERR)

      IF (CMDLINE_OPTIONS%ERROR_TOL       /= ERROR_TOL )     THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%ERROR_TOL       != ERROR_TOL'
      END IF
      IF (CMDLINE_OPTIONS%REPORT_PERF /= REPORT_PERF ) THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%REPORT_PERF != REPORT_PERF'
      END IF
      IF (CMDLINE_OPTIONS%REPORT_DIFFUSION /= REPORT_DIFFUSION ) THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%REPORT_DIFFUSION != REPORT_DIFFUSION'
      END IF
      IF (CMDLINE_OPTIONS%SCALING         /= SCALING )         THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%SCALING         != SCALING', CMDLINE_OPTIONS%SCALING, '!=', SCALING
      END IF
      IF (CMDLINE_OPTIONS%COMM_METHOD     /= COMM_METHOD )     THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%COMM_METHOD     != COMM_METHOD', CMDLINE_OPTIONS%COMM_METHOD, '!=', COMM_METHOD
      END IF
      IF (CMDLINE_OPTIONS%STENCIL         /= STENCIL )         THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%STENCIL         != STENCIL'
      END IF
      IF (CMDLINE_OPTIONS%NSPIKES      /= NSPIKES )      THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NSPIKES      != NSPIKES', CMDLINE_OPTIONS%NSPIKES, '!=', NSPIKES
      END IF
      IF (CMDLINE_OPTIONS%NPX             /= NPX )             THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NPX             != NPX'
      END IF
      IF (CMDLINE_OPTIONS%NPY             /= NPY )             THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NPY             != NPY'
      END IF
      IF (CMDLINE_OPTIONS%NPZ             /= NPZ )             THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NPZ             != NPZ'
      END IF
      IF (CMDLINE_OPTIONS%NX              /= NX )              THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NX              != NX'
      END IF
      IF (CMDLINE_OPTIONS%NY              /= NY )              THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NY              != NY'
      END IF
      IF (CMDLINE_OPTIONS%NZ              /= NZ )              THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NZ              != NZ'
      END IF
      IF (CMDLINE_OPTIONS%NVARS        /= NVARS )        THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NVARS        != NVARS'
      END IF
      IF (CMDLINE_OPTIONS%NTSTEPS      /= NTSTEPS )      THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NTSTEPS      != NTSTEPS'
      END IF
      IF (CMDLINE_OPTIONS%PERCENT_SUM     /= PERCENT_SUM )     THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%PERCENT_SUM     != PERCENT_SUM'
      END IF
      IF (CMDLINE_OPTIONS%DEBUG_GRID      /= DEBUG_GRID )     THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%DEBUG_GRID      != DEBUG_GRID'
      END IF
      IF (CMDLINE_OPTIONS%CP_INTERVAL     /= CP_INTERVAL )     THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%CP_INTERVAL     != CP_INTERVAL'
      END IF
      IF (CMDLINE_OPTIONS%CP_FILE         /= CP_FILE )     THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%CP_FILE         != CP_FILE'
      END IF
      IF (CMDLINE_OPTIONS%RESTART_CP_NUM     /= RESTART_CP_NUM )     THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%RESTART_CP_NUM     != RESTART_CP_NUM'
      END IF
      IF (CMDLINE_OPTIONS%RESTART_FILE         /= RESTART_FILE )     THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%RESTART_FILE         != RESTART_FILE'
      END IF
      IF (CMDLINE_OPTIONS%NX * CMDLINE_OPTIONS%NPX /= NX * NPX ) THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NX * CMDLINE_OPTIONS%NPX != NX * NPX'
      END IF
      IF (CMDLINE_OPTIONS%NY * CMDLINE_OPTIONS%NPY /= NY * NPY ) THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NY * CMDLINE_OPTIONS%NPY != NY * NPY'
      END IF
      IF (CMDLINE_OPTIONS%NZ * CMDLINE_OPTIONS%NPZ /= NZ * NPZ ) THEN
         WRITE (*,*) 'CMDLINE_OPTIONS%NZ * CMDLINE_OPTIONS%NPZ != NZ * NPZ'
      END IF


      ALLOCATE ( MY_GRIDS_TO_SUM(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_VERIFY: ALLOCATE ( MY_GRIDS_TO_SUM )', NVARS )
      MY_GRIDS_TO_SUM = 0.0

      CALL MPI_FILE_SET_VIEW(cp_filehandle,           &
                             DISPLACEMENT%GRIDSTOSUM, &
                             CP_GRIDSTOSUM_TYPE,      &
                             CP_GRIDSTOSUM_TYPE,      &
                             'native',                &
                             MPI_INFO_NULL,           &
                             IERR)
      CALL MPI_FILE_READ_ALL (cp_filehandle, MY_GRIDS_TO_SUM, CP_GRIDSTOSUM_EXTENT, MPI_BYTE, MPI_STATUS_IGNORE, IERR)

      DO I = 1, NVARS
         IF ( MG_CP_MPIIO_INTEGER_TO_LOGICAL(MY_GRIDS_TO_SUM(I)) .NEQV. GRIDS_TO_SUM(I) ) THEN
            WRITE (*,*) 'MY_GRIDS_TO_SUM(I)) /= GRIDS_TO_SUM(I)'
         END IF
      END DO

      DEALLOCATE ( MY_GRIDS_TO_SUM )

      CALL MPI_FILE_SET_VIEW(cp_filehandle,         &
                             DISPLACEMENT%PECOORDS, &
                             CP_PECOORDS_TYPE, &
                             CP_PECOORDSARRAY_TYPE, &
                             'native',              &
                             MPI_INFO_NULL,         &
                             IERR)
      CALL MPI_FILE_READ_ALL ( cp_filehandle, PE_COORDS, 1, CP_PECOORDS_TYPE, MPI_STATUS_IGNORE, IERR)

      IF ( PE_COORDS%MY_GLOBAL_NX_START /= MY_GLOBAL_NX_START ) THEN
         WRITE (*,*) ' PE_COORDS%MY_GLOBAL_NX_START /= MY_GLOBAL_NX_START'
      END IF
      IF ( PE_COORDS%MY_GLOBAL_NY_START /= MY_GLOBAL_NY_START ) THEN
         WRITE (*,*) ' PE_COORDS%MY_GLOBAL_NY_START /= MY_GLOBAL_NY_START'
      END IF
      IF ( PE_COORDS%MY_GLOBAL_NZ_START /= MY_GLOBAL_NZ_START ) THEN
         WRITE (*,*) ' PE_COORDS%MY_GLOBAL_NZ_START /= MY_GLOBAL_NZ_START'
      END IF
      IF ( PE_COORDS%MY_GLOBAL_NX_END /= MY_GLOBAL_NX_END ) THEN
         WRITE (*,*) ' PE_COORDS%MY_GLOBAL_NX_END /= MY_GLOBAL_NX_END'
      END IF
      IF ( PE_COORDS%MY_GLOBAL_NY_END /= MY_GLOBAL_NY_END ) THEN
         WRITE (*,*) ' PE_COORDS%MY_GLOBAL_NY_END /= MY_GLOBAL_NY_END'
      END IF
      IF ( PE_COORDS%MY_GLOBAL_NZ_END /= MY_GLOBAL_NZ_END ) THEN
         WRITE (*,*) ' PE_COORDS%MY_GLOBAL_NZ_END /= MY_GLOBAL_NZ_END'
      END IF
      IF ( PE_COORDS%NUM_NEIGHS /= NUM_NEIGHS ) THEN
         WRITE (*,*) ' PE_COORDS%NUM_NEIGHS /= NUM_NEIGHS'
      END IF
      IF ( PE_COORDS%NUM_SUM_GRID /= NUM_SUM_GRID ) THEN
         WRITE (*,*) ' PE_COORDS%NUM_SUM_GRID /= NUM_SUM_GRID'
      END IF
      IF ( PE_COORDS%NUMPES /= NUMPES ) THEN
         WRITE (*,*) ' PE_COORDS%NUMPES /= NUMPES'
      END IF
      IF ( PE_COORDS%MYPE /= MYPE ) THEN
         WRITE (*,*) ' PE_COORDS%MYPE /= MYPE'
      END IF
      IF ( PE_COORDS%MYPX /= MYPX ) THEN
         WRITE (*,*) ' PE_COORDS%MYPX /= MYPX'
      END IF
      IF ( PE_COORDS%MYPY /= MYPY ) THEN
         WRITE (*,*) ' PE_COORDS%MYPY /= MYPY'
      END IF
      IF ( PE_COORDS%MYPZ /= MYPZ ) THEN
         WRITE (*,*) ' PE_COORDS%MYPZ /= MYPZ'
      END IF

      ALLOCATE ( MY_SPIKES%SPIKES(NVARS,NSPIKES), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_FILE_INIT: ALLOCATE ( SPIKES )', NVARS*NSPIKES )
      MY_SPIKES%SPIKES = -1.0
      ALLOCATE ( MY_SPIKES%SPIKE_LOC(0:3,NSPIKES), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_FILE_INIT: ALLOCATE ( SPIKE_LOC )', 4*NSPIKES )
      MY_SPIKES%SPIKE_LOC = -1

      CALL MG_CP_MPIIO_CREATE_SPIKES_TYPES (CMDLINE_OPTIONS, IERR )

      CALL MPI_FILE_SET_VIEW(cp_filehandle,             &
                             DISPLACEMENT%SPIKES_WRITE,       &
                             CP_SPIKES_WRITE_TYPE,            &
                             CP_GLOBALSPIKES_WRITE_TYPE, &
                             'native',                  &
                             MPI_INFO_NULL,             &
                             IERR)
      CALL MPI_FILE_READ_ALL (cp_filehandle, MY_SPIKES%SPIKES, 1, CP_SPIKES_WRITE_TYPE, MPI_STATUS_IGNORE, IERR)

      CALL MPI_FILE_SET_VIEW(cp_filehandle,             &
                             DISPLACEMENT%SPIKELOC_WRITE,       &
                             CP_SPIKELOC_WRITE_TYPE,            &
                             CP_GLOBALSPIKELOC_WRITE_TYPE, &
                             'native',                  &
                             MPI_INFO_NULL,             &
                             IERR)
      CALL MPI_FILE_READ_ALL (cp_filehandle, MY_SPIKES%SPIKE_LOC, 1, CP_SPIKELOC_WRITE_TYPE, MPI_STATUS_IGNORE, IERR)

      CALL MG_CP_MPIIO_FREE_SPIKES_TYPES ( IERR )

      DO I = 1, NVARS
         DO J = 1, NSPIKES
            IF (MY_SPIKES%SPIKES(I,J) /= SPIKES(I,J) ) THEN
                  WRITE (*,*) 'MY_SPIKES%SPIKES(I,J) != SPIKES(I,J)', MY_SPIKES%SPIKES(I,J), '!=', SPIKES(I,J)
            END IF
         END DO
      END DO
      DO I = 0, 3
         DO J = 1, NSPIKES
            IF (MY_SPIKES%SPIKE_LOC(I,J) /= SPIKE_LOC(I,J) ) THEN
                  WRITE (*,*) 'MY_SPIKES%SPIKE_LOC(I,J) != SPIKE_LOC(I,J)', MY_SPIKES%SPIKE_LOC(I,J), '!=', SPIKE_LOC(I,J)
            END IF
         END DO
      END DO

      DEALLOCATE ( MY_SPIKES%SPIKES )
      DEALLOCATE ( MY_SPIKES%SPIKE_LOC )

      ALLOCATE ( MY_FLUX_OUT(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_VERIFY: ALLOCATE ( MY_FLUX_OUT )', NVARS )
      MY_FLUX_OUT = 0.0
      ALLOCATE ( MY_SOURCE_TOTAL(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_VERIFY: ALLOCATE ( MY_SOURCE_TOTAL )', NVARS )
      MY_SOURCE_TOTAL = 0.0

      CALL MPI_FILE_SET_VIEW(cp_filehandle,                     &
                             MG_CP_MPIIO_HEADER_DISPLACEMENT(CP_NUM, .FALSE.), &
                             CP_TSHEADER_TYPE,                  &
                             CP_TSHEADER_TYPE,                  &
                             'native',                          &
                             MPI_INFO_NULL,                     &
                             IERR)
      CALL MPI_FILE_READ_ALL (cp_filehandle, TSHEADER, 1, CP_TSHEADER_TYPE, MPI_STATUS_IGNORE, IERR)

      IF ( TSHEADER%TSTEP /= TSTEP ) THEN
         WRITE (*,*) ' TSHEADER%TSTEP != TSTEP', TSHEADER%TSTEP, '!=', TSTEP
      END IF

      CALL MPI_FILE_SET_VIEW(cp_filehandle,                     &
                             MG_CP_MPIIO_HEADER_DISPLACEMENT(CP_NUM, .FALSE.)+CP_TSHEADER_EXTENT, &
                             CP_FLUXOUT_TYPE, &
                             CP_FLUXOUTARRAY_TYPE,             &
                             'native',                          &
                             MPI_INFO_NULL,                     &
                             IERR)
      CALL MPI_FILE_READ_ALL (cp_filehandle, MY_FLUX_OUT, 1, CP_FLUXOUT_TYPE, MPI_STATUS_IGNORE, IERR)

      CALL MPI_FILE_SET_VIEW(cp_filehandle,                     &
                             MG_CP_MPIIO_HEADER_DISPLACEMENT(CP_NUM, .FALSE.)+CP_TSHEADER_EXTENT+CP_FLUXOUTARRAY_EXTENT, &
                             CP_SOURCETOTAL_TYPE,               &
                             CP_SOURCETOTALARRAY_TYPE,               &
                             'native',                          &
                             MPI_INFO_NULL,                     &
                             IERR)
      CALL MPI_FILE_READ_ALL (cp_filehandle, MY_SOURCE_TOTAL, 1, CP_SOURCETOTAL_TYPE, MPI_STATUS_IGNORE, IERR)

      DO I = 1, NVARS
         IF ( MY_FLUX_OUT(I) /= FLUX_OUT(I) ) THEN
            WRITE (*,*) 'RANK', MYPE, 'MY_FLUX_OUT(I) != FLUX_OUT(I)', MY_FLUX_OUT(I), '!=', FLUX_OUT(I)
         END IF
      END DO
      DO I = 1, NVARS
         IF ( MY_SOURCE_TOTAL(I) /= SOURCE_TOTAL(I) ) THEN
            WRITE (*,*) 'RANK', MYPE, 'I', I, 'MY_SOURCE_TOTAL(I) != SOURCE_TOTAL(I)', MY_SOURCE_TOTAL(I), '!=', SOURCE_TOTAL(I)
         END IF
      END DO

      IVAR = 1
      CALL MPI_FILE_SET_VIEW(cp_filehandle,                      &
                             MG_CP_MPIIO_GRID_DISPLACEMENT(CP_NUM, IVAR, .FALSE.), &
                             MG_MPI_REAL, &
                             CP_TSGRID_TYPE,                     &
                             'native',                           &
                             MPI_INFO_NULL,                      &
                             IERR)
      CALL MPI_FILE_READ_ALL ( cp_filehandle, MYGRID, 1, CP_NOGHOST_TYPE, MPI_STATUS_IGNORE, IERR)

      DO K = 1, NZ
         DO J = 1, NY
            DO I = 1, NX
               IF ( MYGRID ( I, J, K ) /= GRID ( I, J, K, IVAR ) ) THEN
                  WRITE (*,*) 'MYGRID ( I, J, K ) != GRID ( I, J, K, IVAR )', MYGRID (I,J,K), '!=', GRID(I,J,K,IVAR), I,J,K,IVAR
               END IF
            END DO
         END DO
      END DO

      DEALLOCATE ( MY_FLUX_OUT )
      DEALLOCATE ( MY_SOURCE_TOTAL )

      RETURN

   END SUBROUTINE MG_CP_MPIIO_VERIFY


!  ===================================================================================

#endif _MG_CHECKPT_MPIIO

#endif _MG_MPI


#if defined _MG_SERIAL

   SUBROUTINE MG_CP_MPIIO_RESTART_CMDLINE ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      RETURN

   END SUBROUTINE MG_CP_MPIIO_RESTART_CMDLINE

   SUBROUTINE MG_CP_MPIIO_RESTART_GRID ( GRID, RESTART_FIRST_PASS, STARTING_SPIKE, STARTING_TSTEP, GSUM, IERR )

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

      RETURN

   END SUBROUTINE MG_CP_MPIIO_RESTART_GRID

#elif defined _MG_MPI

   SUBROUTINE MG_CP_MPIIO_RESTART_CMDLINE ( IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      TYPE ( CMDLINE_OPTIONS_TYPE ) :: CMDLINE_OPTIONS

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _MG_CHECKPT_MPIIO

      ! if RESTART_CP_NUM <  -1 then don't restart
      ! if RESTART_CP_NUM == -1 then restart from last checkpoint
      ! if RESTART_CP_NUM >=  0 then restart from requested checkpoint
      IF (RESTART_CP_NUM < -1) THEN
         RETURN
      END IF

      ! Initialize the MPI types.
      CALL MG_CP_MPIIO_CREATE_CMDLINE_TYPE ( IERR )

      CALL MPI_FILE_OPEN(MPI_COMM_WORLD, RESTART_FILE, MPI_MODE_RDONLY, MPI_INFO_NULL, restart_filehandle, IERR)

      CALL MPI_FILE_SET_VIEW(restart_filehandle,   &
                             DISPLACEMENT%CMDLINE, &
                             CP_CMDLINE_TYPE,      &
                             CP_CMDLINE_TYPE,      &
                             'native',             &
                             MPI_INFO_NULL,        &
                             IERR)
      CALL MPI_FILE_READ_ALL (restart_filehandle, CMDLINE_OPTIONS, CP_CMDLINE_EXTENT, MPI_BYTE, MPI_STATUS_IGNORE, IERR)

      CALL MG_CP_MPIIO_CHECK_CMDLINE ( CMDLINE_OPTIONS, IERR )

      ERROR_TOL       =CMDLINE_OPTIONS%ERROR_TOL
      REPORT_PERF     =CMDLINE_OPTIONS%REPORT_PERF
      REPORT_DIFFUSION=CMDLINE_OPTIONS%REPORT_DIFFUSION
      SCALING         =CMDLINE_OPTIONS%SCALING
      COMM_METHOD     =CMDLINE_OPTIONS%COMM_METHOD
      STENCIL         =CMDLINE_OPTIONS%STENCIL
!      NSPIKES         =CMDLINE_OPTIONS%NSPIKES
      NPX             =CMDLINE_OPTIONS%NPX
      NPY             =CMDLINE_OPTIONS%NPY
      NPZ             =CMDLINE_OPTIONS%NPZ
      NX              =CMDLINE_OPTIONS%NX
      NY              =CMDLINE_OPTIONS%NY
      NZ              =CMDLINE_OPTIONS%NZ
      NVARS           =CMDLINE_OPTIONS%NVARS
      NTSTEPS         =CMDLINE_OPTIONS%NTSTEPS
      DEBUG_GRID      =CMDLINE_OPTIONS%DEBUG_GRID
      PERCENT_SUM     =CMDLINE_OPTIONS%PERCENT_SUM
!      CP_INTERVAL     =CMDLINE_OPTIONS%CP_INTERVAL
!      CP_FILE         =CMDLINE_OPTIONS%CP_FILE
!      RESTART_CP_NUM  =CMDLINE_OPTIONS%RESTART_CP_NUM
!      RESTART_FILE    =CMDLINE_OPTIONS%RESTART_FILE

      CALL MPI_FILE_CLOSE(restart_filehandle, IERR)

      CALL MG_CP_MPIIO_FREE_CMDLINE_TYPE ( IERR )

#endif

      RETURN

   END SUBROUTINE MG_CP_MPIIO_RESTART_CMDLINE

   SUBROUTINE MG_CP_MPIIO_RESTART_GRID ( GRID, RESTART_FIRST_PASS, STARTING_SPIKE, STARTING_TSTEP, GSUM, IERR )

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

      INTEGER(KIND=MG_INT), DIMENSION(:), ALLOCATABLE   ::  MY_GRIDS_TO_SUM ! boolean

      TYPE ( CMDLINE_OPTIONS_TYPE ) :: CMDLINE_OPTIONS
      TYPE ( PE_COORDS_TYPE )       :: PE_COORDS
      TYPE ( TSHEADER_TYPE )        :: TSHEADER


      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _MG_CHECKPT_MPIIO

      ! if RESTART_CP_NUM <  -1 then don't restart
      ! if RESTART_CP_NUM == -1 then restart from last checkpoint
      ! if RESTART_CP_NUM >=  0 then restart from requested checkpoint
      IF (RESTART_CP_NUM < -1) THEN
         RETURN
      END IF


      ! Initialize the MPI types.
      CALL MG_CP_MPIIO_CREATE_CMDLINE_TYPE ( IERR )
      CALL MG_CP_MPIIO_CREATE_GRID_TYPES ( IERR )

      CALL MPI_FILE_OPEN(MPI_COMM_WORLD, RESTART_FILE, MPI_MODE_RDONLY, MPI_INFO_NULL, restart_filehandle, IERR)

      CALL MPI_FILE_SET_VIEW(restart_filehandle,  &
                             DISPLACEMENT%CP_NUM, &
                             MG_MPI_INT,          &
                             MG_MPI_INT,          &
                             'native',            &
                             MPI_INFO_NULL,       &
                             IERR)
      CALL MPI_FILE_READ_ALL (restart_filehandle, max_cp_num, 1, MG_MPI_INT, MPI_STATUS_IGNORE, IERR)

      CP_NUM=RESTART_CP_NUM
      IF ( (CP_NUM == -1) .OR. (CP_NUM > max_cp_num) ) THEN
         CP_NUM = max_cp_num
      END IF

      CALL MPI_FILE_SET_VIEW(restart_filehandle,   &
                             DISPLACEMENT%CMDLINE, &
                             CP_CMDLINE_TYPE,      &
                             CP_CMDLINE_TYPE,      &
                             'native',             &
                             MPI_INFO_NULL,        &
                             IERR)
      CALL MPI_FILE_READ_ALL (restart_filehandle, CMDLINE_OPTIONS, CP_CMDLINE_EXTENT, MPI_BYTE, MPI_STATUS_IGNORE, IERR)

#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'CP_FILE==', TRIM(CP_FILE), 'CMDLINE_OPTIONS%CP_FILE==', TRIM(CMDLINE_OPTIONS%CP_FILE)
      END IF
#endif
      IF (TRIM(CP_FILE) .NE. TRIM(CMDLINE_OPTIONS%CP_FILE)) THEN
         RESTARTED_WITH_NEW_CP_FILE=.TRUE.
      END IF

      ALLOCATE ( MY_GRIDS_TO_SUM(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_VERIFY: ALLOCATE ( MY_GRIDS_TO_SUM )', NVARS )
      MY_GRIDS_TO_SUM = 0.0

      CALL MPI_FILE_SET_VIEW(restart_filehandle,      &
                             DISPLACEMENT%GRIDSTOSUM, &
                             CP_GRIDSTOSUM_TYPE,      &
                             CP_GRIDSTOSUM_TYPE,      &
                             'native',                &
                             MPI_INFO_NULL,           &
                             IERR)
      CALL MPI_FILE_READ_ALL (restart_filehandle, MY_GRIDS_TO_SUM, CP_GRIDSTOSUM_EXTENT, MPI_BYTE, MPI_STATUS_IGNORE, IERR)

      DO I = 1, NVARS
         GRIDS_TO_SUM(I)=MG_CP_MPIIO_INTEGER_TO_LOGICAL(MY_GRIDS_TO_SUM(I))
      END DO

      CALL MPI_FILE_SET_VIEW(restart_filehandle,    &
                             DISPLACEMENT%PECOORDS, &
                             CP_PECOORDS_TYPE, &
                             CP_PECOORDSARRAY_TYPE, &
                             'native',              &
                             MPI_INFO_NULL,         &
                             IERR)
      CALL MPI_FILE_READ_ALL ( restart_filehandle, PE_COORDS, 1, CP_PECOORDS_TYPE, MPI_STATUS_IGNORE, IERR)

      MY_GLOBAL_NX_START=PE_COORDS%MY_GLOBAL_NX_START
      MY_GLOBAL_NY_START=PE_COORDS%MY_GLOBAL_NY_START
      MY_GLOBAL_NZ_START=PE_COORDS%MY_GLOBAL_NZ_START
      MY_GLOBAL_NX_END  =PE_COORDS%MY_GLOBAL_NX_END
      MY_GLOBAL_NY_END  =PE_COORDS%MY_GLOBAL_NY_END
      MY_GLOBAL_NZ_END  =PE_COORDS%MY_GLOBAL_NZ_END
      NUM_NEIGHS        =PE_COORDS%NUM_NEIGHS
      NUM_SUM_GRID      =PE_COORDS%NUM_SUM_GRID
      NUMPES            =PE_COORDS%NUMPES
      MYPE              =PE_COORDS%MYPE
      MYPX              =PE_COORDS%MYPX
      MYPY              =PE_COORDS%MYPY
      MYPZ              =PE_COORDS%MYPZ


      CALL MG_CP_MPIIO_CREATE_SPIKES_TYPES ( CMDLINE_OPTIONS, IERR )

      CALL MG_CP_MPIIO_READ_SPIKES ( CMDLINE_OPTIONS, IERR )

      CALL MG_CP_MPIIO_FREE_SPIKES_TYPES ( IERR )

      CALL MPI_FILE_SET_VIEW(restart_filehandle,                &
                             MG_CP_MPIIO_HEADER_DISPLACEMENT(CP_NUM, .TRUE.), &
                             CP_TSHEADER_TYPE,                  &
                             CP_TSHEADER_TYPE,                  &
                             'native',                          &
                             MPI_INFO_NULL,                     &
                             IERR)
      CALL MPI_FILE_READ_ALL (restart_filehandle, TSHEADER, 1, CP_TSHEADER_TYPE, MPI_STATUS_IGNORE, IERR)

      STARTING_SPIKE=TSHEADER%CURRENT_SPIKE
      STARTING_TSTEP=TSHEADER%TSTEP+1
      GSUM          =TSHEADER%GSUM
#if defined _DEBUG_CHECKPOINT
      IF ( MYPE == ROOTPE ) THEN
         WRITE (*,*) 'tsheader%tstep==', TSHEADER%TSTEP, 'starting_tstep==', STARTING_TSTEP, 'starting_spike==', STARTING_SPIKE, 'tsheader%gsum==', GSUM
      END IF
#endif
      CALL MPI_FILE_SET_VIEW(restart_filehandle,                     &
                             MG_CP_MPIIO_HEADER_DISPLACEMENT(CP_NUM, .TRUE.)+CP_TSHEADER_EXTENT, &
                             CP_FLUXOUT_TYPE, &
                             CP_FLUXOUTARRAY_TYPE,             &
                             'native',                          &
                             MPI_INFO_NULL,                     &
                             IERR)
      CALL MPI_FILE_READ_ALL (restart_filehandle, FLUX_OUT, 1, CP_FLUXOUT_TYPE, MPI_STATUS_IGNORE, IERR)

      CALL MPI_FILE_SET_VIEW(restart_filehandle,                     &
                             MG_CP_MPIIO_HEADER_DISPLACEMENT(CP_NUM, .TRUE.)+CP_TSHEADER_EXTENT+CP_FLUXOUTARRAY_EXTENT, &
                             CP_SOURCETOTAL_TYPE,               &
                             CP_SOURCETOTALARRAY_TYPE,          &
                             'native',                          &
                             MPI_INFO_NULL,                     &
                             IERR)
      CALL MPI_FILE_READ_ALL (restart_filehandle, SOURCE_TOTAL, 1, CP_SOURCETOTAL_TYPE, MPI_STATUS_IGNORE, IERR)


      DO I = 1, NVARS
         CALL MG_CP_MPIIO_READ_TSTEP_VAR ( CP_NUM, GRID, I, IERR )
      END DO

      CALL MPI_FILE_CLOSE(restart_filehandle, IERR)

      CALL MG_CP_MPIIO_FREE_CMDLINE_TYPE ( IERR )
      CALL MG_CP_MPIIO_FREE_GRID_TYPES ( IERR )

      RESTART_FIRST_PASS=.TRUE.
      RESTARTED=.TRUE.

#endif _MG_CHECKPT_MPIIO

      RETURN

   END SUBROUTINE MG_CP_MPIIO_RESTART_GRID

!  ===================================================================================

#if defined _MG_CHECKPT_MPIIO

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_CHECK_CMDLINE ( CMDLINE_OPTIONS, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      TYPE ( CMDLINE_OPTIONS_TYPE ), INTENT(IN) :: &
         CMDLINE_OPTIONS

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER :: MYPE

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      CALL MPI_COMM_RANK ( MPI_COMM_WORLD, MYPE, IERR )

      IF ( MYPE == ROOTPE ) THEN
          IF (CMDLINE_OPTIONS%ERROR_TOL       /= ERROR_TOL )     THEN
             WRITE (*,102) 'Error Tolerance', ERROR_TOL, CMDLINE_OPTIONS%ERROR_TOL
          END IF
          IF (CMDLINE_OPTIONS%REPORT_PERF /= REPORT_PERF ) THEN
             WRITE (*,101) 'REPORT_PERF', REPORT_PERF, CMDLINE_OPTIONS%REPORT_PERF
          END IF
          IF (CMDLINE_OPTIONS%REPORT_DIFFUSION /= REPORT_DIFFUSION ) THEN
             WRITE (*,101) 'Error Report', REPORT_DIFFUSION, CMDLINE_OPTIONS%REPORT_DIFFUSION
          END IF
          IF (CMDLINE_OPTIONS%SCALING         /= SCALING )         THEN
             WRITE (*,101) 'Scaling', SCALING, CMDLINE_OPTIONS%SCALING
          END IF
          IF (CMDLINE_OPTIONS%COMM_METHOD     /= COMM_METHOD )     THEN
             WRITE (*,101) 'Communication Method', COMM_METHOD, CMDLINE_OPTIONS%COMM_METHOD
          END IF
          IF (CMDLINE_OPTIONS%STENCIL         /= STENCIL )         THEN
             WRITE (*,101) 'Stencil', STENCIL, CMDLINE_OPTIONS%STENCIL
          END IF
          IF (CMDLINE_OPTIONS%NPX             /= NPX )             THEN
             WRITE (*,101) 'Task Grid X', NPX, CMDLINE_OPTIONS%NPX
          END IF
          IF (CMDLINE_OPTIONS%NPY             /= NPY )             THEN
             WRITE (*,101) 'Task Grid Y', NPY, CMDLINE_OPTIONS%NPY
          END IF
          IF (CMDLINE_OPTIONS%NPZ             /= NPZ )             THEN
             WRITE (*,101) 'Task Grid Z', NPZ, CMDLINE_OPTIONS%NPZ
          END IF
          IF (CMDLINE_OPTIONS%NX             /= NX )             THEN
             WRITE (*,101) 'Local Grid X Dimension', NX, CMDLINE_OPTIONS%NX
          END IF
          IF (CMDLINE_OPTIONS%NY             /= NY )             THEN
             WRITE (*,101) 'Local Grid Y Dimension', NY, CMDLINE_OPTIONS%NY
          END IF
          IF (CMDLINE_OPTIONS%NZ             /= NZ )             THEN
             WRITE (*,101) 'Local Grid Z Dimension', NZ, CMDLINE_OPTIONS%NZ
          END IF
          IF (CMDLINE_OPTIONS%NVARS        /= NVARS )        THEN
             WRITE (*,101) 'Number of variables', NVARS, CMDLINE_OPTIONS%NVARS
          END IF
          IF (CMDLINE_OPTIONS%NTSTEPS      /= NTSTEPS )      THEN
             WRITE (*,101) 'Number of timesteps', NTSTEPS, CMDLINE_OPTIONS%NTSTEPS
          END IF
          IF (CMDLINE_OPTIONS%PERCENT_SUM     /= PERCENT_SUM )     THEN
             WRITE (*,101) 'Percent Variables Reduced', PERCENT_SUM, CMDLINE_OPTIONS%PERCENT_SUM
          END IF
          IF (CMDLINE_OPTIONS%DEBUG_GRID      /= DEBUG_GRID )     THEN
             WRITE (*,101) 'Grid Debugging', DEBUG_GRID, CMDLINE_OPTIONS%DEBUG_GRID
          END IF
      END IF

      ! Format statements

 100  FORMAT ( ' ==================== RESTART ====================' )

 101  FORMAT ( A32, ' cannot be changed at restart.  Requested ', I5, ' ; Replaced with ', I5, '.' )

 102  FORMAT ( A32, ' cannot be changed at restart.  Requested ', 1PE9.2, ' ; Replaced with ', 1PE9.2, '.' )

      RETURN

   END SUBROUTINE MG_CP_MPIIO_CHECK_CMDLINE

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_READ_SPIKES ( CMDLINE_OPTIONS, IERR )

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      TYPE ( CMDLINE_OPTIONS_TYPE ), INTENT(IN) :: &
         CMDLINE_OPTIONS

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER ::  &
         I,       &
         J,       &
         K

      TYPE ( SPIKES_TYPE )          :: MY_SPIKES

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      ALLOCATE ( MY_SPIKES%SPIKES(NVARS,CMDLINE_OPTIONS%NSPIKES), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_FILE_INIT: ALLOCATE ( SPIKES )', NVARS*CMDLINE_OPTIONS%NSPIKES )
      MY_SPIKES%SPIKES = -1.0

      ALLOCATE ( MY_SPIKES%SPIKE_LOC(0:3,CMDLINE_OPTIONS%NSPIKES), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_CP_MPIIO_FILE_INIT: ALLOCATE ( SPIKE_LOC )', 4*CMDLINE_OPTIONS%NSPIKES )
      MY_SPIKES%SPIKE_LOC = -1

      CALL MPI_FILE_SET_VIEW(restart_filehandle,        &
                             DISPLACEMENT%SPIKES_READ,  &
                             CP_SPIKES_READ_TYPE,       &
                             CP_GLOBALSPIKES_READ_TYPE, &
                             'native',                  &
                             MPI_INFO_NULL,             &
                             IERR)
      CALL MPI_FILE_READ_ALL (restart_filehandle, MY_SPIKES%SPIKES, 1, CP_SPIKES_READ_TYPE, MPI_STATUS_IGNORE, IERR)

      CALL MPI_FILE_SET_VIEW(restart_filehandle,          &
                             DISPLACEMENT%SPIKELOC_READ,  &
                             CP_SPIKELOC_READ_TYPE,       &
                             CP_GLOBALSPIKELOC_READ_TYPE, &
                             'native',                    &
                             MPI_INFO_NULL,               &
                             IERR)
      CALL MPI_FILE_READ_ALL (restart_filehandle, MY_SPIKES%SPIKE_LOC, 1, CP_SPIKELOC_READ_TYPE, MPI_STATUS_IGNORE, IERR)

#if defined _DEBUG_CHECKPOINT
      DO K = 0, NUMPES
        IF ( MYPE == K ) THEN
          DO I = 1, NVARS
             DO J = 1, CMDLINE_OPTIONS%NSPIKES
                IF ( SPIKES(I,J) /= MY_SPIKES%SPIKES(I,J) ) THEN
                   WRITE (*,301) K, I, J, SPIKES(I,J), MY_SPIKES%SPIKES(I,J)
                END IF
             END DO
          END DO
          DO I = 0, 3
             DO J = 1, CMDLINE_OPTIONS%NSPIKES
                IF ( SPIKE_LOC(I,J) /= MY_SPIKES%SPIKE_LOC(I,J) ) THEN
                   WRITE (*,302) K, I, J, SPIKE_LOC(I,J), MY_SPIKES%SPIKE_LOC(I,J)
                END IF
             END DO
          END DO
        END IF
        CALL MPI_BARRIER( MPI_COMM_WORLD, IERR )
      END DO
 301  FORMAT ( 'RANK(', I3, ')', 'SPIKES(', I2, ',', I2, ') == ORIG(', 1PE9.2, ') CP(', 1PE9.2, ')' )
 302  FORMAT ( 'RANK(', I3, ')', 'SPIKE_LOC(', I2, ',', I2, ') == ORIG(', I5, ') CP(', I5, ')' )
#endif

      DO I = 1, NVARS
         DO J = 1, CMDLINE_OPTIONS%NSPIKES
            SPIKES(I,J)=MY_SPIKES%SPIKES(I,J)
         END DO
      END DO
      DO I = 0, 3
         DO J = 1, CMDLINE_OPTIONS%NSPIKES
            SPIKE_LOC(I,J)=MY_SPIKES%SPIKE_LOC(I,J)
         END DO
      END DO


      DEALLOCATE ( MY_SPIKES%SPIKES )
      DEALLOCATE ( MY_SPIKES%SPIKE_LOC )

      RETURN

   END SUBROUTINE MG_CP_MPIIO_READ_SPIKES

!  ===================================================================================

!  ===================================================================================

   SUBROUTINE MG_CP_MPIIO_READ_TSTEP_VAR ( CP_NUM, GRID, IVAR, IERR )

      IMPLICIT NONE

      ! ---------------------
      ! Argument Declarations
      ! ---------------------

      REAL(KIND=MG_REAL), INTENT(INOUT) ::     &
         GRID(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS)
      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         CP_NUM,    &    ! checkpoint number to restart (0 based)
         IVAR
      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER ::            &
         I,                 &
         status

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      CALL MPI_FILE_SET_VIEW(restart_filehandle,                    &
                             MG_CP_MPIIO_GRID_DISPLACEMENT(CP_NUM, IVAR, .TRUE.), &
                             MG_MPI_REAL,                           &
                             CP_TSGRID_TYPE,                        &
                             'native',                              &
                             MPI_INFO_NULL,                         &
                             IERR)
      CALL MPI_FILE_READ_ALL ( restart_filehandle, GRID(0,0,0,IVAR), 1, CP_NOGHOST_TYPE, MPI_STATUS_IGNORE, IERR)

      RETURN

   END SUBROUTINE MG_CP_MPIIO_READ_TSTEP_VAR

!  ===================================================================================

!  ===================================================================================

   INTEGER FUNCTION MG_CP_MPIIO_LOGICAL_TO_INTEGER ( L )

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
         MG_CP_MPIIO_LOGICAL_TO_INTEGER=1
      ELSE
         MG_CP_MPIIO_LOGICAL_TO_INTEGER=0
      END IF

#if defined _DEBUG_CHECKPOINT
      WRITE (*,*) "L==", L, "INT==", MG_CP_MPIIO_LOGICAL_TO_INTEGER
#endif

      RETURN

   END FUNCTION MG_CP_MPIIO_LOGICAL_TO_INTEGER

!  ===================================================================================

!  ===================================================================================

   LOGICAL FUNCTION MG_CP_MPIIO_INTEGER_TO_LOGICAL ( I )

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
         MG_CP_MPIIO_INTEGER_TO_LOGICAL=.FALSE.
      ELSE
         MG_CP_MPIIO_INTEGER_TO_LOGICAL=.TRUE.
      END IF

#if defined _DEBUG_CHECKPOINT
      WRITE (*,*) "I==", I, "LOG==", MG_CP_MPIIO_INTEGER_TO_LOGICAL
#endif

      RETURN

   END FUNCTION MG_CP_MPIIO_INTEGER_TO_LOGICAL

!  ===================================================================================

#endif _MG_CHECKPT_MPIIO

#endif _MG_MPI

END MODULE MG_CHECKPOINT_MPIIO_MOD
