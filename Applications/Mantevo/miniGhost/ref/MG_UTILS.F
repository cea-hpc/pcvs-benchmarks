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

MODULE MG_UTILS_MOD

   USE MG_CONSTANTS_MOD
   USE MG_OPTIONS_MOD

   IMPLICIT NONE

CONTAINS

   ! MG_INIT             : Set up MPI environment, problem input.
   ! MG_PRINT_HEADER     : Write configuration to STDIO.
   ! MG_GRID_INIT        : Allocate GRID arrays.
   ! MG_INSERT_SPIKE     : Insert heat sources.
   ! MG_ASSERT           : Error checking.
   ! MG_TIMER            : Returns time.
   ! MG_COMPUTE_STDDEV   : Compute standard deviation of input profiling data.

!  =================================================================================

   SUBROUTINE MG_INIT ( IERR )

      IMPLICIT NONE

      INTEGER ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      INTEGER ::  &
         I,                    &    ! Counter.
         IX,                   &
         IY,                   &
         IZ,                   &
         J,                    &    ! Counter.
         MYPE_XY,              &    ! tmp var
         REMAINDER,            &
         XLOC,                 &
         YLOC,                 &
         ZLOC

      ! ------------
      ! Local Arrays
      ! ------------

      REAL(KIND=MG_REAL), DIMENSION(:,:), ALLOCATABLE ::   &
         RSPIKE_LOC            ! Temporary for random number generator.

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

#if defined _MG_MPI
      ! Parallel processing configuration:

      CALL MPI_COMM_DUP ( MPI_COMM_WORLD, MPI_COMM_MG, IERR )
      CALL MG_ASSERT ( IERR, "MG_INIT: MPI_COMM_DUP(MPI_COMM_MG)", IERR )

      CALL MPI_ERRHANDLER_SET ( MPI_COMM_MG, MPI_ERRORS_ARE_FATAL, IERR )
      CALL MG_ASSERT ( IERR, "MG_INIT: MPI_ERRHANDLER_SET", IERR )

      CALL MPI_COMM_RANK ( MPI_COMM_MG, MYPE, IERR )
      CALL MG_ASSERT ( IERR, "MG_INIT: MPI_COMM_RANK", IERR )

      CALL MPI_COMM_SIZE ( MPI_COMM_MG, NUMPES, IERR )
      CALL MG_ASSERT ( IERR, "MG_INIT: MPI_COMM_SIZE", IERR )

#elif defined _MG_SERIAL
      MYPE = 0    ! All formulas based on these values should work correctly.
      NUMPES = 1
#endif

      ! ---------------------------------
      ! Set position in 3d processor grid
      ! ---------------------------------

      MYPE_XY = MOD ( MYPE, NPX*NPY )
      MYPY = MYPE_XY / NPX
      REMAINDER = MOD ( MYPE_XY, NPX )
      IF ( REMAINDER /= 0 ) THEN
         MYPX = REMAINDER
      ELSE
         MYPX = 0
      END IF
      MYPZ = MYPE / ( NPX*NPY )

      ! --------------
      ! Set neighbors.
      ! --------------

      ALLOCATE ( NEIGHBORS(MAX_NUM_NEIGHBORS), STAT = IERR )
      NEIGHBORS(1:MAX_NUM_NEIGHBORS) = -1
      CALL MG_ASSERT ( IERR, 'MG_INIT: ALLOCATE ( NEIGHBORS )', MAX_NUM_NEIGHBORS )

      ALLOCATE ( NEIGHBORS_ORIG(MAX_NUM_NEIGHBORS), STAT = IERR )
      NEIGHBORS_ORIG(1:MAX_NUM_NEIGHBORS) = -1
      CALL MG_ASSERT ( IERR, 'MG_INIT: ALLOCATE ( NEIGHBORS_ORIG )', MAX_NUM_NEIGHBORS )

      NUM_NEIGHS = 0
      IF ( MYPY /= 0  ) THEN
         NEIGHBORS(SOUTH) = MYPE - NPX
         NUM_NEIGHS = NUM_NEIGHS + 1
      END IF
      IF ( MYPY /= NPY-1 ) THEN
         NEIGHBORS(NORTH) = MYPE + NPX
         NUM_NEIGHS = NUM_NEIGHS + 1
      END IF
      IF ( MYPX /= 0 ) THEN
         NEIGHBORS(WEST) = MYPE - 1
         NUM_NEIGHS = NUM_NEIGHS + 1
      END IF
      IF ( MYPX /= NPX-1 ) THEN
         NEIGHBORS(EAST) = MYPE + 1
         NUM_NEIGHS = NUM_NEIGHS + 1
      END IF
      IF ( MYPZ /= 0 ) THEN
         NEIGHBORS(BACK) = MYPE - ( NPX*NPY )
         NUM_NEIGHS = NUM_NEIGHS + 1
      END IF
      IF ( MYPZ /= NPZ-1 ) THEN
         NEIGHBORS(FRONT) = MYPE + ( NPX*NPY )
         NUM_NEIGHS = NUM_NEIGHS + 1
      END IF

      NEIGHBORS_ORIG = NEIGHBORS

      ALLOCATE ( SPIKES(NVARS,NSPIKES), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_INIT: ALLOCATE ( SPIKES )', NVARS*NSPIKES )
      SPIKES = -1.0

      ALLOCATE ( SPIKE_LOC(0:3,NSPIKES), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_INIT: ALLOCATE ( SPIKE_LOC )', 4*NSPIKES )
      SPIKE_LOC = -1

      ALLOCATE ( SOURCE_TOTAL(NVARS), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_INIT: ALLOCATE ( SOURCE_TOTAL )', NVARS )
      SOURCE_TOTAL = 0.0

      ! -------------------------------------------------------
      ! ROOTPE generates and distributes SPIKES and SPIKE_LOC.
      ! Each GRIDi has a unique SPIKE value, but for each set,
      ! all in same location within the GRIDi array.
      ! Global location computed, PEs determines ownership.
      ! -------------------------------------------------------

      ! Determine global indices (excluding ghost)

      GLOBAL_NX = NX * NPX
      GLOBAL_NY = NY * NPY
      GLOBAL_NZ = NZ * NPZ

      MY_GLOBAL_NX_START = NX * MYPX + 1
      MY_GLOBAL_NY_START = NY * MYPY + 1
      MY_GLOBAL_NZ_START = NZ * MYPZ + 1

      MY_GLOBAL_NX_END   = MY_GLOBAL_NX_START + NX - 1
      MY_GLOBAL_NY_END   = MY_GLOBAL_NY_START + NY - 1
      MY_GLOBAL_NZ_END   = MY_GLOBAL_NZ_START + NZ - 1

      IF ( MYPE == ROOTPE ) THEN

         CALL MG_RANDOM_SEED ( IERR )
         CALL RANDOM_NUMBER ( SPIKES ( 1:NVARS,1:NSPIKES ) )
         SPIKES = ( SPIKES + 1.0 ) * GLOBAL_NX * GLOBAL_NY * GLOBAL_NZ * NUMPES

         CALL MG_RANDOM_SEED ( IERR )
         ALLOCATE ( RSPIKE_LOC(3,NSPIKES), STAT = IERR )
         CALL RANDOM_NUMBER ( RSPIKE_LOC )

         ! First spike set to problem dimension, inserted center of global grid.
         SPIKES(1,1) = REAL ( GLOBAL_NX * GLOBAL_NY * GLOBAL_NZ )

         SPIKE_LOC(0,1) = -1
         SPIKE_LOC(1,1) = ( GLOBAL_NX + 1 ) / 2   ! The +1 covers the case of a 2d domain.
         SPIKE_LOC(2,1) = ( GLOBAL_NY + 1 ) / 2
         SPIKE_LOC(3,1) = ( GLOBAL_NZ + 1 ) / 2

         ! Set additional spikes randomly about global grid

         DO I = 2, NSPIKES
            SPIKE_LOC(0,I) = -1   ! Owning MPI rank will set to its MYPE.
            SPIKE_LOC(1,I) = INT ( ( RSPIKE_LOC(1,I) * GLOBAL_NX ) + 1 )
            SPIKE_LOC(2,I) = INT ( ( RSPIKE_LOC(2,I) * GLOBAL_NY ) + 1 )
            SPIKE_LOC(3,I) = INT ( ( RSPIKE_LOC(3,I) * GLOBAL_NZ ) + 1 )
         END DO

         DEALLOCATE ( RSPIKE_LOC )

      END IF ! ROOTPE work.

#if defined _MG_MPI

      ! -----------------
      ! Distribute SPIKES
      ! -----------------

      CALL MPI_BCAST ( SPIKES, NVARS*NSPIKES, MG_MPI_REAL, ROOTPE,  &
                       MPI_COMM_MG, IERR )
      CALL MG_ASSERT ( IERR, 'INIT: MPI_BCAST(SPIKES)', IERR )

      CALL MPI_BCAST ( SPIKE_LOC, 4*NSPIKES, MPI_INTEGER, ROOTPE,  &
                       MPI_COMM_MG, IERR )
      CALL MG_ASSERT ( IERR, 'INIT: MPI_BCAST(SPIKE_LOC)', IERR )

#endif

      ! Owning MPI rank determines converts the global location to its local array indices.

      DO I = 1, NSPIKES

         XLOC = SPIKE_LOC ( 1, I ) ! Global values here.
         YLOC = SPIKE_LOC ( 2, I )
         ZLOC = SPIKE_LOC ( 3, I )

         IF ( ( MY_GLOBAL_NX_START <= XLOC ) .AND. ( XLOC <= MY_GLOBAL_NX_END ) .AND.  &
              ( MY_GLOBAL_NY_START <= YLOC ) .AND. ( YLOC <= MY_GLOBAL_NY_END ) .AND.  &
              ( MY_GLOBAL_NZ_START <= ZLOC ) .AND. ( ZLOC <= MY_GLOBAL_NZ_END ) ) THEN

            SPIKE_LOC ( 0, I ) = MYPE

            SPIKE_LOC ( 1, I ) = SPIKE_LOC ( 1, I ) - MY_GLOBAL_NX_START + 1
            SPIKE_LOC ( 2, I ) = SPIKE_LOC ( 2, I ) - MY_GLOBAL_NY_START + 1
            SPIKE_LOC ( 3, I ) = SPIKE_LOC ( 3, I ) - MY_GLOBAL_NZ_START + 1

         ELSE

            ! Not owner, so set to -1 (to make obvious).

            SPIKE_LOC ( 0, I ) = -1

            SPIKE_LOC ( 1, I ) = -1
            SPIKE_LOC ( 2, I ) = -1
            SPIKE_LOC ( 3, I ) = -1

         END IF

      END DO

      RETURN

   END SUBROUTINE MG_INIT

!  ===================================================================================

   SUBROUTINE MG_PRINT_HEADER ( COMM_METHOD, STENCIL, IERR )

      IMPLICIT NONE

      ! Argument Declarations
      INTEGER(KIND=MG_INT), INTENT(IN)  ::       &
         COMM_METHOD, STENCIL

      INTEGER(KIND=MG_INT), INTENT(OUT) :: IERR

      !  Purpose
      !  =======
      !  Collate, process, and report performance results.

      ! Local Scalars
      CHARACTER(LEN=30) ::        &
         TEST_DATE
      CHARACTER(LEN=30) ::        &
         TEST_TIME

      INTEGER ::            &
         I,                              &
         ICLOCK_RATE,                    &
         IDUM,                           &
         LEN

      REAL(KIND=MG_REAL4) ::       &
         CLOCK_RES = 0.0

      INTEGER(KIND=MG_INT8), PARAMETER ::  &
         SIZE_OF_DATA = 8

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      IF ( MYPE /= ROOTPE ) &
         RETURN

      CALL DATE_AND_TIME ( TEST_DATE, TEST_TIME )

      WRITE(*,*)
      WRITE(*,100)
      WRITE(*,*) '          Mantevo miniapp MiniGhost experiment'
      WRITE(*,100)

#if defined _MG_MPI
      CLOCK_RES = REAL(MPI_WTICK ( ))

      WRITE(*,*)
      SELECT CASE ( COMM_METHOD )
         CASE ( COMM_METHOD_BSPMA )

            WRITE(*,*) 'Communication strategy: full message aggregation (COMM_METHOD_BSPMA)'

         CASE ( COMM_METHOD_SVAF )

            WRITE(*,*) 'Communication strategy: one variable at a time (COMM_METHOD_SVAF)'

         CASE DEFAULT

            WRITE(*,*) '** Warning ** Unknown communication strategy'

      END SELECT
#endif

      WRITE(*,*)

      SELECT CASE ( STENCIL )

         CASE ( STENCIL_NONE )

            WRITE(*,*) 'No computation inserted.'

         CASE ( STENCIL_2D5PT )

            WRITE(*,*) 'Computation: 5 pt difference stencil on a 2D grid (STENCIL_2D5PT)'

         CASE ( STENCIL_2D9PT )

            WRITE(*,*) 'Computation: 9 pt difference stencil on a 2D grid (STENCIL_2D9PT)'

         CASE ( STENCIL_3D7PT )

            WRITE(*,*) 'Computation: 7 pt difference stencil on a 3D grid (STENCIL_3D7PT)'

         CASE ( STENCIL_3D27PT )

            WRITE(*,*) 'Computation: 27 pt difference stencil on a 3D grid stencil (STENCIL_3D27PT)'

         CASE DEFAULT

            WRITE(*,*) '** Warning ** Unknown computation'

      END SELECT

      WRITE(*,*)
      WRITE(*,101) NX * NPX, NY * NPY, NZ * NPZ
      WRITE(*,102) NX, NY, NZ
      WRITE(*,*)
      WRITE(*,105) NVARS
      WRITE(*,*)
      IF ( REPORT_DIFFUSION /= 0 ) THEN
         WRITE(*,115) REPORT_DIFFUSION, ERROR_TOL
      END IF
      WRITE(*,106) NUM_SUM_GRID, PERCENT_SUM
      WRITE(*,*)
      WRITE(*,110) NTSTEPS
      WRITE(*,*)
      WRITE(*,111) NSPIKES
      WRITE(*,*)

#if defined _MG_MPI
      IF ( SCALING == SCALING_STRONG ) THEN   ! Not that it really matters.
         WRITE(*,*) 'MPI version, strong scaling'
      ELSE
         WRITE(*,*) 'MPI version, weak scaling'
      END IF
      WRITE(*,*)
      IF ( NUMPES == 1 ) THEN
         WRITE(*,120) TEST_TIME, TEST_DATE
      ELSE
         WRITE(*,121) NUMPES, NPX, NPY, NPZ, TEST_TIME, TEST_DATE
      END IF
#elif defined _MG_SERIAL
      IF ( SCALING == SCALING_STRONG ) THEN   ! Not that it really matters.
         WRITE(*,140) 'Serial version, strong scaling', TEST_TIME, TEST_DATE
      ELSE
         WRITE(*,140) 'Serial version, weak scaling', TEST_TIME, TEST_DATE
      END IF
      WRITE(*,*)
#endif

      ! Format statements

 100  FORMAT ( ' ========================================================' )

 101  FORMAT ( '      Global Grid Dimension: ', I8, ', ', I8, ', ', I8 )
 102  FORMAT ( '      Local Grid Dimension : ', I8, ', ', I8, ', ', I8 )

 105  FORMAT ( ' Number of variables: ', I2 )
 106  FORMAT ( ' Number of variables reduced each time step: ', I2, '; requested  ', I3, '%.')

 110  FORMAT ( ' Time steps per spike: ', I6 )
 111  FORMAT ( ' Number of spikes:     ', I6 )

 115  FORMAT ( ' Error reported every ', I3, ' time steps. Tolerance is ', 1PE9.2 )

 120  FORMAT ( ' 1 process executing', // &
               ' Program execution at ', A10, ' on ', A8, '.' )

 121  FORMAT ( I4, ' processes executing on logical ', I5, ' x ', I5, ' x ', I5, ' processor grid' // &
             ' Program execution at ', A10, ' on ', A8, '.', // )
 140  FORMAT ( A32, ', ', A10, ' on ', A8, '.' )

   END SUBROUTINE MG_PRINT_HEADER

!  ===================================================================================

   SUBROUTINE MG_GRID_INIT ( GRID, IERR )

      INTEGER(KIND=MG_INT), INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER(KIND=MG_INT) :: &
         I, J, K, IVAR, ISEED,       & ! Counters
         ITMP

      INTEGER(KIND=MG_INT), DIMENSION(:), ALLOCATABLE ::    &
         MG_SEEDS

      REAL(KIND=MG_REAL) ::     &
         GRID(0:NX+1, 0:NY+1, 0:NZ+1, NVARS ), &
         LSUM(NVARS)              ! Sum of locally owned GRID

      integer pe
      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      ! Initialize each GRID array. Set SOURCE_TOTAL to sum of elements.

      CALL RANDOM_SEED ( ISEED )
      ALLOCATE ( MG_SEEDS(ISEED), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_GRID_INIT:ALLOCATE ( MG_SEEDS(ISEED) )', ISEED )
      DO I = 1, ISEED
         MG_SEEDS(I) = MYPE + 7 + ISEED
      END DO
!     CALL RANDOM_SEED ( PUT = MG_SEEDS )

      ! Explicit loops enable first touch affinity for OpenMP.

!$OMP PARALLEL DO
      DO IVAR = 1, NVARS
         DO K = 0, NZ+1
            DO J = 0, NY+1
               DO I = 0, NX+1
                  GRID(I, J, K, IVAR) = 0.0
               END DO
            END DO
         END DO
      END DO
!$OMP END PARALLEL DO
      SOURCE_TOTAL = 0.0

      IF ( DEBUG_GRID /= 1 ) THEN

!$OMP PARALLEL DO
         DO IVAR = 1, NVARS
            LSUM(IVAR) = 0.0
            DO K = 1, NZ
               DO J = 1, NY
                  DO I = 1, NX
                     CALL MG_RANDOM_SEED ( IERR )
                     CALL RANDOM_NUMBER( GRID(I, J, K, IVAR) )
                     ITMP = GRID(I, J, K, IVAR)*100
                     GRID(I, J, K, IVAR) = MOD ( ITMP, NX*NY*NZ ) + 1
                     LSUM(IVAR) = LSUM(IVAR) + GRID(I, J, K, IVAR )
                  END DO
               END DO
            END DO
         END DO
!$OMP END PARALLEL DO

      END IF

#if defined _MG_MPI
      CALL MPI_ALLREDUCE ( LSUM, SOURCE_TOTAL, NVARS, MG_MPI_REAL, MPI_SUM, MPI_COMM_MG, IERR )
      CALL MG_ASSERT ( IERR, 'MG_GRID_INIT : MPI_ALLREDUCE', 1 )
#elif defined _MG_SERIAL
      DO IVAR = 1, NVARS
         SOURCE_TOTAL(IVAR) = LSUM(IVAR)
      END DO
#endif

      ALLOCATE ( WORK( 0:NX+1, 0:NY+1, 0:NZ+1 ), STAT = IERR )
      CALL MG_ASSERT ( IERR, 'MG_GRID_INIT: ALLOCATE ( WORK )', (NX+2)*(NY+2)*(NZ+2) )

      RETURN

   END SUBROUTINE MG_GRID_INIT

!  ===================================================================================

   SUBROUTINE MG_INSERT_SPIKE ( GRID, SPIKE_NUM, IERR )

      ! Insert heat source (SPIKES) into arrays.
      ! This vlaue is added to the total heat applied to the variable.

      INTEGER(KIND=MG_INT), INTENT(IN) ::  &
         SPIKE_NUM

      REAL(KIND=MG_REAL), DIMENSION(0:NX+1, 0:NY+1, 0:NZ+1, 1:NVARS), INTENT(INOUT) :: &
         GRID

      INTEGER(KIND=MG_INT), INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ------------------
      ! Local Declarations
      ! ------------------

      INTEGER ::  &
         IX,      & ! Local index
         IY,      & ! Local index
         IZ,      & ! Local index
         IVAR       ! Counter

      integer i, j, k, pe
      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR = 0

      IF ( MYPE == ( SPIKE_LOC ( 0, SPIKE_NUM ) ) ) THEN

         IX = SPIKE_LOC ( 1, SPIKE_NUM )
         IY = SPIKE_LOC ( 2, SPIKE_NUM )
         IZ = SPIKE_LOC ( 3, SPIKE_NUM )

         DO IVAR = 1, NVARS
            GRID( IX, IY, IZ, IVAR ) = GRID( IX, IY, IZ, IVAR ) + SPIKES( IVAR, SPIKE_NUM )
         END DO

      END IF

      IF ( MYPE == ROOTPE ) THEN    ! FIXME: Only root keeps running total. Copacetic?
         DO IVAR = 1, NVARS
            SOURCE_TOTAL( IVAR ) = SOURCE_TOTAL( IVAR ) + SPIKES( IVAR, SPIKE_NUM )
         END DO
      END IF

   END SUBROUTINE MG_INSERT_SPIKE

!  ===================================================================================

   SUBROUTINE MG_ASSERT ( IERR, ERROR_MSG, INFO )

      USE MG_CONSTANTS_MOD

      IMPLICIT NONE

      CHARACTER(LEN=*), INTENT(IN) ::    &
         ERROR_MSG

      INTEGER(KIND=MG_INT), INTENT(IN) ::             &
         IERR,                           & ! Error code.
         INFO

      ! ---------------------
      ! Executable Statements
      ! ---------------------

#if defined _MG_MPI

      CHARACTER*(MPI_MAX_ERROR_STRING) STRING
      INTEGER RESULTLEN, IERROR

      IF ( IERR /= 0 ) THEN
         WRITE(*,80) MYPE, TRIM(ERROR_MSG), IERR, INFO
         CALL MPI_ERROR_STRING(IERR, STRING, RESULTLEN, IERROR)
         WRITE(*,81) STRING
         call sleep(3)
         CALL MPI_ABORT ( MPI_COMM_MG, -1, IERR )
      END IF

 80   FORMAT ( '** Error ** [pe ', I5, '] ', A40, '; CODE = ', I7, &
               '. Additional info:', I4 )
 81   FORMAT ( 'MPI error message: ', A80)

#else

      IF ( IERR /= 0 ) THEN
         WRITE(*,90) TRIM(ERROR_MSG), IERR, INFO
         STOP
      END IF

#endif

 90   FORMAT ( '** Error ** ', A40, '; CODE = ', I7, '. Additional info:', I4 )


   END SUBROUTINE MG_ASSERT

!  ===================================================================================

   DOUBLE PRECISION FUNCTION MG_TIMER ()

#if defined _MG_MPI
      include 'mpif.h'

      MG_TIMER = MPI_WTIME ()

#else
      INTEGER COUNT_1, COUNT_RATE, COUNT_MAX

      CALL SYSTEM_CLOCK (COUNT_1, COUNT_RATE, COUNT_MAX)
      MG_TIMER = COUNT_1 * 1.0 / COUNT_RATE

#endif

      RETURN

   END FUNCTION MG_TIMER
!  ===================================================================================

   DOUBLE PRECISION FUNCTION MG_COMPUTE_STDDEV ( VALUES, MEAN )

      DOUBLE PRECISION :: &
         VALUES(*), MEAN

      INTEGER :: I
      DOUBLE PRECISION :: TMP

      TMP = 0.0
      DO I = 1, NUMPES
         TMP = TMP + ( VALUES(I) - MEAN )**2
      END DO

      MG_COMPUTE_STDDEV = SQRT ( TMP / REAL( NUMPES ))

      RETURN

   END FUNCTION MG_COMPUTE_STDDEV

!  ===================================================================================

   SUBROUTINE MG_RANDOM_SEED( IERR )

      IMPLICIT NONE

      INTEGER, INTENT(OUT) ::  &
         IERR                    ! Return status.

      ! ---------------
      ! Local Variables
      ! ---------------

      integer :: values(1:8), rs_size
      integer, dimension(:), allocatable :: seed

      ! ---------------------
      ! Executable Statements
      ! ---------------------

      IERR=0

      data  values/1,2,3,4,5,6,7,8/
      call random_seed(size=rs_size)
      allocate(seed(1:rs_size))
      seed(:) = values(8)
      call random_seed(put=seed)

      RETURN

   END SUBROUTINE MG_RANDOM_SEED


END MODULE MG_UTILS_MOD
