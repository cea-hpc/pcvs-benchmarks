C ----------------------------------------------------------------------
C MESSAGE PASSING INTERFACE TEST CASE SUITE
C
C Copyright - 1996 Intel Corporation
C
C Intel Corporation hereby grants a non-exclusive license under Intel's
C copyright to copy, modify and distribute this software for any purpose 
C and without fee, provided that the above copyright notice and the
C following paragraphs appear on all copies.
C
C Intel Corporation makes no representation that the test cases
C comprising this suite are correct or are an accurate representation
C of any standard.
C
C IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT
C OR SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING,
C CONSEQUENTIAL, INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT
C LIMITED TO INFRINGEMENT, LOSS OF USE, BUSINESS INTERRUPTIONS, AND
C LOSS OF PROFITS, IRRESPECTIVE OF WHETHER INTEL HAS ADVANCE NOTICE OF
C THE POSSIBILITY OF ANY SUCH DAMAGES.
C
C INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING,
C BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS
C FOR A PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED
C HEREUNDER IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO
C OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR
C MODIFICATIONS.
C
C ----------------------------------------------------------------------
C **********************************************************************
C                           Test for MPI_Type_extent()
C 
C All rank call MPI_Type_extent() with MPI_LB and MPI_UB.  The resulting extent
C will be verified (should be 0).
C 
C This test may be run in any communicator with a minimum of 1 group members.
C 
C The MPITEST environment provides looping over communicator size.
C The properties of the loops are encoded in configuration
C arrays in the file config.h .
C 
C MPI Calls dependencies for this test:
C   MPI_Init(), MPI_Finalize(), MPI_Comm_test_inter(), MPI_Error_string(),
C   MPI_Type_extent(), 
C   [MPI_Get_count(), MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]
C 
C Test history:
C    1  08/08/96     simont       Original version
C ******************************************************************************/
C
#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER TEST_NUMP
      INTEGER COMM_INDEX
      INTEGER COMM_TYPE
      INTEGER COMM_COUNT
      INTEGER MAX_COMM_COUNT
      INTEGER FAIL
      INTEGER SIZE
      INTEGER LOOP_CNT
      INTEGER IERR, IERR2

      INTEGER COMM
C     WDG - 4/30/93 The original test use MPITEST_AINT, but this is not
c     correct for the Fortran binding
      INTEGER EXTENT

      LOGICAL INTER_FLAG

      INTEGER MPITEST_MIN_DISPL
      PARAMETER (MPITEST_MIN_DISPL = 100)

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32        TESTNAME

      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING

 99   FORMAT(A, INT_FMT)
 100  FORMAT(A200)
 110  FORMAT(A)
 130  FORMAT(A, INT_FMT, A)
 140  FORMAT(A, A, INT_FMT)

C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF,99) 'MPI_INIT() returned ', IERR 
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ENDIF

      TESTNAME='MPI_Type_extent_MPI_LB_UB'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
         INFOBUF = 'Starting test '//TESTNAME
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      ENDIF

      FAIL = 0
      LOOP_CNT = 0

C     Find the maximum number of the communicators in config array
C
      CALL MPITEST_NUM_COMM_SIZES(MAX_COMM_COUNT)

      DO 300 COMM_COUNT = 1, MAX_COMM_COUNT
         CALL MPITEST_GET_COMM_INDEX(COMM_COUNT, COMM_INDEX)
         CALL MPITEST_GET_COMM_TYPE(COMM_COUNT, COMM_TYPE)

         CALL MPITEST_GET_COMMUNICATOR(COMM_TYPE, COMM_INDEX,
     $                                 COMM, TEST_NUMP)

         IF (COMM .EQ. MPI_COMM_NULL) GO TO 1000

         IF (TEST_NUMP .LT. 2) THEN
C           Skipping communicator with comm size < 2
C
            WRITE(INFOBUF, 130)
     $         'Skipping communicator with comm_size < 2 (commtype: ',
     $         COMM_TYPE, ') for this test'
            CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
         ELSE
            CALL MPI_COMM_TEST_INTER(COMM, INTER_FLAG, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
               WRITE(INFOBUF,99) 'MPI_COMM_TEST_INTER() returned ',
     $            IERR 
               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
               ERRORSTRING=' '
               CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
               WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
               CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
            ENDIF

            IF (INTER_FLAG .EQV. .TRUE.) THEN
               WRITE(INFOBUF, 130)
     $            'Skipping inter communicator (commtype: ',
     $            COMM_TYPE, ') for this test'
               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            ELSE

               LOOP_CNT = LOOP_CNT + 1

C              MPI_LB
C
               WRITE(INFOBUF, 110)
     $            'Calling MPI_TYPE_EXTENT() with MPI_LB'
               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

               CALL MPI_TYPE_EXTENT(MPI_LB, EXTENT, IERR)
               IF (IERR .NE. MPI_SUCCESS) THEN
                  WRITE(INFOBUF, 99)
     $               'MPI_TYPE_EXTENT() returned ', IERR
                  CALL MPITEST_MESSAGE(
     $               MPITEST_NONFATAL, INFOBUF)
                  ERRORSTRING=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $               SIZE, IERR2)
                  WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                  CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
               END IF

               IF (EXTENT .NE. 0) THEN
                  FAIL = FAIL + 1
                  WRITE(INFOBUF, 140)
     $               'unexpected return MPI_TYPE_EXTENT() w/ MPI_LB ',
     $               'Expected: 0 Actual: ', EXTENT
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
               END IF

C              MPI_UB
C
               WRITE(INFOBUF, 110)
     $            'Calling MPI_TYPE_EXTENT() with MPI_UB'
               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

               CALL MPI_TYPE_EXTENT(MPI_UB, EXTENT, IERR)
               IF (IERR .NE. MPI_SUCCESS) THEN
                  WRITE(INFOBUF, 99)
     $               'MPI_TYPE_EXTENT() returned ', IERR
                  CALL MPITEST_MESSAGE(
     $               MPITEST_NONFATAL, INFOBUF)
                  ERRORSTRING=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $               SIZE, IERR2)
                  WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                  CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
               END IF

               IF (EXTENT .NE. 0) THEN
                  FAIL = FAIL + 1
                  WRITE(INFOBUF, 140)
     $               'unexpected return MPI_TYPE_EXTENT() w/ MPI_UB ',
     $               'Expected: 0 Actual: ', EXTENT
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
               END IF
            END IF
         END IF
 1000    CONTINUE

         CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
 300  CONTINUE

C     Report overall result
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
