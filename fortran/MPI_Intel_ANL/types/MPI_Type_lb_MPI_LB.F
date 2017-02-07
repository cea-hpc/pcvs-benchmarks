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
C                           Test for MPI_TYPE_LB()
C 
C All ranks will first do a MPI_TYPE_STRUCT() of all types in the type config
C array (in config.h) plus MPI_LB having diplacement MPI_MIN_DISPL
C with the minimum displacement.  All ranks then use call MPI_TYPE_LB() using
C the newly created datatype and verify that the returned displacement is equal
C to MPITEST_MIN_DISPL.
C 
C This test may be run in any communicator with a minimum of 1 group members,
C with any data type, and with any message length.
C 
C The MPITEST environment provides looping over communicator size,
C message length.  The properties of the loops are encoded in configuration
C arrays in the file config.h .
C 
C MPI Calls dependencies for this test:
C   MPI_INIT(), MPI_FINALIZE(), MPI_TYPE_LB(),
C   MPI_COMM_TEST_INTER(), MPI_ERROR_STRING(),
C   MPI_TYPE_STRUCT(), MPI_TYPE_COMMIT(),
C   MPI_TYPE_SIZE(), MPI_TYPE_FREE(),
C   [MPI_GET_COUNT(), MPI_ALLREDUCE(), MPI_COMM_RANK(), MPI_COMM_SIZE()]
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

      INTEGER TEST_TYPE
      INTEGER TEST_NUMP
      INTEGER LENGTH_COUNT
      INTEGER LENGTH
      INTEGER COMM_INDEX
      INTEGER COMM_TYPE
      INTEGER COMM_COUNT
      INTEGER MAX_COMM_COUNT
      INTEGER TYPE_COUNT
      INTEGER MAX_TYPE_COUNT
      INTEGER MAX_LENGTH_COUNT
      INTEGER FAIL
      INTEGER SIZE
      INTEGER LOOP_CNT
      INTEGER IERR, IERR2

      INTEGER COMM

      INTEGER MPITEST_MIN_DISPL
      PARAMETER (MPITEST_MIN_DISPL = 10)

      INTEGER NEWTYPE, TYPES(MPITEST_CFGSIZ)

      INTEGER BLKLENS(MPITEST_CFGSIZ), TYPE_SIZES(MPITEST_CFGSIZ)
C WDG - 4/28/03 - Original code use MPITEST_AINT, which is not correct
C for the fortran binding
      INTEGER DISPLS(MPITEST_CFGSIZ)
      INTEGER DISPL
      INTEGER XT

      LOGICAL INTER_FLAG

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32        TESTNAME

      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING

 99   FORMAT(A, INT_FMT)
 100  FORMAT(A200)
 110  FORMAT(A)
 130  FORMAT(A, INT_FMT, A)
 140  FORMAT(A, A, INT_FMT)
 150  FORMAT(A, A, INT_FMT, A, INT_FMT)
 160  FORMAT(A, INT_FMT, INT_FMT, A, INT_FMT, A, INT_FMT,
     $   A, INT_FMT)

C
C     Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF,99) 'MPI_INIT() returned ', IERR 
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ENDIF

      TESTNAME='MPI_Type_lb_MPI_LB'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
         INFOBUF = 'Starting test '//TESTNAME
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      ENDIF

      FAIL = 0
      LOOP_CNT = 0

      CALL MPITEST_NUM_DATATYPES(MAX_TYPE_COUNT)

C     for MPI_LB
C
      MAX_TYPE_COUNT = MAX_TYPE_COUNT + 1

C     Make sure the number of input types is not too large
C
      IF (MAX_TYPE_COUNT .GT. MPITEST_CFGSIZ) THEN
         WRITE(INFOBUF, 130) 'Input number of types too large,
     $      using the first ', MPITEST_CFGSIZ - 1, ' types only'
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
         MAX_TYPE_COUNT = MPITEST_CFGSIZ
      END IF

C     Find the maximum number of the communicators in config array
C
      CALL MPITEST_NUM_COMM_SIZES(MAX_COMM_COUNT)

      DO 300 COMM_COUNT = 1, MAX_COMM_COUNT
         CALL MPITEST_GET_COMM_INDEX(COMM_COUNT, COMM_INDEX)
         CALL MPITEST_GET_COMM_TYPE(COMM_COUNT, COMM_TYPE)

         CALL MPITEST_GET_COMMUNICATOR(COMM_TYPE, COMM_INDEX,
     $                                 COMM, TEST_NUMP)

         IF (COMM .EQ. MPI_COMM_NULL) GO TO 1000

         IF (TEST_NUMP .LT. 1) THEN
C           Skipping communicator with comm size < 1
C
            WRITE(INFOBUF, 130)
     $         'Skipping communicator with comm_size < 1 (commtype: ',
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
               CALL MPITEST_NUM_MESSAGE_LENGTHS(MAX_LENGTH_COUNT)

               DO 400 LENGTH_COUNT = 1, MAX_LENGTH_COUNT, 1
                  CALL MPITEST_GET_MESSAGE_LENGTH(LENGTH_COUNT, LENGTH)

                  DO 500 TYPE_COUNT = 1, MAX_TYPE_COUNT - 1, 1
                     CALL MPITEST_GET_DATATYPE(TYPE_COUNT,
     $                  TEST_TYPE)
                     TYPES(TYPE_COUNT) =
     $                  MPITEST_MPI_DATATYPES(TEST_TYPE)

                     IF (TYPE_COUNT .EQ. 1) THEN
                        DISPLS(TYPE_COUNT) = LENGTH
                     ELSE
                        DISPLS(TYPE_COUNT) = DISPLS(TYPE_COUNT - 1)
     $                     + TYPE_SIZES(TYPE_COUNT-1)
                     END IF

                     BLKLENS(TYPE_COUNT) = 1

                     CALL MPI_TYPE_SIZE(TYPES(TYPE_COUNT),
     $                  TYPE_SIZES(TYPE_COUNT), IERR)
                     IF (IERR .NE. MPI_SUCCESS) THEN
                        WRITE(INFOBUF,99) 
     $                     'MPI_TYPE_SIZE() returned ', IERR 
                        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                        ERRORSTRING=' '
                        CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                     SIZE, IERR2)
                        WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                     END IF

                     CALL MPI_TYPE_EXTENT(TYPES(TYPE_COUNT),
     $                  XT, IERR)
                     IF (IERR .NE. MPI_SUCCESS) THEN
                        WRITE(INFOBUF,99)
     $                     'MPI_TYPE_EXTENT() returned ',
     $                     IERR
                        CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                     INFOBUF)
                        ERRORSTRING=' '
                        CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                     SIZE, IERR2)
                        WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                     INFOBUF)
                     END IF
 500              CONTINUE

                  TYPES(MAX_TYPE_COUNT) = MPI_LB
                  DISPLS(MAX_TYPE_COUNT) = MPITEST_MIN_DISPL
                  BLKLENS(MAX_TYPE_COUNT) = 1
                  TYPE_SIZES(MAX_TYPE_COUNT) = 0

                  CALL MPI_TYPE_STRUCT(MAX_TYPE_COUNT, BLKLENS, DISPLS,
     $               TYPES, NEWTYPE, IERR)

                  IF (IERR .NE. MPI_SUCCESS) THEN
                     WRITE(INFOBUF, 99)
     $                  'MPI_TYPE_STRUCT() returned ', IERR
                     CALL MPITEST_MESSAGE(
     $                  MPITEST_NONFATAL, INFOBUF)
                     ERRORSTRING=' '
                     CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                  SIZE, IERR2)
                     WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                     CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                  END IF

                  CALL MPI_TYPE_COMMIT(NEWTYPE, IERR)

                  IF (IERR .NE. MPI_SUCCESS) THEN
                     WRITE(INFOBUF, 99)
     $                  'MPI_TYPE_COMMIT() returned ', IERR
                     CALL MPITEST_MESSAGE(
     $                  MPITEST_NONFATAL, INFOBUF)
                     ERRORSTRING=' '
                     CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                  SIZE, IERR2)
                     WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                     CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                  END IF

                  IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                     WRITE(INFOBUF, 160) '(', LENGTH_COUNT, COMM_COUNT,
     $                  ') length ', LENGTH, ' commsize ', TEST_NUMP,
     $                  ' commtype ', COMM_TYPE
                     CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
                  END IF

                  LOOP_CNT = LOOP_CNT + 1

C                 Test MPI_TYPE_LB() for this datatype
C
                  CALL MPI_TYPE_LB(NEWTYPE, DISPL, IERR)

                  IF (IERR .NE. MPI_SUCCESS) THEN
                     WRITE(INFOBUF, 99)
     $                  'MPI_TYPE_LB() returned ', IERR
                     CALL MPITEST_MESSAGE(
     $                  MPITEST_NONFATAL, INFOBUF)
                     ERRORSTRING=' '
                     CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                  SIZE, IERR2)
                     WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                     CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                  ELSE IF (DISPL .NE. MPITEST_MIN_DISPL) THEN
                     FAIL = FAIL + 1
                     WRITE(INFOBUF, 150)
     $                  'MPI_TYPE_LB() returned unexpected ',
     $                  'displacement Expected: ', MPITEST_MIN_DISPL,
     $                  ', Actual: ', DISPL
                     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                  END IF

C                 Free newly created datatype
C
                  CALL MPI_TYPE_FREE(NEWTYPE, IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                     WRITE(INFOBUF, 99)
     $                  'MPI_TYPE_FREE() returned ', IERR
                     CALL MPITEST_MESSAGE(
     $                  MPITEST_NONFATAL, INFOBUF)
                     ERRORSTRING=' '
                     CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                  SIZE, IERR2)
                     WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                     CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                  END IF

#ifdef MPITEST_SYNC
                  CALL MPI_BARRIER(COMM, IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                     WRITE(INFOBUF, 99)
     $                  'MPI_BARRIER() returned ', IERR
                     CALL MPITEST_MESSAGE(
     $                  MPITEST_NONFATAL, INFOBUF)
                     ERRORSTRING=' '
                     CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                  SIZE, IERR2)
                     WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                     CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                  END IF
#endif
 400           CONTINUE
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
