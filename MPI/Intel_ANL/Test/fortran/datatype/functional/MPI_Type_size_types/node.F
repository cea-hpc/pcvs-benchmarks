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
C                           Test for MPI_TYPE_SIZE()
C 
C All ranks will first do a MPI_TYPE_STRUCT() of all types in the type config
C array (in config.h).  Each rank then use the length (from config.h) to
C replicate the datatype following by a call to MPI_TYPE_SIZE() using the resulting
C concatenated type.  The resulting type will be verified to be > 0.
C 
C This test may be run in any communicator with a minimum of 2 group members,
C with any data type, and with any non-negative message length.
C 
C The MPITEST environment provides looping over communicator size,
C message length.  The properties of the loops are encoded in configuration
C arrays in the file config.h .
C 
C MPI Calls dependencies for this test:
C   MPI_INIT(), MPI_FINALIZE(), MPI_COMM_TEST_INTER(), MPI_ERROR_STRING(),
C   MPI_TYPE_STRUCT(), MPI_TYPE_COMMIT(),
C   MPI_TYPE_SIZE(), MPI_TYPE_EXTENT(), MPI_TYPE_FREE(),
C   [MPI_GET_COUNT(), MPI_ALLREDUCE(), MPI_COMM_RANK(), MPI_COMM_SIZE()]
C 
C Test history:
C    1  07/29/96     simont       Original version
C ******************************************************************************/
C
#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER TEST_TYPE
      INTEGER LENGTH_COUNT
      INTEGER LENGTH
      INTEGER MAX_LENGTH_COUNT
      INTEGER TEST_NUMP
      INTEGER COMM_INDEX
      INTEGER COMM_TYPE
      INTEGER TYPE_COUNT
      INTEGER MAX_TYPE_COUNT
      INTEGER COMM_COUNT
      INTEGER MAX_COMM_COUNT
      INTEGER FAIL
      INTEGER SIZE
      INTEGER LOOP_CNT
      INTEGER IERR, IERR2

      INTEGER COMM, TYPE_SIZE, BLKLENS(MPITEST_CFGSIZ)

      INTEGER NEWTYPE, NEWTYPE2, TYPES(MPITEST_CFGSIZ)

      INTEGER DISPLS(MPITEST_CFGSIZ), TYPE_SIZES(MPITEST_CFGSIZ)

      LOGICAL INTER_FLAG

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32        TESTNAME

      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING

 99   FORMAT(A, INT_FMT)
 100  FORMAT(A200)
 110  FORMAT(A)
 130  FORMAT(A, INT_FMT, A)
 140  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT)
 150  FORMAT(A, A, A, INT_FMT, A, INT_FMT)
 160  FORMAT(INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT,
     $       A, INT_FMT, A, INT_FMT, A, INT_FMT)
 170  FORMAT(A, INT_FMT, A, INT_FMT)

C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF,99) 'MPI_INIT() returned ', IERR 
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ENDIF

      TESTNAME='MPI_Type_size_types'
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
               CALL MPITEST_NUM_DATATYPES(MAX_TYPE_COUNT)

               DO 400 TYPE_COUNT = 1, MAX_TYPE_COUNT, 1
                  CALL MPITEST_GET_DATATYPE(TYPE_COUNT, TEST_TYPE)

                  TYPES(TYPE_COUNT) = MPITEST_MPI_DATATYPES(TEST_TYPE)

                  IF (TYPE_COUNT .EQ. 0) THEN
                     DISPLS(TYPE_COUNT) = 0
                  ELSE
                     DISPLS(TYPE_COUNT) = DISPLS(TYPE_COUNT - 1) +
     $                  TYPE_SIZES(TYPE_COUNT - 1)
                  END IF

                  BLKLENS(TYPE_COUNT) = 1

                  CALL MPI_TYPE_EXTENT(TYPES(TYPE_COUNT),
     $               TYPE_SIZES(TYPE_COUNT), IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                     WRITE(INFOBUF, 99)
     $                  'MPI_TYPE_EXTENT() returned ', IERR
                     CALL MPITEST_MESSAGE(
     $                  MPITEST_NONFATAL, INFOBUF)
                     ERRORSTRING=' '
                     CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                  SIZE, IERR2)
                     WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                     CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                  END IF
 400           CONTINUE

               CALL MPI_TYPE_STRUCT(MAX_TYPE_COUNT, BLKLENS, DISPLS,
     $            TYPES, NEWTYPE, IERR)
               IF (IERR .NE. MPI_SUCCESS) THEN
                  WRITE(INFOBUF, 99)
     $               'MPI_TYPE_STRUCT() returned ', IERR
                  CALL MPITEST_MESSAGE(
     $               MPITEST_NONFATAL, INFOBUF)
                  ERRORSTRING=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $               SIZE, IERR2)
                  WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                  CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
               END IF

               CALL MPI_TYPE_COMMIT(NEWTYPE, IERR)
               IF (IERR .NE. MPI_SUCCESS) THEN
                  WRITE(INFOBUF, 99)
     $               'MPI_TYPE_COMMIT() returned ', IERR
                  CALL MPITEST_MESSAGE(
     $               MPITEST_NONFATAL, INFOBUF)
                  ERRORSTRING=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $               SIZE, IERR2)
                  WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                  CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
               END IF

               CALL MPITEST_NUM_MESSAGE_LENGTHS(MAX_LENGTH_COUNT)

               DO 500 LENGTH_COUNT = 1, MAX_LENGTH_COUNT, 1
                  CALL MPITEST_GET_MESSAGE_LENGTH(LENGTH_COUNT,
     $               LENGTH)

                  IF (LENGTH .EQ. 0) THEN
                     WRITE(INFOBUF, 110) 'Skipping length = 0'
                     CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
                  ELSE

C                    Print an informational message
C
                     IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                        WRITE(INFOBUF, 140) '(', COMM_COUNT,
     $                     ') commsize ', TEST_NUMP, ' commtype: ',
     $                     COMM_TYPE
                        CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                     INFOBUF)
                     END IF

C                    Make yet another datatype
C
                     CALL MPI_TYPE_CONTIGUOUS(LENGTH, NEWTYPE,
     $                  NEWTYPE2, IERR)
                     IF (IERR .NE. MPI_SUCCESS) THEN
                        WRITE(INFOBUF, 99)
     $                     'MPI_TYPE_CONTIGUOUS() returned ', IERR
                        CALL MPITEST_MESSAGE(
     $                     MPITEST_NONFATAL, INFOBUF)
                        ERRORSTRING=' '
                        CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                     SIZE, IERR2)
                        WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                     END IF

                     CALL MPI_TYPE_COMMIT(NEWTYPE2, IERR)
                     IF (IERR .NE. MPI_SUCCESS) THEN
                        WRITE(INFOBUF, 99)
     $                     'MPI_TYPE_COMMIT() returned ', IERR
                        CALL MPITEST_MESSAGE(
     $                     MPITEST_NONFATAL, INFOBUF)
                        ERRORSTRING=' '
                        CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                     SIZE, IERR2)
                        WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                     END IF

                     WRITE(INFOBUF, 110)
     $                  'Calling MPI_TYPE_SIZE on uer datatype'
                     CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

                     CALL MPI_TYPE_SIZE(NEWTYPE2,
     $                  TYPE_SIZE, IERR)
                     IF (IERR .NE. MPI_SUCCESS) THEN
                        WRITE(INFOBUF, 99)
     $                     'MPI_TYPE_SIZE() returned ', IERR
                        CALL MPITEST_MESSAGE(
     $                     MPITEST_NONFATAL, INFOBUF)
                        ERRORSTRING=' '
                        CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                     SIZE, IERR2)
                        WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                     ELSE IF (TYPE_SIZE .LT. 0) THEN
                        FAIL = FAIL + 1
                        WRITE(INFOBUF, 99)
     $                     'MPI_TYPE_SIZE() returned negative size ',
     $                     TYPE_SIZE
                        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                     ELSE
                        WRITE(INFOBUF, 170) '(mpi type = ', NEWTYPE2,
     $                     ') -> size = ', TYPE_SIZE
                        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
                     END IF

                     LOOP_CNT = LOOP_CNT + 1

                     CALL MPI_TYPE_FREE(NEWTYPE2, IERR)
                     IF (IERR .NE. MPI_SUCCESS) THEN
                        WRITE(INFOBUF, 99)
     $                     'MPI_TYPE_FREE() returned ', IERR
                        CALL MPITEST_MESSAGE(
     $                     MPITEST_NONFATAL, INFOBUF)
                        ERRORSTRING=' '
                        CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                     SIZE, IERR2)
                        WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                     END IF
                  END IF
 500           CONTINUE
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
