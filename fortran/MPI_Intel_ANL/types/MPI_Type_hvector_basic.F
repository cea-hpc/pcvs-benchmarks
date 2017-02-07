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
C                           Test for MPI_TYPE_HVECTOR()
C 
C All rank will call MPI_TYPE_HVECTOR() and then MPI_TYPE_COMMIT() to create
C a new user defined type using basic type defined in the types array in config.h.
C The root rank will then send a message using the new type to the receiving rank.
C The receiveing rank will do a recv.  The received buffer will be verified.
C Depending on whether MPITEST_STATUS_CHK is #defined or not, the output status
C object from MPI_RECV() may ot may not be verified. 
C 
C The input count to MPI_TYPE_HVECTOR() is determined by each message length
C in config.h and the input oldtype to MPI_TYPE_HVECTOR() is determined
C by the input types in config.h.
C 
C This test may be run in any communicator with a minimum of 2 group members,
C with any data type, and with any non-negative message length.
C 
C The MPITEST environment provides looping over communicator size,
C message length and the root's rank.  The properties of the loops are
C encoded in configuration arrays in the file config.h .
C 
C MPI Calls dependencies for this test:
C   MPI_SEND(), MPI_RECV(), MPI_CANCEL(), MPI_INIT(), MPI_FINALIZE()
C   MPI_COMM_TEST_INTER(), MPI_ERROR_STRING(), MPI_TYPE_EXTENT(),
C   MPI_TYPE_HVECTOR(), MPI_TYPE_COMMIT(),
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
      INTEGER LENGTH_COUNT
      INTEGER LENGTH
      INTEGER BYTE_LENGTH
      INTEGER MAX_LENGTH_COUNT
      INTEGER TEST_NUMP
      INTEGER COMM_INDEX
      INTEGER COMM_TYPE
      INTEGER TYPE_COUNT
      INTEGER MAX_TYPE_COUNT
      INTEGER COMM_COUNT
      INTEGER MAX_COMM_COUNT
      INTEGER ERROR, ERROR2
      INTEGER FAIL
      INTEGER SIZE
      INTEGER LOOP_CNT
      INTEGER IERR, IERR2
      INTEGER MAX_LENGTH
      INTEGER ROOT
      INTEGER TAG
      INTEGER DEST
      MPITEST_AINT EXTENT


      INTEGER MPITEST_TAG 
      PARAMETER(MPITEST_TAG = 1) 

      INTEGER COMM, STATUS(MPI_STATUS_SIZE), NEWTYPE

      LOGICAL INTER_FLAG

#ifdef MPITEST_STATUS_CHK
      INTEGER COUNT
      INTEGER EXPECTED_COUNT
#endif

      MPITEST_BUF_TYPE VALUE
      MPITEST_BUF_TYPE BUFFER(MAX_BUFF_SIZE+1)

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

C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF,99) 'MPI_INIT() returned ', IERR 
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ENDIF

      TESTNAME='MPI_Type_hvector_basic'
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

                  CALL MPI_TYPE_EXTENT(
     $               MPITEST_MPI_DATATYPES(TEST_TYPE), EXTENT,
     $                  IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                     WRITE(INFOBUF,99) 
     $                  'MPI_TYPE_EXTENT() returned ',
     $                  IERR 
                     CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                  INFOBUF)
                     ERRORSTRING=' '
                     CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                  SIZE, IERR2)
                     WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                  INFOBUF)
                  END IF

C                 Found out the number of message length in
C                 config array
C
                  CALL MPITEST_NUM_MESSAGE_LENGTHS(MAX_LENGTH_COUNT)

C                 Looping throught each length input
C
                  DO 500 LENGTH_COUNT = 1, MAX_LENGTH_COUNT, 1
                     CALL MPITEST_GET_MESSAGE_LENGTH(LENGTH_COUNT,
     $                  LENGTH)

C                    Skipping too large length
C
                     IF (MPITEST_BUF_EXTENT * MAX_BUFF_SIZE .LT.
     $                   EXTENT * LENGTH) THEN

C                       Make sure there is enough space in the buffer
C
                        WRITE(INFOBUF, 130)
     $                  'Skipping too large length = ', LENGTH,
     $                  ' for buffer space'
                     ELSE
C                       Create a user defined datatype 
C
C                        print *,' length = ',length,'extent = ',extent
                        CALL MPI_TYPE_HVECTOR(LENGTH, 1, EXTENT, 
     $                  MPITEST_MPI_DATATYPES(TEST_TYPE), NEWTYPE,
     $                     IERR)
                        IF (IERR .NE. MPI_SUCCESS) THEN
                           WRITE(INFOBUF,99) 
     $                        'MPI_TYPE_HVECTOR() returned ',
     $                        IERR 
                           CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                        INFOBUF)
                           ERRORSTRING=' '
                           CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                        SIZE, IERR2)
                           WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                           CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                        INFOBUF)
                        END IF

                        CALL MPI_TYPE_COMMIT(NEWTYPE, IERR)
                        IF (IERR .NE. MPI_SUCCESS) THEN
                           WRITE(INFOBUF,99)
     $                        'MPI_TYPE_COMMIT() returned ', IERR 
                           CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                        INFOBUF)
                           ERRORSTRING=' '
                           CALL MPI_ERROR_STRING(IERR,
     $                        ERRORSTRING, SIZE, IERR2)
                           WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                           CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                        INFOBUF)
                        END IF

C                       Now test the newly created type in data transmission
C
                        DO 600 ROOT = 0, TEST_NUMP - 1, 1

C                          Print an informational message
C
                           IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                              WRITE(INFOBUF, 140) '(', LENGTH_COUNT,
     $                        ', ', COMM_COUNT, ') length ', LENGTH
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)
                           END IF

C                          Set up datatemplate for initialize buffer
C
                           CALL MPITEST_DATATEMPLATE_INIT(VALUE,
     $                        MPITEST_CURRENT_RANK, TEST_TYPE)

C                          Initialize buffer
C
                           CALL MPITEST_INIT_BUFFER(TEST_TYPE,
     $                        LENGTH + 1, VALUE, BUFFER)

                           LOOP_CNT = LOOP_CNT + 1

                           TAG = MPITEST_TAG

C                          Destination rank
C
                           DEST = ROOT + 1
                           IF (DEST .GE. TEST_NUMP) THEN
                              DEST = 0
                           END IF

                           IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
                              WRITE(INFOBUF, 140)
     $                           'Sending from source: ', ROOT,
     $                           ' to ', DEST, ', tag: ', TAG
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)

                              CALL MPI_SEND(BUFFER, 1, NEWTYPE, DEST,
     $                           TAG, COMM, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 WRITE(INFOBUF, 99)
     $                              'MPI_SEND() returned ', IERR
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                                 ERRORSTRING=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              ERRORSTRING, SIZE, IERR2)
                                 WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                                 CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                              INFOBUF)
                              END IF
                           ELSE IF (MPITEST_CURRENT_RANK .EQ. DEST)
     $                         THEN
C                             Receive data from root's rank
C
                              WRITE(INFOBUF, 140)
     $                           'Receiving from source: ', ROOT,
     $                           ' to ', DEST, ', tag: ', TAG
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)

                              CALL MPI_RECV(BUFFER, 1, NEWTYPE, ROOT,
     $                           TAG, COMM, STATUS, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 WRITE(INFOBUF, 99)
     $                              'MPI_RECV() returned ', IERR
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                                 ERRORSTRING=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              ERRORSTRING, SIZE, IERR2)
                                 WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                                 CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                              INFOBUF)
                              END IF

#ifdef MPITEST_STATUS_CHK
C                             Check status(MPI_SOURCE)
C
                              WRITE(INFOBUF, 110)
     $                           'Verifying output status object'
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)

                              IF (STATUS(MPI_SOURCE) .NE. ROOT) THEN
                                 FAIL = FAIL + 1
                                 WRITE(INFOBUF, 150)
     $                              'status object returned from ',
     $                              'MPI_RECV() has unexpected ',
     $                              'MPI_SOURCE field Expected: ',
     $                              ROOT, ', Actual: ',
     $                              STATUS(MPI_SOURCE)
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                              END IF

C                             Check STATUS(MPI_TAG)
C
                              IF (STATUS(MPI_TAG) .NE. TAG) THEN
                                 FAIL = FAIL + 1
                                 WRITE(INFOBUF, 150)
     $                              'status object returned from ',
     $                              'MPI_RECV() has unexpected ',
     $                              'MPI_SOURCE field Expected: ',
     $                              TAG, ', Actual: ',
     $                              STATUS(MPI_TAG)
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                              END IF

C                             Not checking STATUS(MPI_ERROR)
C

C                             Checking the length of the message received
C
                              IF (LENGTH .NE. 0) THEN
                                 EXPECTED_COUNT = 1
                              ELSE
                                 EXPECTED_COUNT = 0
                              END IF

                              CALL MPI_GET_COUNT(STATUS, NEWTYPE,
     $                           COUNT, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 WRITE(INFOBUF, 99)
     $                              'MPI_GET_COUNT() returned ',
     $                              IERR
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                                 ERRORSTRING=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              ERRORSTRING, SIZE, IERR2)
                                 WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                                 CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                              INFOBUF)
                              END IF

                              IF (COUNT .NE. EXPECTED_COUNT) THEN
                                 FAIL = FAIL + 1
                                 WRITE(INFOBUF, 150)
     $                              'status object returned from ',
     $                              'MPI_RECV() contains ',
     $                              'unexpected length  Expected:',
     $                              EXPECTED_COUNT, ', Actual: ',
     $                              COUNT
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                              END IF
#else
                              WRITE(INFOBUF, 110)
     $                           'output status object *not* verified'
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                           INFOBUF)
#endif

C                             Verify the received buffer
C
                              WRITE(INFOBUF, 110)
     $                           'Verifying data received'
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_INFO1, INFOBUF)

C                             Expecting to receive the root rank number
C
                              CALL MPITEST_DATATEMPLATE_INIT(
     $                           VALUE, ROOT, TEST_TYPE)
                              CALL MPITEST_BUFFER_ERRORS(
     $                           TEST_TYPE, LENGTH, VALUE,
     $                           BUFFER, ERROR)

C                             Check for receive buffer overflow
C
                              CALL MPITEST_DATATEMPLATE_INIT(
     $                           VALUE, MPITEST_CURRENT_RANK,
     $                           TEST_TYPE)
                              CALL MPITEST_BUFFER_ERRORS_OV(
     $                           TEST_TYPE, LENGTH, VALUE,
     $                           BUFFER, ERROR2)

                              IF (ERROR + ERROR2 .NE. 0) THEN
                                 IF (IERR .EQ. MPI_SUCCESS) THEN
                                    FAIL = FAIL + 1
                                 END IF
                                 WRITE(INFOBUF, 160) ERROR + ERROR2,
     $                              ' errors in buffer (',
     $                              LENGTH_COUNT, ' , ',
     $                              COMM_COUNT, ') len ', LENGTH,
     $                              ' commsize ', TEST_NUMP,
     $                              ' commtype ', COMM_TYPE,
     $                              ' data_type ', TEST_TYPE,
     $                              ' root ', ROOT
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                              ELSE
                                 WRITE(INFOBUF, 110)
     $                              '0 error found in buffer'
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_INFO2,
     $                                 INFOBUF)
                              END IF
                           END IF
 600                    CONTINUE

C                       Free user defined type
C
                        CALL MPI_TYPE_FREE(NEWTYPE, IERR)
                        IF (IERR .NE. MPI_SUCCESS) THEN
                           WRITE(INFOBUF,99)
     $                        'MPI_TYPE_FREE() returned ', IERR 
                           CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                        INFOBUF)
                           ERRORSTRING=' '
                           CALL MPI_ERROR_STRING(IERR,
     $                        ERRORSTRING, SIZE, IERR2)
                           WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                           CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                        INFOBUF)
                        END IF
                     END IF
 500              CONTINUE
 400           CONTINUE
            END IF
         END IF
1000        CONTINUE

         CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
 300  CONTINUE

C     Report overall result
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
