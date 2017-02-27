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
C                           Test for MPI_TYPE_FREE()
C 
C Test to verify that pending communication usin user defined type will be
C completed even after the user type has been marked for deallocation
C using MPI_TYPE_FREE().
C 
C All ranks will first do a MPI_TYPE_STRUCT() of all types in the type config
C array (in config.h).  Root's rank will then do a Isend() using the newly
C created type followed by a MPI_TYPE_FREE() of that type before any recv has
C been posted.  All participating will then synchronize and the destination rank
C will then receive and verify the data using the user datatype.
C 
C This test may be run in any communicator with a minimum of 2 group members,
C with any data type, and with any non-negative message length.
C 
C The MPITEST environment provides looping over communicator size,
C message length and the root's rank.  The properties of the loops are
C encoded in configuration arrays in the file config.h .
C 
C MPI Calls dependencies for this test:
C   MPI_ISEND(), MPI_IRECV(), MPI_INIT(), MPI_FINALIZE()
C   MPI_COMM_TEST_INTER(), MPI_ERROR_STRING(), MPI_WAIT(),
C   MPI_TYPE_STRUCT(), MPI_TYPE_COMMIT(), MPI_BARRIER(),
C   MPI_TYPE_SIZE(), MPI_TYPE_EXTENT(), MPI_TYPE_FREE(),
C   [MPI_GET_COUNT(), MPI_ALLREDUCE(), MPI_COMM_RANK(), MPI_COMM_SIZE()]
C 
C Test history:
C    1  08/09/96     simont       Original version
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
      INTEGER ERROR, ERROR2
      INTEGER FAIL
      INTEGER SIZE
      INTEGER LOOP_CNT
      INTEGER IERR, IERR2
      INTEGER ROOT
      INTEGER TAG
      INTEGER DEST
C     WDG - 4/29/03 - The original code used MPITEST_AINT, which is 
C     incorrect for the Fortran binding
      INTEGER EXTENT, XT

      INTEGER I, J, K, ACC, POS

      INTEGER MPITEST_TAG 
      PARAMETER(MPITEST_TAG = 1) 
      INTEGER MPITEST_ADD_BUFSIZE
      PARAMETER (MPITEST_ADD_BUFSIZE = 20)

C     WDG - 4/29/03 - The original code used MPITEST_AINT, which is 
C     incorrect for the Fortran binding
      INTEGER DISPLS(MPITEST_CFGSIZ)

      INTEGER COMM, STATUS(MPI_STATUS_SIZE), REQUEST

      LOGICAL INTER_FLAG

      INTEGER NEWTYPE

      INTEGER TYPES(MPITEST_CFGSIZ)

      INTEGER BLKLENS(MPITEST_CFGSIZ)

      INTEGER TYPE_SIZES(MPITEST_CFGSIZ)

      CHARACTER*(MAX_BUFF_SIZE + MPITEST_ADD_BUFSIZE) BUFFER

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32        TESTNAME

      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING

 99   FORMAT(A, INT_FMT)
 100  FORMAT(A200)
 110  FORMAT(A)
 115  FORMAT(A, A)
 120  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT,
     $       A, INT_FMT, A)
 130  FORMAT(A, INT_FMT, A)
 140  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT)
 150  FORMAT(A, A, A, INT_FMT, A, INT_FMT)
 155  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT,
     $       A, INT_FMT, A, INT_FMT, A, INT_FMT)
 160  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT,
     $       A, INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT)
 170  FORMAT(A, A, INT_FMT, A, INT_FMT, A, A)
 180  FORMAT(A, A, INT_FMT, A, A, INT_FMT, A, A, A,
     $       INT_FMT, A)
 190  FORMAT(INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT,
     $       A, INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT)
 200  FORMAT(A, INT_FMT, A, INT_FMT)

C
C     Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF,99) 'MPI_INIT() returned ', IERR 
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ENDIF

      TESTNAME='MPI_Type_free_pending'
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
      CALL MPITEST_NUM_DATATYPES(MAX_TYPE_COUNT)

C     Make sure the number of input types is not too large
C
      IF (MAX_TYPE_COUNT .GT. MPITEST_CFGSIZ) THEN
         WRITE(INFOBUF, 130) 'Input number of types too large,
     $      using the first ', MPITEST_CFGSIZ, ' types only'
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
         MAX_TYPE_COUNT = MPITEST_CFGSIZ
      END IF

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
               DO 400 TYPE_COUNT = 1, MAX_TYPE_COUNT, 1
                  CALL MPITEST_GET_DATATYPE(TYPE_COUNT,
     $               TEST_TYPE)
                  TYPES(TYPE_COUNT) =
     $               MPITEST_MPI_DATATYPES(TEST_TYPE)

                  IF (TYPE_COUNT .EQ. 1) THEN
                     DISPLS(TYPE_COUNT) = 0
                  ELSE
                     DISPLS(TYPE_COUNT) = DISPLS(TYPE_COUNT - 1)
     $                  + TYPE_SIZES(TYPE_COUNT-1)
                  END IF

                  BLKLENS(TYPE_COUNT) = 1

                  CALL MPI_TYPE_SIZE(TYPES(TYPE_COUNT),
     $               TYPE_SIZES(TYPE_COUNT), IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                     WRITE(INFOBUF,99) 
     $                  'MPI_TYPE_SIZE() returned ', IERR 
                     CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                     ERRORSTRING=' '
                     CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                  SIZE, IERR2)
                     WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                     CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                  END IF

                  CALL MPI_TYPE_EXTENT(TYPES(TYPE_COUNT),
     $               XT, IERR)
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
 400           CONTINUE

               IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                  DO 450 TYPE_COUNT = 1, MAX_TYPE_COUNT, 1
                     WRITE(INFOBUF, 160) 'blklens(',
     $                  TYPE_COUNT, ') = ',
     $                  BLKLENS(TYPE_COUNT),
     $                  ', displs(', TYPE_COUNT,
     $                  ') = ', DISPLS(TYPE_COUNT),
     $                  ', types(', TYPE_COUNT,
     $                  ') = ', TYPES(TYPE_COUNT),
     $                  ', type_sizes(', TYPE_COUNT,
     $                  ') = ', TYPE_SIZES(TYPE_COUNT)
                     CALL MPITEST_MESSAGE(MPITEST_INFO2,
     $                  INFOBUF) 
 450              CONTINUE
               END IF

C              Found out the number of message length in
C              config array
C
               CALL MPITEST_NUM_MESSAGE_LENGTHS(MAX_LENGTH_COUNT)

C              Looping throught each length input
C
               DO 500 LENGTH_COUNT = 1, MAX_LENGTH_COUNT, 1
                  CALL MPITEST_GET_MESSAGE_LENGTH(LENGTH_COUNT,
     $               LENGTH)

C              Skipping length 0
C
                  IF (LENGTH .EQ. 0) THEN
                     IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                        WRITE(INFOBUF, 110) 'Skipping length = 0'
                        CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                     INFOBUF)
                     END IF
                  ELSE
C                    Now test the newly created type in data transmission
C
                     DO 600 ROOT = 0, TEST_NUMP - 1, 1

                        CALL MPI_TYPE_STRUCT(MAX_TYPE_COUNT, BLKLENS,
     $                     DISPLS, TYPES, NEWTYPE, IERR)
                        IF (IERR .NE. MPI_SUCCESS) THEN
                           WRITE(INFOBUF,99) 
     $                        'MPI_TYPE_STRUCT() returned ', IERR 
                           CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                        INFOBUF)
                           ERRORSTRING=' '
                           CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                        SIZE, IERR2)
                           WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                           CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                        END IF

                        CALL MPI_TYPE_COMMIT(NEWTYPE, IERR)
                        IF (IERR .NE. MPI_SUCCESS) THEN
                           WRITE(INFOBUF,99)
     $                        'MPI_TYPE_COMMIT() returned ', IERR 
                           CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                        INFOBUF)
                           ERRORSTRING=' '
                           CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                        SIZE, IERR2)
                           WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                           CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                        END IF

                        CALL MPI_TYPE_EXTENT(NEWTYPE, EXTENT, IERR)
                        IF (IERR .NE. MPI_SUCCESS) THEN
                           WRITE(INFOBUF,99) 
     $                        'MPI_TYPE_EXTENT() returned ', IERR 
                           CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                        INFOBUF)
                           ERRORSTRING=' '
                           CALL MPI_ERROR_STRING(IERR, ERRORSTRING,
     $                        SIZE, IERR2)
                           WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                           CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                        END IF

C                       Make sure there is enough space in the buffer
C
                        IF (EXTENT * LENGTH .GT. MAX_BUFF_SIZE) THEN
                           WRITE(INFOBUF, 120) 'Skipping length = ',
     $                        LENGTH,
     $                        ' (type extent * length = ', EXTENT,
     $                        ' * ', LENGTH, ' = ', EXTENT * LENGTH,
     $                        ', Maximum buffer size for this type (',
     $                        MAX_BUFF_SIZE, ')'
                           CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
                        ELSE

C                          Print an informational message
C
                           IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                              WRITE(INFOBUF, 155) '(', LENGTH_COUNT,
     $                           ', ', COMM_COUNT, ') length ', LENGTH,
     $                           ' commsize ', TEST_NUMP, ' commtype ',
     $                           COMM_TYPE, ' extent ', EXTENT,
     $                           ' root ', ROOT
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)
                           END IF

C                          Initialize send & recv buffer
C
                           IF (MPITEST_CURRENT_RANK .NE. ROOT) THEN
                              DO 650 I = 1, LENGTH * EXTENT +
     $                           MPITEST_ADD_BUFSIZE, 1
                                 BUFFER(I:I) = 'A'
 650                          CONTINUE
                           ELSE
C                             Root's rank
C
                              DO 670 I = 1, LENGTH, 1

C                                Initialize each block
C
                                 ACC = 0

                                 DO 680 J = 1, MAX_TYPE_COUNT, 1
C                                   Initialize each basic type
C
                                    DO 690 K = 1, TYPE_SIZES(J), 1
C                                      Use TYPE_COUNT as send data
C
                                       POS = (I-1) * EXTENT + ACC + K
                                       BUFFER(POS:POS) = CHAR(J)
 690                                CONTINUE

                                    ACC = ACC + TYPE_SIZES(J)
 680                             CONTINUE
 670                          CONTINUE

C                             Also initialize the unused data buffer as well
C
                              DO 700 I = LENGTH * EXTENT + 1,
     $                           LENGTH * EXTENT +
     $                           MPITEST_ADD_BUFSIZE, 1
                                 BUFFER(I:I) = 'B'
 700                          CONTINUE
                           END IF

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
#ifdef MPITEST_DISP_BUF
                              WRITE(INFOBUF, 110)
     $                           'Displaying Senders data buffer:'
                              CALL MPITEST_MESSAGE(MPITEST_INFO2,
     $                           INFOBUF)

                              DO 710 I = 1, LENGTH * EXTENT +
     $                           MPITEST_ADD_BUFSIZE, 1
                                 WRITE(INFOBUF, 200) 'buffer(', I,
     $                              ') = ', ICHAR(BUFFER(I:I))
                                 CALL MPITEST_MESSAGE(MPITEST_INFO2,
     $                              INFOBUF)
 710                          CONTINUE
#endif
                              CALL MPI_ISEND(BUFFER, LENGTH, NEWTYPE,
     $                           DEST, TAG, COMM, REQUEST, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 WRITE(INFOBUF, 99)
     $                              'MPI_ISEND() returned ', IERR
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                                 ERRORSTRING=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              ERRORSTRING, SIZE, IERR2)
                                 WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                                 CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                              INFOBUF)
                              END IF

                              WRITE(INFOBUF, 110)
     $                           'Freeing user defined type'
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)

                              CALL MPI_TYPE_FREE(NEWTYPE, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 WRITE(INFOBUF,99)
     $                              'MPI_TYPE_FREE() returned ', IERR 
                                 CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                              INFOBUF)
                                 ERRORSTRING=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              ERRORSTRING, SIZE, IERR2)
                                 WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                                 CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                              INFOBUF)
                              ELSE IF (NEWTYPE .NE. MPI_DATATYPE_NULL)
     $                              THEN
                                 FAIL = FAIL + 1
                                 WRITE(INFOBUF, 115)
     $                              'type is not set to MPI_DATATYPE',
     $                              '_NULL by MPI_TYPE_FREE()'
                                 CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                              INFOBUF)
                              END IF
                           END IF

C                          Synchronzing so we can be sure that the recv
C                          has not been posted yet
C
                           WRITE(INFOBUF, 110)
     $                        'Synchronzing with all ranks...'
                           CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

                           CALL MPI_BARRIER(COMM, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              WRITE(INFOBUF, 99)
     $                           'MPI_BARRIER() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL, INFOBUF)
                              ERRORSTRING=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           ERRORSTRING, SIZE, IERR2)
                              WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                              CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                           INFOBUF)
                           END IF

                           IF (MPITEST_CURRENT_RANK .EQ. DEST) THEN
C                             Receive data from root's rank
C
                              WRITE(INFOBUF, 140)
     $                           'Receiving from source: ', ROOT,
     $                           ' to ', DEST, ', tag: ', TAG
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)

                              CALL MPI_IRECV(BUFFER, LENGTH, NEWTYPE,
     $                           ROOT, TAG, COMM, REQUEST, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 WRITE(INFOBUF, 99)
     $                              'MPI_IRECV() returned ', IERR
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                                 ERRORSTRING=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              ERRORSTRING, SIZE, IERR2)
                                 WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                                 CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                              INFOBUF)
                              END IF

C                             Free user type
C
                              CALL MPI_TYPE_FREE(NEWTYPE, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 WRITE(INFOBUF, 99)
     $                              'MPI_TYPE_FREE() returned ', IERR
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                                 ERRORSTRING=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              ERRORSTRING, SIZE, IERR2)
                                 WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                                 CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                              INFOBUF)
                              END IF

C                             Wait for the recv request to be finished
C
                              WRITE(INFOBUF, 110)
     $                           'Waiting for the recv request ...'
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)

                              CALL MPI_WAIT(REQUEST, STATUS, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 WRITE(INFOBUF, 99)
     $                              'MPI_WAIT() returned ', IERR
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
     $                              'MPI_WAIT() has unexpected ',
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
     $                              'MPI_WAIT() has unexpected ',
     $                              'MPI_SOURCE field Expected: ',
     $                              TAG, ', Actual: ',
     $                              STATUS(MPI_TAG)
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL, INFOBUF)
                              END IF

C                             Not checking STATUS(MPI_ERROR)
C
C                             Also not checking the receive length
C                             since the user type has been freed
C

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

                              ERROR = 0
                              ERROR2 = 0

                              DO 800 I = 1, LENGTH, 1
C                                Verify each block
C
                                 ACC = 0
                                 DO 810 J = 1, MAX_TYPE_COUNT, 1
C                                   Verify each basic type
C
                                    DO 820 K = 1, TYPE_SIZES(J), 1
                                       POS = (I-1) * EXTENT + ACC + K
                                       IF (BUFFER(POS:POS) .NE.
     $                                    CHAR(J)) THEN
                                          FAIL = FAIL + 1
                                          ERROR = ERROR + 1
#ifdef MPITEST_1ST_ERR
                                          IF (ERROR .LE. 1) THEN
#endif
                                             WRITE(INFOBUF, 170)
     $                                          'Unexpected received',
     $                                          ' buffer(', POS,
     $                                          '), Expected: ',
     $                                          J, ', Actual: ',
     $                                          ICHAR(BUFFER(POS:POS))

                                             CALL MPITEST_MESSAGE(
     $                                          MPITEST_NONFATAL,
     $                                          INFOBUF)
#ifdef MPITEST_1ST_ERR
                                          END IF
#endif
                                       END IF
 820                                CONTINUE
                                    ACC = ACC + TYPE_SIZES(J)

C                                   Do not verify space between inner types
C
 810                             CONTINUE
 800                          CONTINUE

C                             Verify that receiver did not receive more date
C                             than expected
C
                              IF (BUFFER(LENGTH*EXTENT+1:
     $                                   LENGTH*EXTENT+1)
     $                           .NE. 'A') THEN
                                 FAIL = FAIL + 1
                                 ERROR2 = ERROR2 + 1
#ifdef MPITEST_1ST_ERR
                                 IF (ERROR .LE. 1) THEN
#endif
C                                   Writing out the ICHAR output as well
C                                   in case the character received is
C                                   invisible
C
                                    WRITE(INFOBUF, 180)
     $                                 'Received buffer overflow, ',
     $                                 'Expected buffer(',
     $                                 LENGTH * EXTENT + 1, '): A',
     $                                 ' Actual buffer(', 
     $                                 LENGTH * EXTENT + 1, '): ',
     $                                 BUFFER(LENGTH * EXTENT + 1:
     $                                        LENGTH * EXTENT + 1),
     $                                 ' (ICHAR = ',
     $                                 ICHAR(BUFFER(LENGTH*EXTENT+1:
     $                                    LENGTH*EXTENT+1)), ')'
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_NONFATAL, INFOBUF)
#ifdef MPITEST_1ST_ERR
                                 END IF
#endif
                              END IF

                              IF (ERROR + ERROR2 .NE. 0) THEN
                                 IF (IERR .EQ. MPI_SUCCESS) THEN
                                    FAIL = FAIL + 1
                                 END IF
#ifdef MPITEST_DISP_BUF
                                 WRITE(INFOBUF, 110)
     $                              'Displaying received data buffer:'
                                 CALL MPITEST_MESSAGE(MPITEST_INFO2,
     $                              INFOBUF)

                                 DO 900 I = 1, LENGTH * EXTENT +
     $                              MPITEST_ADD_BUFSIZE, 1
                                    WRITE(INFOBUF, 200)
     $                                 'buffer(', I, ') = ',
     $                                 ICHAR(BUFFER(I:I))
                                    CALL MPITEST_MESSAGE(MPITEST_INFO2,
     $                                 INFOBUF)
 900                             CONTINUE
#endif
                                 WRITE(INFOBUF, 190) ERROR + ERROR2,
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
     $                              MPITEST_INFO2,
     $                              INFOBUF)
                              END IF
                           END IF

                           IF ((MPITEST_CURRENT_RANK .NE. ROOT) .AND.
     $                         (MPITEST_CURRENT_RANK .NE. DEST)) THEN
C                             Free newly created datatype
C
                              WRITE(INFOBUF, 110)
     $                           'Freeing user defined type'
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)

                              CALL MPI_TYPE_FREE(NEWTYPE, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 WRITE(INFOBUF,99)
     $                              'MPI_TYPE_FREE() returned ', IERR 
                                 CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                              INFOBUF)
                                 ERRORSTRING=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              ERRORSTRING, SIZE, IERR2)
                                 WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                                 CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                              INFOBUF)
                              ELSE IF (NEWTYPE .NE. MPI_DATATYPE_NULL)
     $                              THEN
                                 FAIL = FAIL + 1
                                 WRITE(INFOBUF, 115)
     $                              'type is not set to MPI_DATATYPE_',
     $                              'NULL by MPI_TYPE_FREE()'
                                 CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                              INFOBUF)
                              END IF
                           END IF
                        END IF
#ifdef MPITEST_SYNC
                        CALL MPI_BARRIER(COMM, IERR)
                        IF (IERR .NE. MPI_SUCCESS) THEN
                           WRITE(INFOBUF, 99)
     $                        'MPI_BARRIER() returned ',
     $                        IERR
                           CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                        INFOBUF)
                           ERRORSTRING=' '
                           CALL MPI_ERROR_STRING(IERR,
     $                        ERRORSTRING, SIZE, IERR2)
                           WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                           CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                        END IF
#endif
 600                 CONTINUE

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
