C -----------------------------------------------------------------------------
C MESSAGE PASSING INTERFACE TEST CASE SUITE
C 
C Copyright - 1996 Intel Corporation
C 
C Intel Corporation hereby grants a non-exclusive license under Intel's
C copyright to copy, modify and distribute this software for any purpose
C and without fee, provided that the above copyright notice and the following
C paragraphs appear on all copies.
C 
C Intel Corporation makes no representation that the test cases comprising
C this suite are correct or are an accurate representation of any standard.
C 
C IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT OR
C SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING, CONSEQUENTIAL,
C INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT LIMITED TO INFRINGEMENT,
C LOSS OF USE, BUSINESS INTERRUPTIONS, AND LOSS OF PROFITS, IRRESPECTIVE OF
C WHETHER INTEL HAS ADVANCE NOTICE OF THE POSSIBILITY OF ANY SUCH DAMAGES.
C 
C INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT
C LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
C PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED HEREUNDER
C IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO OBLIGATION TO PROVIDE
C MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS.
C -----------------------------------------------------------------------------
C *****************************************************************************
C                           Test for MPI_Cancel()
C
C The test contains 2 phases:
C 
C 1).
C The root rank will submit Isend() to the next rank (circular).
C The root rank will then do a MPI_Cancel() followed by MPI_Wait() for the
C request to be completed.  It then call MPI_Test_cancelled() to verify that
C the send request has been cancelled.  All participating ranks will then
C do a MPI_Barrier().
C 
C 2).
C If things goes well with the first phase of the test, the root rank will
C then make another send request with different data using the user buffer
C and request handle sending to the next rank (circular).  The next rank
C will do a MPI_Recv() to receive the data.  The received data will be
C verified.  Depending on whether MPITEST_STATUS_CHK is #defined or not,
C the status object returned by the MPI_Recv() may be checked.
C 
C This test may be run in any communicator with a minimum of 2 group members.
C 
C The MPITEST environment provides looping over communicator size, message
C length, and root's rank.
C 
C The properties of the loops are encoded in configuration arrays in the
C file config.h .
C 
C MPI Calls dependencies for this test:
C   MPI_ISEND(), MPI_CANCEL(), MPI_TEST_CANCELLED(), MPI_RECV(), MPI_INIT(),
C   MPI_FINALIZE(), MPI_COMM_TEST_INTER(), MPI_BARRIER(), MPI_ERROR_STRING(),
C   MPI_WAIT(), [MPI_GET_COUNT(), MPI_ALLREDUCE(), MPI_COMM_RANK(), MPI_COMM_SIZE()]
C
C Test history:
C
C   1  06/23/96     simont       Original version
C
C *****************************************************************************
C
#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

C     The index of the current buffer type
      INTEGER TEST_TYPE

C     The length of the current buffer
      INTEGER LENGTH

C     The length of the current buffer in bytes
      INTEGER BYTE_LENGTH

C     The number of process in the current communicator
      INTEGER TEST_NUMP

C     The array index of the current communicator
      INTEGER COMM_INDEX

C     The index of the current communicator type
      INTEGER COMM_TYPE

C     Loop counter for data type loop
      INTEGER TYPE_COUNT

C     Loop counter for the message length loop
      INTEGER LENGTH_COUNT

C     Loop counter for communicator loop
      INTEGER COMM_COUNT

C     The maximum of input communicator from config file 
      INTEGER MAX_COMM_COUNT

C     Communicator
      INTEGER COMM

C     Errors from one MPI call
      INTEGER ERROR

C     Counts total number of failure(s)
      INTEGER FAIL

C     Return size from MPI_Error_string
      INTEGER SIZE

C     Counts total number of loops through test
      INTEGER LOOP_CNT

C     Return value from MPI call
      INTEGER IERR, IERR2

C     Maximum bufer length specified in config file
      INTEGER MAX_LENGTH

C     Maximum buffer length in bytes
      INTEGER MAX_BYTE_LENGTH

C     The root rank
      INTEGER ROOT

C     The destination rank
      INTEGER DEST

C     Message tag
      INTEGER TAG

C     Status object returned from MPI call
      INTEGER STATUS(MPI_STATUS_SIZE)

C     Request handle
      INTEGER SEND_REQUEST

      INTEGER I, J

C     Logical flag
      LOGICAL FLAG

      INTEGER MPITEST_TAG 
      PARAMETER (MPITEST_TAG = 119)

#ifdef MPITEST_STATUS_CHK
      INTEGER COUNT, MAX_LENGTH_COUNT
#endif

      MPITEST_BUF_TYPE VALUE

C     Send buffer for test phase 1 and recv buffer for
C     test phase 2.  In some system where MPI_CANCEL() does
C     not work, MPI_RECV() may end up having to receive all
C     sent buffer.  So, we need a bigger buffer here to
C     prevent running out of address space.
C
      MPITEST_BUF_TYPE BUFFER(MAX_BUFF_SIZE*MAX_RANKS)
      MPITEST_BUF_TYPE BUFFER2(MAX_BUFF_SIZE+1)

      CHARACTER*(IOSIZE)  INFOBUF

      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 10   FORMAT(A,INT_FMT)
 20   FORMAT(A)
 25   FORMAT(A, A)
 30   FORMAT(A, A, INT_FMT, A)
 40   FORMAT(A, INT_FMT, A)
 50   FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT,
     $       A, INT_FMT, A, INT_FMT, A, INT_FMT)
 60   FORMAT(A, INT_FMT, A, INT_FMT)
 70   FORMAT(A, A, A, INT_FMT, A, INT_FMT)
 75   FORMAT(A, A, A)
 80   FORMAT(INT_FMT, A)

C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)

      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF,10) 'MPI_INIT() returned ', IERR 
         CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
         INFOBUF=' '
         CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
      END IF

      CALL MPITEST_INIT(IERR)
      TESTNAME = 'MPI_Cancel_isend'
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      TAG = MPITEST_TAG
      FAIL = 0
      LOOP_CNT = 0
C
C     Get number of communicators, data types and message lengths to test
C
      CALL MPITEST_NUM_COMM_SIZES(MAX_COMM_COUNT)

C
C     Get max message length for test
C
      CALL MPITEST_GET_MAX_MESSAGE_LENGTH(MAX_BYTE_LENGTH)

C
C     Loop through communicators
C
      DO 100 COMM_COUNT=1, MAX_COMM_COUNT, 1
         CALL MPITEST_GET_COMM_INDEX(COMM_COUNT, COMM_INDEX)
         CALL MPITEST_GET_COMM_TYPE(COMM_COUNT, COMM_TYPE)

         CALL MPITEST_GET_COMMUNICATOR(COMM_TYPE, COMM_INDEX,
     $                                 COMM, TEST_NUMP)

         IF (TEST_NUMP .LT. 2) THEN
C           Skipping communicator with comm size < 2
C
            WRITE(INFOBUF, 30) 'Skipping communicator with size < 2',
     $           ' (commtype: ', COMM_TYPE, ') for this test'
            CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
         ELSE
C           All other process(es) not in the current communicator
C           will skip this.
            IF (COMM .EQ. MPI_COMM_NULL) GO TO 800

            CALL MPI_COMM_TEST_INTER(COMM, FLAG, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
               WRITE(INFOBUF, 10)
     $              'MPITEST_COMM_TEST_INTER() returned ', IERR
               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
               INFOBUF=' '
               CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
               CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            ELSE IF (FLAG .EQV. .TRUE.) THEN
               WRITE(INFOBUF, 40)
     $              'Skipping inter communicator (commtype: ',
     $              COMM_TYPE, ') for this test'
               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            ELSE
               TEST_TYPE = MPITEST_INTEGER

C              Convert the number of bytes in the maximum length
C              message into the number of elements of the current
C              type
C
               CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE,
     $                                      MAX_BYTE_LENGTH,
     $                                      MAX_LENGTH)

#ifdef MPITEST_STATUS_CHK
               CALL MPITEST_NUM_MESSAGE_LENGTHS(MAX_LENGTH_COUNT)

               DO 200 LENGTH_COUNT=1, MAX_LENGTH_COUNT, 1
                  CALL MPITEST_GET_MESSAGE_LENGTH(LENGTH_COUNT,
     $                                            BYTE_LENGTH)
                  CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE,
     $                                         BYTE_LENGTH,
     $                                         LENGTH)
#else
                  LENGTH_COUNT = 1
                  LENGTH = 1
#endif
                  DO 300 ROOT=0, TEST_NUMP - 1, 1
C                    Print an informational message
C
                     IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                        WRITE(INFOBUF, 50) '(', LENGTH_COUNT, ', ', 
     $                       COMM_COUNT, ') length ', LENGTH,
     $                       ' commsize ', TEST_NUMP, ' commtype ',
     $                       COMM_TYPE, ' data type ', TEST_TYPE,
     $                       ' root ', ROOT
                        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
                     END IF

                     IF (MPITEST_CURRENT_RANK .NE. MPI_UNDEFINED) THEN
C                       Initialize send buffer
C
                        CALL MPITEST_DATATEMPLATE_INIT(VALUE,
     $                          MPITEST_CURRENT_RANK,
     $                          TEST_TYPE)
                        CALL MPITEST_INIT_BUFFER(TEST_TYPE,
     $                     LENGTH+1, VALUE, BUFFER)

C                       Destination rank the message is being sent to
C
                        DEST = ROOT + 1
                        IF (DEST .GE. TEST_NUMP) THEN
                           DEST = 0
                        END IF

                        IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
                           WRITE(INFOBUF, 60)
     $                        'Sending message to ',
     $                        DEST, ', tag: ', TAG
                           CALL MPITEST_MESSAGE(
     $                        MPITEST_INFO1,
     $                        INFOBUF)

                           CALL MPI_ISEND(BUFFER, LENGTH,
     $                        MPITEST_MPI_DATATYPES(
     $                        TEST_TYPE), DEST, TAG, COMM,
     $                        SEND_REQUEST, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10) 
     $                           'MPI_ISEND() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_NONFATAL, INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_FATAL, INFOBUF)
                           END IF

C                          Cancelling the request
C
                           WRITE(INFOBUF, 20)
     $                       'Cancelling the request ...'
                           CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                             INFOBUF)

                           CALL MPI_CANCEL(SEND_REQUEST, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10) 
     $                           'MPI_CANCEL() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_NONFATAL, INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_FATAL, INFOBUF)
                           END IF

C                          Wait for the send request to be cancelled
C
                           LOOP_CNT = LOOP_CNT + 1
                           WRITE(INFOBUF, 20)
     $                       'Wait for the send request to be cancelled'
                           CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                             INFOBUF)

                           CALL MPI_WAIT(SEND_REQUEST, STATUS, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10) 
     $                           'MPI_WAIT() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_NONFATAL, INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_FATAL, INFOBUF)
                           END IF

C                          Make sure the send request has been
C                          successfully cancelled
C
                           WRITE(INFOBUF, 25)
     $                       'Checking if the send request has ',
     $                       'been cancelled'
                           CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                             INFOBUF)

                           CALL MPI_TEST_CANCELLED(STATUS, FLAG, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10) 
     $                           'MPI_TEST_CANCELLED() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_NONFATAL, INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_FATAL, INFOBUF)
                           END IF

                           IF (FLAG .NEQV. .TRUE.) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 75) 'MPI_TEST_CANCELLED()',
     $                           ' returned .FALSE. FLAG with a ',
     $                           'cancelled requested'
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                           INFOBUF)
                           END IF
                        END IF

                        CALL MPI_BARRIER(COMM, IERR)
                        IF (IERR .NE. MPI_SUCCESS) THEN
                           WRITE(INFOBUF,10)
     $                        'MPI_BARRIER() returned ', IERR 
                           CALL MPITEST_MESSAGE(
     $                        MPITEST_NONFATAL, INFOBUF)
                           INFOBUF=' '
                           CALL MPI_ERROR_STRING(IERR,
     $                        INFOBUF, SIZE, IERR2)
                           CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                        INFOBUF)
                        END IF

                        CALL MPITEST_DATATEMPLATE_INIT(VALUE,
     $                          -1, TEST_TYPE)

                        IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
C                          Initialize send buffer
C
                           CALL MPITEST_INIT_BUFFER(TEST_TYPE,
     $                        LENGTH+1, VALUE, BUFFER2)

C                          Reuse the send request handle for
C                          another transmission
C
                           WRITE(INFOBUF, 60)
     $                        'Sending message 2 to ',
     $                        DEST, ', tag: ', TAG
                           CALL MPITEST_MESSAGE(
     $                        MPITEST_INFO1,
     $                        INFOBUF)

                           CALL MPI_ISEND(BUFFER2, LENGTH,
     $                        MPITEST_MPI_DATATYPES(
     $                        TEST_TYPE), DEST, TAG, COMM,
     $                        SEND_REQUEST, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10) 
     $                           'MPI_ISEND() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_NONFATAL, INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_FATAL, INFOBUF)
                           END IF

                           LOOP_CNT = LOOP_CNT + 1
                           CALL MPI_WAIT(SEND_REQUEST, STATUS, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10) 
     $                           'MPI_WAIT() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_NONFATAL, INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                            MPITEST_FATAL, INFOBUF)
                           END IF

                        ELSE IF (MPITEST_CURRENT_RANK .EQ. DEST) THEN
                           WRITE(INFOBUF, 60)
     $                        'Receiving message from ',
     $                        ROOT, ', tag: ', TAG
                           CALL MPITEST_MESSAGE(
     $                        MPITEST_INFO1,
     $                        INFOBUF)

C                           CALL MPITEST_DATATEMPLATE_INIT(
C     $                        VALUE, -1,
C     $                        TEST_TYPE)
CC
C                           CALL MPITEST_INIT_BUFFER(TEST_TYPE,
C     $                        LENGTH+1, VALUE, BUFFER2)

                           LOOP_CNT = LOOP_CNT + 1
                           CALL MPI_RECV(BUFFER, MAX_LENGTH,
     $                        MPITEST_MPI_DATATYPES(TEST_TYPE),
     $                        ROOT, TAG, COMM, STATUS, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10)
     $                           'MPI_RECV() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL,
     $                           INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_FATAL,
     $                           INFOBUF)
                           END IF

#ifdef MPITEST_STATUS_CHK
C                          Check status(MPI_SOURCE)
C
                           WRITE(INFOBUF, 20)
     $                        'Verifying output status object'
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                           INFOBUF)

                           IF (STATUS(MPI_SOURCE) .NE. 
     $                         ROOT) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 70)
     $                           'STATUS(MPI_SOURCE) from ',
     $                           'MPI_RECV() is unexpected',
     $                           ' Expected: ', ROOT,
     $                           ' Actual: ',
     $                           STATUS(MPI_SOURCE)
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL,
     $                           INFOBUF)
                           END IF

C                          Check STATUS(MPI_TAG)
C
                           IF (STATUS(MPI_TAG) .NE. TAG) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 70)
     $                           'STATUS(MPI_TAG) from ',
     $                           'MPI_RECV() is unexpected',
     $                           ' Expected: ', TAG, ' Actual: ',
     $                           STATUS(MPI_TAG)
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL,
     $                           INFOBUF)
                           END IF

C                          Not checking STATUS(MPI_ERROR)
C

C                          Checking length of message to be
C                          received
C

                           CALL MPI_GET_COUNT(STATUS,
     $                        MPITEST_MPI_DATATYPES(TEST_TYPE),
     $                        COUNT, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10)
     $                           'MPI_GET_COUNT() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL,
     $                           INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_FATAL,
     $                           INFOBUF)
                           END IF

                           IF (COUNT .NE. LENGTH) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 70) 'STATUS from ',
     $                           'MPI_RECV() has unexpected ',
     $                           ' length Expected: ', LENGTH,
     $                           ' Actual: ', COUNT
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL,
     $                           INFOBUF)
                           END IF
#else
                           WRITE(INFOBUF, 20)
     $                        'Output STATUS *not* verified'
                           CALL MPITEST_MESSAGE(
     $                        MPITEST_INFO1,
     $                        INFOBUF)
#endif

                           WRITE(INFOBUF, 20)
     $                        'Verifying data received'
                           CALL MPITEST_MESSAGE(
     $                        MPITEST_INFO1, INFOBUF)

                           CALL MPITEST_DATATEMPLATE_INIT(
     $                        VALUE, -1, TEST_TYPE)

                           CALL MPITEST_BUFFER_ERRORS(
     $                        TEST_TYPE, LENGTH, VALUE,
     $                        BUFFER, ERROR)

                           IF (ERROR .NE. 0) THEN
                              WRITE(INFOBUF, 80)
     $                           ERROR, ' errors in buffer'
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL,
     $                           INFOBUF)
                              FAIL = FAIL + 1
                           END IF
                        END IF

                        CALL MPI_BARRIER(COMM, IERR)
                        IF (IERR .NE. MPI_SUCCESS) THEN
                           FAIL = FAIL + 1
                           WRITE(INFOBUF, 10)
     $                        'MPI_BARRIER() returned ',
     $                        IERR
                           CALL MPITEST_MESSAGE(
     $                        MPITEST_NONFATAL,
     $                        INFOBUF)
                           INFOBUF=' '
                           CALL MPI_ERROR_STRING(IERR,
     $                        INFOBUF, SIZE, IERR2)
                           CALL MPITEST_MESSAGE(
     $                        MPITEST_FATAL, INFOBUF)
                        END IF
                     END IF
 300              CONTINUE

#ifdef MPITEST_STATUS_CHK
 200           CONTINUE
#endif

            END IF
         END IF

 800     CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM, IERR)
 100  CONTINUE

      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END

     
