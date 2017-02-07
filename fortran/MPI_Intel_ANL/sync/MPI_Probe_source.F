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
C                           Test for MPI_Probe()
C Each rank (except the root rank) will do a send to the root rank.  The root
C rank will then do a MPI_Probe() followed by a MPI_Recv().  The buffer
C received may or may not be send and / or checked (depending on if
C MPITEST_BUFFER_RECV and / or MPITEST_BUFFER_CHK is / are #defined or not in
C this source).  The status object returned from MPI_Probe() may or may not
C be checked depending on if MPITEST_STATUS_CHK is defined or not.
C
C This test may be run in any communicator with a minimum of 2 group members,
C with any data type, and with any non-negative message length.
C 
C The MPITEST environment provides looping over communicator size and
C message length (if MPITEST_STATUS_CHK is #defined)  The properties of the
C loops are encoded in configuration arrays in the file config.h .
C
C MPI Calls dependencies for this test:
C   MPI_ISEND(), MPI_RECV(), MPI_CANCEL(), MPI_PROBE(), MPI_INIT(), MPI_FINALIZE()
C   MPI_COMM_TEST_INTER(), MPI_BARRIER(), MPI_ERROR_STRING(), 
C   [MPI_GET_COUNT(), MPI_ALLREDUCE(), MPI_COMM_RANK(), MPI_COMM_SIZE()]
C
C
C Test history:
C
C   1  06/20/96     simont       Original version
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

C     The source rank
      INTEGER SOURCE

C     Message tag
      INTEGER TAG

C     Status object returned from MPI call
      INTEGER STATUS(MPI_STATUS_SIZE)

C     Request handle
      INTEGER REQUEST

C     Maximum number of rank participating in test
      INTEGER MAXNP
      INTEGER I, J

C     Logical flag
      LOGICAL FLAG

      INTEGER MPITEST_TAG  
      PARAMETER (MPITEST_TAG = 119)
 
C  Maximum number of processes to participate in message sending
C  This is here so that the test can be run in arbitray number 
C  of MPI processess without potentially exhausting message buffer 
C  as the number of mpi processes created increase. 
      INTEGER MPITEST_MAX_NP 
      PARAMETER (MPITEST_MAX_NP = 10)

#ifdef MPITEST_STATUS_CHK
      INTEGER COUNT, MAX_LENGTH_COUNT
#endif

      MPITEST_BUF_TYPE VALUE

      MPITEST_BUF_TYPE BUFFER(MAX_BUFF_SIZE+1)

      CHARACTER*(IOSIZE)  INFOBUF

      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 10   FORMAT(A,INT_FMT)
 20   FORMAT(A)
 30   FORMAT(A, A, INT_FMT, A)
 40   FORMAT(A, INT_FMT, A)
 50   FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT,
     $       A, INT_FMT, A, INT_FMT, A, INT_FMT)
 60   FORMAT(A, INT_FMT, A, INT_FMT)
 70   FORMAT(A, A, A, INT_FMT, A, INT_FMT)
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
      TESTNAME = 'MPI_Probe_source'
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

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

C        A measure to prevent having message flood the message buffer
C        when test scales up, all ranks larger than MPITEST_MAX_NP
C        would not participate in message sending.  MPITEST_MAX_NP
C        can be customized if needed at the top of this source
C
         IF (TEST_NUMP .GT. MPITEST_MAX_NP) THEN
            MAXNP = MPITEST_MAX_NP
         ELSE
            MAXNP = TEST_NUMP
         END IF

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
                  DO 300 ROOT=0, MAXNP - 1, 1
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
                        CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH+1,
     $                          VALUE, BUFFER)

                        LOOP_CNT = LOOP_CNT + 1

                        TAG = MPITEST_TAG

                        IF ((MPITEST_CURRENT_RANK .NE. ROOT) .AND.
     $                      (MPITEST_CURRENT_RANK .LT. MAXNP)) THEN
                           WRITE(INFOBUF, 60) 'Sending message to ',
     $                          ROOT, ', tag: ', TAG
                           CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

                           CALL MPI_ISEND(BUFFER, LENGTH,
     $                             MPITEST_MPI_DATATYPES(TEST_TYPE),
     $                             ROOT, TAG, COMM, REQUEST, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10) 
     $                             'MPI_ISEND() returned ', IERR
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                             INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR, INFOBUF,
     $                                              SIZE, IERR2)
                              CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                                             INFOBUF)
                           END IF
                        ELSE IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
C                          Call MPI_PROBE() with MPI_ANY_SOURCE first
C
                           WRITE(INFOBUF, 10)
     $                        'Probing source MPI_ANY_SOURCE tag: ',
     $                        TAG
                           CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                             INFOBUF)

                           CALL MPI_PROBE(MPI_ANY_SOURCE, TAG,
     $                                    COMM, STATUS, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10)
     $                             'MPI_PROBE() returned ', IERR
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                             INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR, INFOBUF,
     $                                              SIZE, IERR2)
                              CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                                             INFOBUF)
                           END IF

#ifdef MPITEST_STATUS_CHK
C                          Check status(MPI_SOURCE)
C
                           WRITE(INFOBUF, 20)
     $                          'Verifying output status object'
                           CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                                          INFOBUF)

                           IF (STATUS(MPI_SOURCE) .LT. 0) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 30)
     $                           'STATUS(MPI_SOURCE) from ',
     $                           'MPI_PROBE() is negative (',
     $                           STATUS(MPI_SOURCE), ')'
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                INFOBUF)
                           ELSE IF (STATUS(MPI_SOURCE) .GE.
     $                              TEST_NUMP) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 30) 'STATUS(MPI_SOURCE)',
     $                             ' from MPI_PROBE() is too large (',
     $                             STATUS(MPI_SOURCE), ')'
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                INFOBUF)
                           END IF

C                          Check STATUS(MPI_TAG)
C
                           IF (STATUS(MPI_TAG) .NE. TAG) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 70) 'STATUS(MPI_TAG)',
     $                             ' from MPI_PROBE() is unexpected',
     $                             ' Expected: ', TAG, ' Actual: ',
     $                             STATUS(MPI_TAG)
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                INFOBUF) 
                           END IF

C                          Not checkin STATUS(MPI_ERROR)
C

C                          Checking length of message to be received
C

                           CALL MPI_GET_COUNT(STATUS,
     $                             MPITEST_MPI_DATATYPES(TEST_TYPE),
     $                             COUNT, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10)
     $                             'MPI_GET_COUNT() returned ', IERR
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                             INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR, INFOBUF,
     $                                              SIZE, IERR2)
                              CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                                             INFOBUF)
                           END IF

                           IF (COUNT .NE. LENGTH) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 70) 'STATUS from ',
     $                             'MPI_PROBE() has unexpected length',
     $                             ' Expected: ', LENGTH, ' Actual: ',
     $                             COUNT
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                INFOBUF)
                           END IF
#else
                           WRITE(INFOBUF, 20)
     $                          'Output STATUS *not* verified'
                           CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                             INFOBUF)
#endif

                           DO 400 SOURCE=0, MAXNP - 1, 1
                              IF (SOURCE .NE. ROOT) THEN
                                 WRITE(INFOBUF, 60)
     $                              'Probing source ', SOURCE,
     $                              ' tag: ', TAG
                                 CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                                   INFOBUF)

                                 CALL MPI_PROBE(SOURCE, TAG, COMM,
     $                                   STATUS, IERR)

                                 IF (IERR .NE. MPI_SUCCESS) THEN
                                    FAIL = FAIL + 1
                                    WRITE(INFOBUF, 10) 
     $                                'MPI_GET_COUNT() returned ',
     $                                IERR
                                    CALL MPITEST_MESSAGE(
     $                                      MPITEST_NONFATAL,
     $                                      INFOBUF)
                                    INFOBUF=' '
                                    CALL MPI_ERROR_STRING(IERR,
     $                                      INFOBUF,
     $                                      SIZE, IERR2)

                                    CALL MPITEST_MESSAGE(
     $                                      MPITEST_FATAL,
     $                                      INFOBUF)
                                 END IF


#ifdef MPITEST_STATUS_CHK
C                                Check status(MPI_SOURCE)
C
                                 WRITE(INFOBUF, 20)
     $                             'Verifying output status object'
                                 CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                             INFOBUF)

                                 IF (STATUS(MPI_SOURCE) .NE. 
     $                               SOURCE) THEN
                                    FAIL = FAIL + 1
                                    WRITE(INFOBUF, 70)
     $                                 'STATUS(MPI_SOURCE) from ',
     $                                 'MPI_PROBE() is unexpected',
     $                                 ' Expected: ', SOURCE,
     $                                 ' Actual: ',
     $                                 STATUS(MPI_SOURCE)
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_NONFATAL,
     $                                 INFOBUF)
                                 END IF

C                                Check STATUS(MPI_TAG)
C
                                 IF (STATUS(MPI_TAG) .NE. TAG) THEN
                                    FAIL = FAIL + 1
                                    WRITE(INFOBUF, 70)
     $                                 'STATUS(MPI_TAG) from ',
     $                                 'MPI_PROBE() is unexpected',
     $                                 ' Expected: ', TAG, ' Actual: ',
     $                                 STATUS(MPI_TAG)
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_NONFATAL,
     $                                 INFOBUF)
                                 END IF

C                                Not checkin STATUS(MPI_ERROR)
C

C                                Checking length of message to be
C                                received
C

                                 CALL MPI_GET_COUNT(STATUS,
     $                              MPITEST_MPI_DATATYPES(TEST_TYPE),
     $                              COUNT, IERR)
                                 IF (IERR .NE. MPI_SUCCESS) THEN
                                    FAIL = FAIL + 1
                                    WRITE(INFOBUF, 10)
     $                                'MPI_GET_COUNT() returned ',
     $                                IERR
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_NONFATAL,
     $                                 INFOBUF)
                                    INFOBUF=' '
                                    CALL MPI_ERROR_STRING(IERR,
     $                                 INFOBUF, SIZE, IERR2)
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_FATAL,
     $                                 INFOBUF)
                                 END IF

                                 IF (COUNT .NE. LENGTH) THEN
                                    FAIL = FAIL + 1
                                    WRITE(INFOBUF, 70) 'STATUS from ',
     $                                'MPI_PROBE() has unexpected ',
     $                                ' length Expected: ', LENGTH,
     $                                ' Actual: ', COUNT
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_NONFATAL,
     $                                 INFOBUF)
                                 END IF
#else
                                 WRITE(INFOBUF, 20)
     $                             'Output STATUS *not* verified'
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONINFO1,
     $                              INFOBUF)
#endif

                              END IF
 400                       CONTINUE
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

#ifdef MPITEST_BUFFER_RECV
                        IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
                           DO 500 SOURCE=0, MAXNP - 1, 1
                              IF (SOURCE .NE. ROOT) THEN
                                 WRITE(INFOBUF, 60)
     $                              'Receiving from source ',
     $                              SOURCE, ' tag: ', TAG
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_INFO1, INFOBUF)

                                 CALL MPI_RECV(BUFFER, LENGTH,
     $                              MPITEST_MPI_DATATYPES
     $                                 (TEST_TYPE),
     $                              SOURCE, TAG, COMM, STATUS, IERR)

                                 IF (IERR .NE. MPI_SUCCESS) THEN
                                    FAIL = FAIL + 1
                                    WRITE(INFOBUF, 10)
     $                                 'MPI_RECV() returned ',
     $                                 IERR
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_NONFATAL,
     $                                 INFOBUF)
                                    INFOBUF=' '
                                    CALL MPI_ERROR_STRING(IERR,
     $                                 INFOBUF, SIZE, IERR2)
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_FATAL,
     $                                 INFOBUF)
                                 END IF

#ifdef MPITEST_BUFFER_CHK
                                 WRITE(INFOBUF, 20)
     $                              'Verifying data received'
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_INFO1, INFOBUF)

C                                Expecting to receive rank #
C                                from each rank
C
                                 CALL MPITEST_DATATEMPLATE_INIT(
     $                              VALUE, SOURCE, TEST_TYPE)

                                 CALL MPITEST_BUFFER_ERRORS(
     $                              TEST_TYPE, LENGTH, VALUE,
     $                              BUFFER, ERROR)

C                                Check for receive buffer overflow
C
                                 CALL MPITEST_DATATEMPLATE_INIT(
     $                              VALUE, MPITEST_CURRENT_RANK,
     $                              TEST_TYPE)

                                 IF (ERROR .NE. 0) THEN
                                    WRITE(INFOBUF, 80)
     $                                 ERROR, ' errors in buffer'
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_NONFATAL,
     $                                 INFOBUF)
                                    FAIL = FAIL + 1
                                 END IF
#else
                                 WRITE(INFOBUF, 20)
     $                              'data received *not* verified',
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_INFO1, INFOBUF)
#endif
                              END IF
 500                       CONTINUE
                        END IF
                        IF ((MPITEST_CURRENT_RANK .NE. ROOT) .AND.
     $                      (MPITEST_CURRENT_RANK .LT. MAXNP))
     $                         THEN

                           CALL MPI_WAIT(REQUEST, STATUS, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10)
     $                          'MPI_WAIT() returned ',
     $                          IERR
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL,
     $                           INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_FATAL, INFOBUF)
                           END IF
                        END IF
#else
                        IF ((MPITEST_CURRENT_RANK .NE. ROOT) .AND.
     $                      (MPITEST_CURRENT_RANK .LT. MAXNP))
     $                         THEN
C                          Cancel message sent to root from
C                          each rank
C
                           WRITE(INFOBUF, 60)
     $                        'Cancelling messge from source ',
     $                        MPITEST_current_rank, ' tag: ',
     $                        TAG
                           CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                        INFOBUF)

                           CALL MPI_CANCEL(REQUEST, IERR)
                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10)
     $                          'MPI_CANCEL() returned ',
     $                          IERR
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL,
     $                           INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF, SIZE, IERR2)
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_FATAL, INFOBUF)
                           END IF
                        END IF
#endif
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

     
