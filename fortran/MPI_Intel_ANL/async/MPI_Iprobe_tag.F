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
C                           Test for MPI_IPROBE()
C
C The root rank will submit Isend() of various tag to all other ranks.
C Each rank (except the root rank) will do a iprobe() with MPI_ANY_TAG
C followed by various tag used.  Depending on whether MPITEST_BUFFER_RECV
C and or MPITEST_BUFFER_CHK is / are defined or not, a corresponding recv()
C may be issued and the recv buffer may be checked.
C 
C This test may be run in any communicator with a minimum of 2 group members.
C The test only send 1 MPI_INT in each request.
C 
C The MPITEST environment provides looping over communicator size.
C The properties of the loops are encoded in configuration arrays in the
C file mpitest_cfgf.h .
C 
C MPI Calls dependencies for this test:
C   MPI_ISEND(), MPI_IRECV(), MPI_CANCEL(), MPI_IPROBE(), MPI_INIT(), MPI_FINALIZE()
C   MPI_COMM_TEST_INTER(), MPI_BARRIER(), MPI_ERROR_STRING(), MPI_ATTR_GET(),
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

C     The destination rank
      INTEGER DEST

C     Message tag
      INTEGER TAG

C     MPI_TAG_UB's value
      INTEGER ATTR_UB

C     Maximum tag used
      INTEGER MAXTAG

C     Status object returned from MPI call
      INTEGER STATUS(MPI_STATUS_SIZE)

C     Maximum number of rank participating in test
      INTEGER MAXNP
      INTEGER I, J

C     Logical flag
      LOGICAL FLAG, FOUND

C  Maximum number of processes to participate in message sending   
C  This is here so that the test can be run in arbitray number     
C  of MPI processess without potentially exhausting message buffer 
C  as the number of mpi processes created increase.                 
      INTEGER MPITEST_MAX_NP 
      PARAMETER (MPITEST_MAX_NP = 5)

      INTEGER MPITEST_MAX_TAG 
      PARAMETER (MPITEST_MAX_TAG = 5)

C     Request handle
      INTEGER RECV_REQUEST(MPITEST_MAX_TAG)
      INTEGER SEND_REQUEST(MPITEST_MAX_NP, MPITEST_MAX_TAG)

#ifdef MPITEST_STATUS_CHK
      INTEGER COUNT, MAX_LENGTH_COUNT
#endif

      MPITEST_BUF_TYPE VALUE

      MPITEST_BUF_TYPE BUFFER(MPITEST_MAX_NP, MPITEST_MAX_TAG+1)

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
 85   FORMAT(A, INT_FMT, A, INT_FMT, A)
 90   FORMAT(A, INT_FMT, A, INT_FMT, A)

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
      TESTNAME = 'MPI_Iprobe_tag'
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      FAIL = 0
      LOOP_CNT = 0

C     Get MPI_TAG_UB's value
C
      CALL MPI_ATTR_GET(MPI_COMM_WORLD, MPI_TAG_UB, ATTR_UB, FOUND,
     $   IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF, 10)
     $      'MPITEST_COMM_TEST_INTER() returned ', IERR
         CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
         INFOBUF=' '
         CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
      END IF

      IF (FOUND .EQV. .FALSE.) THEN
         WRITE(INFOBUF, 20) 'Cannot find a value for key MPI_TAG_UB'
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ELSE IF (ATTR_UB .LT. MPITEST_TAG_UB_MIN) THEN
         WRITE(INFOBUF, 90) 'Attribute MPI_TAG_UB (', ATTR_UB,
     $      'is less than the required minimum (', MPITEST_TAG_UB_MIN,
     $      ')'
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      IF (MPITEST_MAX_TAG .GT. ATTR_UB) THEN
         MAXTAG = ATTR_UB
      ELSE
         MAXTAG = MPITEST_MAX_TAG
      END IF

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

                  LENGTH_COUNT = 1
                  LENGTH = 1
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
                        DO 900 J=1, MAXNP, 1
                           DO 950 I=1, MAXTAG, 1
                              CALL MPITEST_INIT_BUFFER(TEST_TYPE,
     $                          LENGTH+1, VALUE, BUFFER(J, I))
 950                       CONTINUE
 900                    CONTINUE

                        LOOP_CNT = LOOP_CNT + 1

                        IF ((MPITEST_CURRENT_RANK .NE. ROOT) .AND.
     $                       (MPITEST_CURRENT_RANK .LT. MAXNP)) THEN
C                          Call MPI_IPROBE() with MPI_ANY_TAG first
C
                           WRITE(INFOBUF, 85)
     $                        'Probing source: ', ROOT,
     $                        ' tag: MPI_ANY_TAG (', MPI_ANY_TAG,
     $                        ')'
                           CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                             INFOBUF)

 111                       CALL MPI_IPROBE(ROOT, MPI_ANY_TAG,
     $                                    COMM, FLAG, STATUS, IERR)
                           IF (FLAG .NEQV. .TRUE.) GO TO 111

                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10)
     $                             'MPI_IPROBE() returned ', IERR
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

                           IF (STATUS(MPI_SOURCE) .NE. ROOT) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 70) 'STATUS(MPI_SOURCE)',
     $                             ' from MPI_IPROBE() is unexpected',
     $                             ' Expected: ', ROOT, ' Actual: ',
     $                             STATUS(MPI_SOURCE)
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                INFOBUF) 
                           END IF

C                          Check STATUS(MPI_TAG)
C
                           IF (STATUS(MPI_TAG) .LT. 0) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 30) 'STATUS(MPI_TAG)',
     $                             ' from MPI_IPROBE() is negative (',
     $                             STATUS(MPI_TAG), ')'
                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                INFOBUF) 
                           ELSE IF (STATUS(MPI_TAG) .GE.
     $                              MAXTAG) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 30) 'STATUS(MPI_TAG)',
     $                             ' from MPI_IPROBE() is too large (',
     $                             STATUS(MPI_TAG), ')'
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
     $                             'MPI_IPROBE() has unexpected length',
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

                           DO 400 TAG=MAXTAG - 1, 0, -1
                              WRITE(INFOBUF, 60)
     $                           'Probing source ', ROOT,
     $                           ' tag: ', TAG
                              CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                                INFOBUF)

 222                          CALL MPI_IPROBE(ROOT, TAG, COMM,
     $                                FLAG, STATUS, IERR)
                              IF (FLAG .NEQV. .TRUE.) GO TO 222

                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 FAIL = FAIL + 1
                                 WRITE(INFOBUF, 10) 
     $                              'MPI_GET_COUNT() returned ',
     $                              IERR
                                 CALL MPITEST_MESSAGE(
     $                                   MPITEST_NONFATAL,
     $                                   INFOBUF)
                                 INFOBUF=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                                   INFOBUF,
     $                                   SIZE, IERR2)

                                 CALL MPITEST_MESSAGE(
     $                                   MPITEST_FATAL,
     $                                   INFOBUF)
                              ELSE
#ifdef MPITEST_STATUS_CHK
C                                Check status(MPI_SOURCE)
C
                                 WRITE(INFOBUF, 20)
     $                             'Verifying output status object'
                                 CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                             INFOBUF)

                                 IF (STATUS(MPI_SOURCE) .NE. 
     $                               ROOT) THEN
                                    FAIL = FAIL + 1
                                    WRITE(INFOBUF, 70)
     $                                 'STATUS(MPI_SOURCE) from ',
     $                                 'MPI_IPROBE() is unexpected',
     $                                 ' Expected: ', ROOT,
     $                                 'Actual: ',
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
     $                                 'MPI_IPROBE() is unexpected',
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
     $                                'MPI_IPROBE() has unexpected ',
     $                                'length Expected: ', LENGTH,
     $                                'Actual: ', COUNT
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
                        ELSE IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
C                          Send message to all participating ranks
C                          in communicator except myself (root)
C
                           DO 450 DEST=0, MAXNP - 1, 1
                              IF (DEST .NE. ROOT) THEN
                                 DO 460 TAG=0, MAXTAG - 1, 1
                                    WRITE(INFOBUF, 60)
     $                                 'Sending message to ',
     $                                 ROOT, ', tag: ', TAG
                                    CALL MPITEST_MESSAGE(
     $                                 MPITEST_INFO1,
     $                                 INFOBUF)

                                    CALL MPI_ISEND(
     $                                BUFFER(DEST+1, TAG+1), LENGTH,
     $                                MPITEST_MPI_DATATYPES(
     $                                TEST_TYPE), DEST, TAG, COMM,
     $                                SEND_REQUEST(DEST+1, TAG+1), IERR)
                                    IF (IERR .NE. MPI_SUCCESS) THEN
                                       FAIL = FAIL + 1
                                       WRITE(INFOBUF, 10) 
     $                                    'MPI_ISEND() returned ',
     $                                    IERR
                                       CALL MPITEST_MESSAGE(
     $                                    MPITEST_NONFATAL, INFOBUF)
                                       INFOBUF=' '
                                       CALL MPI_ERROR_STRING(IERR,
     $                                    INFOBUF, SIZE, IERR2)
                                       CALL MPITEST_MESSAGE(
     $                                     MPITEST_FATAL, INFOBUF)
                                    END IF
 460                             CONTINUE
                              END IF
 450                       CONTINUE
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
                        IF ((MPITEST_CURRENT_RANK .NE. ROOT) .AND.
     $                       (MPITEST_CURRENT_RANK .LT. MAXNP))  THEN
                           DO 500 TAG=MAXTAG - 1, 0, -1
                              WRITE(INFOBUF, 60)
     $                           'Receiving from source ',
     $                           ROOT, ' tag: ', TAG
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_INFO1, INFOBUF)

                              CALL MPI_IRECV(BUFFER(1, TAG+1), LENGTH,
     $                              MPITEST_MPI_DATATYPES
     $                                 (TEST_TYPE),
     $                              ROOT, TAG, COMM,
     $                              RECV_REQUEST(TAG+1), IERR)

                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 FAIL = FAIL + 1
                                 WRITE(INFOBUF, 10)
     $                              'MPI_IRECV() returned ',
     $                              IERR
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL,
     $                              INFOBUF)
                                 INFOBUF=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              INFOBUF, SIZE, IERR2)
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_FATAL,
     $                              INFOBUF)
                              END IF

                              CALL MPI_WAIT(RECV_REQUEST(TAG+1),
     $                           STATUS, IERR)
                              IF (IERR .NE. MPI_SUCCESS) THEN
                                 FAIL = FAIL + 1
                                 WRITE(INFOBUF, 10)
     $                              'MPI_WAIT() returned ',
     $                              IERR
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL,
     $                              INFOBUF)
                                 INFOBUF=' '
                                 CALL MPI_ERROR_STRING(IERR,
     $                              INFOBUF, SIZE, IERR2)
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_FATAL,
     $                              INFOBUF)
                              END IF

#ifdef MPITEST_BUFFER_CHK
                              WRITE(INFOBUF, 20)
     $                           'Verifying data received'
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_INFO1, INFOBUF)

C                             Expecting to receive sender's
C                             rank number
C
                              CALL MPITEST_DATATEMPLATE_INIT(
     $                           VALUE, ROOT, TEST_TYPE)

                              CALL MPITEST_BUFFER_ERRORS(
     $                           TEST_TYPE, LENGTH, VALUE,
     $                           BUFFER(1, TAG+1), ERROR)

                              IF (ERROR .NE. 0) THEN
                                 WRITE(INFOBUF, 80)
     $                              ERROR, ' errors in buffer'
                                 CALL MPITEST_MESSAGE(
     $                              MPITEST_NONFATAL,
     $                              INFOBUF)
                                 FAIL = FAIL + 1
                              END IF
#else
                              WRITE(INFOBUF, 20)
     $                           'data received *not* verified',
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_INFO1, INFOBUF)
#endif
 500                       CONTINUE
                        END IF
                        IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
C
                           DO 550 DEST=0, MAXNP - 1, 1
                              IF (DEST .NE. ROOT) THEN
                                 DO 560 TAG=0, MAXTAG - 1, 1

                                    CALL MPI_WAIT(
     $                                SEND_REQUEST(DEST+1, TAG+1), 
     $                                STATUS, IERR)
                                    IF (IERR .NE. MPI_SUCCESS) THEN
                                       FAIL = FAIL + 1
                                       WRITE(INFOBUF, 10)
     $                                   'MPI_WAIT() returned ',
     $                                   IERR
                                       CALL MPITEST_MESSAGE(
     $                                    MPITEST_NONFATAL,
     $                                    INFOBUF)
                                       INFOBUF=' '
                                       CALL MPI_ERROR_STRING(IERR,
     $                                    INFOBUF, SIZE, IERR2)
                                       CALL MPITEST_MESSAGE(
     $                                    MPITEST_FATAL, INFOBUF)
                                    END IF
 560                             CONTINUE
                              END IF
 550                       CONTINUE
                        END IF
#else
                        IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
C                          Cancel message sent from all ranks
C
                           DO 550 DEST=0, MAXNP - 1, 1
                              IF (DEST .NE. ROOT) THEN
                                 DO 560 TAG=0, MAXTAG - 1, 1
                                    WRITE(INFOBUF, 60)
     $                                 'Cancelling messge from source ',
     $                                 MPITEST_current_rank, ' tag: ',
     $                                 TAG
                                    CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                                 INFOBUF)

                                    CALL MPI_CANCEL(
     $                                SEND_REQUEST(DEST+1, TAG+1), IERR)
                                    IF (IERR .NE. MPI_SUCCESS) THEN
                                       FAIL = FAIL + 1
                                       WRITE(INFOBUF, 10)
     $                                   'MPI_CANCEL() returned ',
     $                                   IERR
                                       CALL MPITEST_MESSAGE(
     $                                    MPITEST_NONFATAL,
     $                                    INFOBUF)
                                       INFOBUF=' '
                                       CALL MPI_ERROR_STRING(IERR,
     $                                    INFOBUF, SIZE, IERR2)
                                       CALL MPITEST_MESSAGE(
     $                                    MPITEST_FATAL, INFOBUF)
                                    END IF
 560                             CONTINUE
                              END IF
 550                       CONTINUE
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


            END IF
         END IF

 800     CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM, IERR)
 100  CONTINUE

      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END

     
