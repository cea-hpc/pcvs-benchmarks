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
*                          Functional test for MPI_Test
*
*  Test references:
*
*    MPI Standard:  Section 3.7.5  Multiple Completions
*                   Section 3.7.4  Semantics of Nonblocking Communications
*                   Section 3.7.3  For MPI_Request_free
*  MPI_Request_free states that an ongoing communication associated with
*      the request will be allowed to complete, after which the request
*      will be deallocated.  After deallocation, the request becomes
*      equal to MPI_REQUEST_NULL
*
*  This test sends messages from node 0 to node 1, and uses MPI_Test
*  to check for their proper reception.  After the send the program calls
*  MPI_Request_free for two of the messages to ensure they are sent before
*  the Request Objects are freed.  This test Does a Test on messages
*  that have already been Tested on.
C **********************************************************************
#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER    NUMMESG
      PARAMETER (NUMMESG = 20)
C                               number of messages to Xsend/Irecv

      INTEGER    NUMELM
      PARAMETER (NUMELM = 10)
C                               number of elements to send/receive

      INTEGER    FAIL
      INTEGER    LOOP_CNT
C                               counts total number of failures, loops
      INTEGER    IERR
      INTEGER    ERR
      INTEGER    SIZE
C                               return values from MPI calls
      INTEGER    CNT_LEN
C                               length of received data
      LOGICAL    FLAG
C                               returned from Test

      INTEGER    I
      INTEGER    ERRORS

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

      INTEGER   SENDBUF(NUMELM, 4*NUMMESG)
      INTEGER   RECVBUF(NUMELM, 4*NUMMESG)

      MPITEST_BUF_TYPE VALUE

      CHARACTER*(NUMMESG * (8*NUMELM+MPI_BSEND_OVERHEAD+100)) BSEND_BUFF

      INTEGER   RECV_STAT(MPI_STATUS_SIZE)
      INTEGER   SEND_STAT(MPI_STATUS_SIZE)

      INTEGER   RECV_REQ(4*NUMMESG)
      INTEGER   SEND_REQ(4*NUMMESG)

 98   FORMAT(A,INT_FMT,A,INT_FMT)
 99   FORMAT(A,INT_FMT)
 189  FORMAT(A,INT_FMT,A)

C
C     Initialize the MPI environment
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

C
C     Initialize the MPITEST environment
C
      CALL MPITEST_INIT(IERR)
      TESTNAME = 'MPI_Test_p'
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF


C
C     Initialize the loop/error counters
C
      LOOP_CNT = 0
      FAIL = 0

C
C     Need at least 2 nodes.
C
      IF (MPITEST_NUMP .LT. 2) THEN
        INFOBUF = 'At least 2 ranks required to run this test'
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF


      IF (MPITEST_ME .LT. 2) THEN
        IF (MPITEST_ME .EQ. 0) THEN
C
C         Sender
C
          CALL MPI_BUFFER_ATTACH(BSEND_BUFF, 
     $        NUMMESG * (8*NUMELM + MPI_BSEND_OVERHEAD+100), IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_BUFFER_ATTACH() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

C         Initialize Send Buffers
          DO 100 I=1, NUMMESG*4
            CALL MPITEST_DATATEMPLATE_INIT(VALUE, I, MPITEST_INTEGER)
            CALL MPITEST_INIT_BUFFER(MPITEST_INTEGER, NUMELM, VALUE,
     $         SENDBUF(1, I))
 100      CONTINUE

          CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

          DO 200 I=1, NUMMESG
            LOOP_CNT = LOOP_CNT + 1
            CALL MPI_SEND_INIT(SENDBUF(1,I*4-3), NUMELM, MPI_INTEGER, 1,
     $           4*I-3, MPI_COMM_WORLD, SEND_REQ(4*I-3), IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_SEND_INIT() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF


            LOOP_CNT = LOOP_CNT + 1
            CALL MPI_BSEND_INIT(SENDBUF(1, 4*I-2), NUMELM, MPI_INTEGER,
     $            1, 4*I-2, MPI_COMM_WORLD, SEND_REQ(4*I-2), IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_BSEND_INIT() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF


            LOOP_CNT = LOOP_CNT + 1
            CALL MPI_SSEND_INIT(SENDBUF(1, 4*I-1), NUMELM, MPI_INTEGER,
     $            1, 4*I-1, MPI_COMM_WORLD, SEND_REQ(4*I-1), IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_SSEND_INIT() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF

 
            LOOP_CNT = LOOP_CNT + 1
            CALL MPI_RSEND_INIT(SENDBUF(1, 4*I), NUMELM, MPI_INTEGER, 1, 
     $           4*I, MPI_COMM_WORLD, SEND_REQ(4*I), IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_RSEND_INIT() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF
 200      CONTINUE

          LOOP_CNT = LOOP_CNT + 1
          CALL MPI_STARTALL(4*NUMMESG, SEND_REQ(1), IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_STARTALL(SEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF


          LOOP_CNT = LOOP_CNT + 1
          CALL MPI_REQUEST_FREE(SEND_REQ(5), IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_REQUEST_FREE(SEND_INIT) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          IF (SEND_REQ(5) .NE. MPI_REQUEST_NULL) THEN
            INFOBUF='Request not set to MPI_REQUEST_NULL (SEND_INIT)'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF

 
          LOOP_CNT = LOOP_CNT + 1
          CALL MPI_REQUEST_FREE(SEND_REQ(9), IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_REQUEST_FREE(SEND_INIT) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          IF (SEND_REQ(9) .NE. MPI_REQUEST_NULL) THEN
            INFOBUF='Request not set to MPI_REQUEST_NULL (SEND_INIT)'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF

          DO 250 I = 1, 4*NUMMESG
 225        CONTINUE
            CALL MPI_TEST(SEND_REQ(I), FLAG, SEND_STAT, IERR)
            IF ((IERR .EQ. MPI_SUCCESS) .AND. (FLAG .EQV. .FALSE.))
     $        GO TO 225
            LOOP_CNT = LOOP_CNT + 1
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_TEST returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF
 250      CONTINUE

          DO 260 I = 1, 4*NUMMESG
          IF (SEND_REQ(I) .NE. MPI_REQUEST_NULL) THEN
            CALL MPI_REQUEST_FREE(SEND_REQ(I), IERR)
          END IF
 260      CONTINUE

          CALL MPI_BUFFER_DETACH(BSEND_BUFF, SIZE, IERR) 


        ELSE
C
C         Receiver
C
          DO 300 I=1, 4*NUMMESG
            CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1, MPITEST_INTEGER)
            CALL MPITEST_INIT_BUFFER(MPITEST_INTEGER, NUMELM, VALUE,
     $         RECVBUF(1, I))

            LOOP_CNT = 1 + LOOP_CNT
            CALL MPI_RECV_INIT(RECVBUF(1, I), NUMELM, MPI_INTEGER, 0, I,
     $         MPI_COMM_WORLD, RECV_REQ(I), IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,99) 'MPI_RECV_INIT() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            END IF
 300      CONTINUE

          LOOP_CNT = LOOP_CNT + 1
          CALL MPI_STARTALL(4*NUMMESG, RECV_REQ(1), IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_STARTALL(RECV) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

          DO 400 I = 1, 4*NUMMESG
 350        CONTINUE
            CALL MPI_TEST(RECV_REQ(I), FLAG, RECV_STAT, IERR)
            IF ((IERR .EQ. MPI_SUCCESS) .AND. (FLAG .EQV. .FALSE.))
     $        GO TO 350
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,99) 'MPI_TEST() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            END IF

            CALL MPITEST_DATATEMPLATE_INIT(VALUE, I, MPITEST_INTEGER)
            CALL MPITEST_BUFFER_ERRORS(MPITEST_INTEGER, NUMELM, VALUE,
     $          RECVBUF(1,I), ERRORS)
            IF (ERRORS .NE. 0) THEN
              INFOBUF=' '
              WRITE(INFOBUF,98)
     $         'Unexpected data in recv', I, ' = ' ,
     $         RECVBUF(1,I)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF

            CALL MPI_GET_COUNT(RECV_STAT, MPI_INTEGER, CNT_LEN,
     $          IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,99) 'MPI_GET_COUNT() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF

            IF (NUMELM .NE. CNT_LEN) THEN
              INFOBUF=' '
              WRITE(INFOBUF,98)
     $         'Incorrect length in recv ', I, ' = ' , CNT_LEN
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF

            IF (RECV_STAT(MPI_TAG) .NE. I) THEN
              INFOBUF=' '
              WRITE(INFOBUF,98)
     $         'Incorrect tag in recv ',I,' = ', RECV_STAT(MPI_TAG)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF

 400      CONTINUE

          DO 500 I = 1, 4*NUMMESG
          IF (RECV_REQ(I) .NE. MPI_REQUEST_NULL) THEN
            CALL MPI_REQUEST_FREE(RECV_REQ(I), IERR)
          END IF
 500      CONTINUE

        END IF
      ELSE
C
C       Ranks >= 2 need to match BARRIER above
C
        CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

      END IF


C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END

