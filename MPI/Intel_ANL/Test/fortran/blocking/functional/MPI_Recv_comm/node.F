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
*                          Functional test for MPI_Recv
*
*  This test verifies MPI_Recv sorts based on the communicator
*  specified.  3 communicators are created, and an assortment of messages
*  are sent from rank 0 to rank 1, using the first 2 communicators.
*
*  Rank 1 starts a receive on comm 3 (which should not be satisfied until
*  the end), then the receives specified above.  Messages should be 
*  received in the order sent for each communicator, not in the order
*  the receives were posted (which is different).
C **********************************************************************
#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER    NUMMESG
      PARAMETER (NUMMESG = 20)
C                               number of messages to Xsend/Recv

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

      INTEGER    COMM(64)
C                               additional communicators to sort on
C                               0 = MPI_COMM_WORLD, 2-3 are DUPed
C                               No messages will be sent on comm3
C                               until all others have been received.

      INTEGER    I
      INTEGER    J
      INTEGER    ERRORS

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
      CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

      CHARACTER*20       SENDBUF(3*NUMMESG)
      CHARACTER*20       RECVBUF(3*NUMMESG+1)
      INTEGER    NUMELM
      PARAMETER (NUMELM = 20)
C                               number of elements to send/receive
C                               MUST MATCH CHARACTER LENGTH ABOVE!


      CHARACTER*(NUMMESG * (8*NUMELM+MPI_BSEND_OVERHEAD+100)) BSEND_BUFF

      INTEGER   RECV_STAT(MPI_STATUS_SIZE)
      INTEGER   SEND_STAT(MPI_STATUS_SIZE)

      INTEGER   RECV_REQ

 90   FORMAT(I2, A13, I2)
 97   FORMAT(A,INT_FMT,A,A)
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
      TESTNAME = 'MPI_Recv_comm'
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

C
C     Create some communicators.  Use the first 3 to send some messages.
C     Leave the last one idle until the end.
C
      COMM(1) = MPI_COMM_WORLD

      DO 50 I = 2, 64
        CALL MPI_COMM_DUP(MPI_COMM_WORLD, COMM(I), IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          INFOBUF=' '
          WRITE(INFOBUF,99) 'MPI_COMM_DUP() returned', IERR
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          INFOBUF=' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          FAIL = FAIL + 1
        END IF
 50   CONTINUE

      CALL MPI_BARRIER(COMM(1), IERR)

      IF (MPITEST_ME .LT. 2) THEN

C       Initialize Send Buffers
        DO 100 I=1, NUMMESG*3
          WRITE(SENDBUF(I),90) I, ' this buffer ', I
 100    CONTINUE

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

          J = 0
          DO 200 I=1, NUMMESG
            J = J + 1
            IF (J .EQ. 3) J = 1
            LOOP_CNT = LOOP_CNT + 1
            CALL MPI_SEND(SENDBUF(3*I-2), NUMELM, MPI_CHARACTER, 1,
     $           1, COMM(J), IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_SEND() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF


            J = J + 1
            IF (J .EQ. 3) J = 1
            LOOP_CNT = LOOP_CNT + 1
            CALL MPI_BSEND(SENDBUF(3*I-1), NUMELM, MPI_CHARACTER,
     $            1, 1, COMM(J), IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_BSEND() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF


            J = J + 1
            IF (J .EQ. 3) J = 1
            LOOP_CNT = LOOP_CNT + 1
            CALL MPI_SSEND(SENDBUF(3*I), NUMELM, MPI_CHARACTER,
     $            1, 1, COMM(J), IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_SSEND() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF

 200      CONTINUE

C
C         Wait before sending the final message on the last comm.
C
          CALL MPI_BARRIER(COMM(3), IERR)
          CALL MPI_SEND(SENDBUF(1), NUMELM, MPI_CHARACTER, 1,
     $           1, COMM(3), IERR)
          LOOP_CNT = LOOP_CNT + 1
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_SEND returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_BUFFER_DETACH(BSEND_BUFF, SIZE, IERR) 


        ELSE
C
C         Receiver
C
          LOOP_CNT = 1 + LOOP_CNT
          CALL MPI_IRECV(RECVBUF(3*NUMMESG+1), NUMELM,
     $        MPI_CHARACTER, 0, 1, COMM(3), RECV_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_IRECV() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF

          J = 0
          DO 300 I=1, 3*NUMMESG
            J = J + 1
            IF (J .EQ. 3) J = 1
            RECVBUF(I)='                    '

            LOOP_CNT = 1 + LOOP_CNT
            CALL MPI_RECV(RECVBUF(I), NUMELM, MPI_CHARACTER, 0, 1,
     $         COMM(J), RECV_STAT, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,99) 'MPI_IRECV() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            END IF
 300      CONTINUE

          DO 400 I=1, 3*NUMMESG
            IF (SENDBUF(I)(1:20) .NE. RECVBUF(I)(1:20)) THEN
              INFOBUF=' '
              WRITE(INFOBUF,97)
     $         'Unexpected data in recv', I, ' = ' ,
     $         RECVBUF(I)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF

            CALL MPI_GET_COUNT(RECV_STAT, MPI_CHARACTER, CNT_LEN,
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

            IF (RECV_STAT(MPI_TAG) .NE. 1) THEN
              INFOBUF=' '
              WRITE(INFOBUF,98)
     $         'Incorrect tag in recv ',I,' = ', RECV_STAT(MPI_TAG)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF

 400      CONTINUE

C
C         Make sure we haven't received anything on COMM(3).
C
          CALL MPI_TEST(RECV_REQ, FLAG, RECV_STAT,
     $        IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_TEST() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF
          IF (FLAG .NEQV. .FALSE.) THEN
            INFOBUF='Received Message on COMM3, but none were sent!'
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

C
C         OK, receive the last message.
C
          CALL MPI_BARRIER(COMM(3), IERR)
          CALL MPI_WAIT(RECV_REQ, RECV_STAT, IERR)


        END IF
      ELSE
C
C       Ranks >= 2 need to match BARRIERs above
C
        CALL MPI_BARRIER(COMM(3), IERR)

      END IF

      DO 550 I = 2, 3
        CALL MPI_COMM_FREE(COMM(I), IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          INFOBUF=' '
          WRITE(INFOBUF,99) 'MPI_COMM_FREE() returned', IERR
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          INFOBUF=' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          FAIL = FAIL + 1
        END IF
 550  CONTINUE

C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END

