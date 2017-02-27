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
C                   Test for MPI_Request_free()
C 
C This test verifies that MPI_Request_free can be used on each of the basic
C persistent calls.  Requests are expected to complete even though they are
C freed.
C 
C This test uses the first 2 ranks in MPI_COMM_WORLD, with rank 0 making
C the non-blocking and free calls and rank1 as the destination.
C **********************************************************************
#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER    FAIL
      INTEGER    LOOP_CNT
C                               counts total number of failures, loops
      INTEGER    IERR
      INTEGER    ERR
      INTEGER    SIZE
C                               return values from MPI calls
      INTEGER    I
      INTEGER    ERRORS

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

      INTEGER   ISENDBUF
      INTEGER   ISSENDBUF
      INTEGER   IRSENDBUF
      INTEGER   IBSENDBUF
      INTEGER   RECVBUF(5)

      MPITEST_BUF_TYPE VALUE

      CHARACTER*(2000+MPI_BSEND_OVERHEAD) BUFFER

      INTEGER   RECV_STAT(MPI_STATUS_SIZE,5)
      INTEGER   SEND_STAT(MPI_STATUS_SIZE)

      INTEGER   RECV_REQ(5)
      INTEGER   SEND_REQ

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
      TESTNAME = 'MPI_Request_free_p'
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
          CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
          CALL MPI_BUFFER_ATTACH(BUFFER, 2000+MPI_BSEND_OVERHEAD, IERR)

          LOOP_CNT = LOOP_CNT + 1
          CALL MPI_SEND_INIT(ISENDBUF, 1, MPI_INTEGER, 1, 1,
     $           MPI_COMM_WORLD, SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_SEND_INIT() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_START(SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_START(SEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF


          CALL MPI_REQUEST_FREE(SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_REQUEST_FREE(SEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          IF (SEND_REQ .NE. MPI_REQUEST_NULL) THEN
            INFOBUF='Request not set to MPI_REQUEST_NULL (ISEND) '
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF

 
          LOOP_CNT = LOOP_CNT + 1
          CALL MPI_BSEND_INIT(IBSENDBUF, 1, MPI_INTEGER, 1, 2,
     $           MPI_COMM_WORLD, SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_BSEND_INIT() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_START(SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_START(BSEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_REQUEST_FREE(SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_REQUEST_FREE(BSEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          IF (SEND_REQ .NE. MPI_REQUEST_NULL) THEN
            INFOBUF='Request not set to MPI_REQUEST_NULL (IBSEND) '
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF

 
          LOOP_CNT = LOOP_CNT + 1
          CALL MPI_SSEND_INIT(ISSENDBUF, 1, MPI_INTEGER, 1, 3,
     $           MPI_COMM_WORLD, SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_SSEND_INIT() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_START(SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_START(SSEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_REQUEST_FREE(SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_REQUEST_FREE(SSEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          IF (SEND_REQ .NE. MPI_REQUEST_NULL) THEN
            INFOBUF='Request not set to MPI_REQUEST_NULL (ISSEND) '
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF

 
          LOOP_CNT = LOOP_CNT + 1
          CALL MPI_RSEND_INIT(IRSENDBUF, 1, MPI_INTEGER, 1, 4,
     $           MPI_COMM_WORLD, SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_RSEND_INIT() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_START(SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_START(RSEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_REQUEST_FREE(SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_REQUEST_FREE(RSEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          IF (SEND_REQ .NE. MPI_REQUEST_NULL) THEN
            INFOBUF='Request not set to MPI_REQUEST_NULL (IRSEND) '
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF

 
          LOOP_CNT = LOOP_CNT + 1
          CALL MPI_ISEND(ISENDBUF, 1, MPI_INTEGER, 1, 5,
     $           MPI_COMM_WORLD, SEND_REQ, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_ISEND() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_WAIT(SEND_REQ, SEND_STAT, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_WAIT(ISEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

        ELSE
C
C         Receiver
C
          DO 100 I=1, 4
            LOOP_CNT = 1 + LOOP_CNT
            CALL MPI_IRECV(RECVBUF(I), 1, MPI_INTEGER, 0, I,
     $         MPI_COMM_WORLD, RECV_REQ(I), IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,99) 'MPI_IRECV() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            END IF
 100      CONTINUE

          CALL MPI_RECV_INIT(RECVBUF(5), 1, MPI_INTEGER, 0, 5,
     $        MPI_COMM_WORLD, RECV_REQ(5), IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_RECV_INIT() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF

          CALL MPI_START(RECV_REQ(5), IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_START(RECV) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF

          CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

          CALL MPI_WAITALL(4, RECV_REQ, RECV_STAT, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_WAITALL() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF

          CALL MPI_REQUEST_FREE(RECV_REQ(5), IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99)
     $        'MPI_REQUEST_FREE(RECV) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF


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

