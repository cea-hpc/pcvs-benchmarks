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
C                           Test for MPI_collective_message
C 
C This test validates that collective operations do not interfere with
C ongoing message traffic.  The ranks are split into even-odd pairs (minus
C the last rank if an odd number of ranks).  Before the collective operation,
C the even-numbered rank does an Isend to the odd ranks followed by an Irecv
C from ANY_RANK/ANY_TAG. After the collective operation, The odd node Recv's the
C message, then sends a message to the even rank, while the even rank waits
C for the Irecv to complete.
C 
C There should be no interference from the multiple calls.  Incorrect data
C received or an MPI error constitutes failure.
C 
C Test history:
C    1  09/03/96     gt       Original version
C **********************************************************************

#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

C---------------------------------------------------------------------
C Pre-collective operation:
C 
C The even rank (partner = 0) Isends a message to the even rank, and posts
C an Irecv for ANY_SOURCE/ANY_TAG.
C---------------------------------------------------------------------
      SUBROUTINE PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, TAG,
     $                   REQS, REQR, RC)
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
      INTEGER    PARTNER
      INTEGER    SIDE
      INTEGER    SENDBUF(*)
      INTEGER    RECVBUF(*)
      INTEGER    TAG
      INTEGER    REQS
      INTEGER    REQR
      INTEGER    RC


      INTEGER    IERR, ERRSIZE, ERR
      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 99   FORMAT(A,INT_FMT)

      RC = 0
      SENDBUF(1) = TAG
      RECVBUF(1) = 0
      IF (PARTNER .NE. MPI_UNDEFINED) THEN
        IF (SIDE .EQ. 0) THEN
          CALL MPI_ISEND(SENDBUF, 2048, MPI_INTEGER, PARTNER,
     $             TAG, MPI_COMM_WORLD, REQS, IERR)
          IF (IERR .NE. 0) THEN
            WRITE(INFOBUF,99) 'MPI_ISEND() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            RC = RC + 1
          END IF
          CALL MPI_IRECV(RECVBUF, 2048, MPI_INTEGER, MPI_ANY_SOURCE,
     $             MPI_ANY_TAG, MPI_COMM_WORLD, REQR, IERR)
          IF (IERR .NE. 0) THEN
            WRITE(INFOBUF,99) 'MPI_IRECV() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            RC = RC + 1
          END IF
        END IF
      END IF

      END 

C---------------------------------------------------------------------
C Post-collective operation:
C 
C The even rank (partner = 0) waits for the Irecv posted earlier to complete.
C 
C The odd rank receives the message sent earlier, then sends a message to the
C even rank to saitsfy the Irecv posted earlier.
C---------------------------------------------------------------------
      SUBROUTINE POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, TAG,
     $                   REQS, REQR, RC)
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
      INTEGER    PARTNER
      INTEGER    SIDE
      INTEGER    SENDBUF(*)
      INTEGER    RECVBUF(*)
      INTEGER    TAG
      INTEGER    REQS
      INTEGER    REQR
      INTEGER    RC


      INTEGER    IERR, ERRSIZE, ERR
      INTEGER    STAT(MPI_STATUS_SIZE)
      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 99   FORMAT(A,INT_FMT)

      RC = 0
      SENDBUF(1) = TAG
      IF (PARTNER .NE. MPI_UNDEFINED) THEN
        IF (SIDE .EQ. 1) THEN
C         WDG - The following initialize was improperly placed outside 
C         of the size.eq.1 test (found by Rajeev Thakur)
          RECVBUF(1) = 0
          CALL MPI_RECV(RECVBUF, 2048, MPI_INTEGER, PARTNER,
     $             TAG, MPI_COMM_WORLD, STAT, IERR)
          IF (IERR .NE. 0) THEN
            WRITE(INFOBUF,99) 'MPI_RECV() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            RC = RC + 1
          END IF
          IF (RECVBUF(1) .NE. TAG) THEN
            WRITE(INFOBUF,99) 'MPI_RECV received incorrect data',
     $            RECVBUF(1)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            RC = RC + 1
          END IF
          CALL MPI_SEND(SENDBUF, 2048, MPI_INTEGER, PARTNER,
     $             TAG, MPI_COMM_WORLD, IERR)
          IF (IERR .NE. 0) THEN
            WRITE(INFOBUF,99) 'MPI_SEND() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            RC = RC + 1
          END IF
        ELSE
          CALL MPI_WAIT(REQR, STAT, IERR)
          IF (IERR .NE. 0) THEN
            WRITE(INFOBUF,99) 'MPI_WAIT(RECV) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            RC = RC + 1
          END IF
          IF (RECVBUF(1) .NE. TAG) THEN
            WRITE(INFOBUF,99) 'MPI_IRECV received incorrect data',
     $            RECVBUF(1)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            RC = RC + 1
          END IF
          CALL MPI_WAIT(REQS, STAT, IERR)
          IF (IERR .NE. 0) THEN
            WRITE(INFOBUF,99) 'MPI_WAIT(SEND) returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            RC = RC + 1
          END IF
        END IF
      END IF

      END

C---------------------------------------------------------------------
      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'


      INTEGER    FAIL
      INTEGER    LOOP_CNT
      INTEGER    LOOP_FAIL
C                               counts total number of failures, loops
      INTEGER    ERROR, ERROR2
C                               number of data errors found in buffer
      INTEGER    IERR
      INTEGER    ERR
      INTEGER    ERRSIZE
C                               return value from MPI calls
      INTEGER    K
C                               
      INTEGER    PARTNER
C                               partner rank for ongoing calls
      INTEGER    SIDE
C                               even or odd partner
      INTEGER    SENDBUF(2048)
      INTEGER    RECVBUF(2048)
      INTEGER    VALUE
      INTEGER    RESULT
      INTEGER    EXPECT
      INTEGER    VALUE_ARY(MAX_RANKS)
      INTEGER    VALUE_DSPLSARY(MAX_RANKS)
      INTEGER    COUNTS_ARY(MAX_RANKS)
      INTEGER    RESULT_ARY(MAX_RANKS)
      INTEGER    RESULT_DSPLSARY(MAX_RANKS)
C                                 buffers and parms for calls
      INTEGER    REQ1, REQ2
C

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 97   FORMAT(A,INT_FMT,A,INT_FMT)
 98   FORMAT(A,INT_FMT,A)
 99   FORMAT(A,INT_FMT)
 179  FORMAT(A,I6,A,I6,A,I6,A,I6,A,I3,A,INT_FMT)
 189  FORMAT(INT_FMT,A,A,I6,A,I6,A,I6,A,I6,A,I3,A,INT_FMT)

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
      TESTNAME = 'MPI_collective_message'
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      IF (MPITEST_NUMP .LT. 2) THEN
        INFOBUF = 'This test requires at least 2 ranks'
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF


C
C     Initialize the loop/error counters
C
      LOOP_CNT = 0
      FAIL = 0

      IF ((MPITEST_ME .EQ. MPITEST_NUMP-1) .AND.
     $    (MOD(MPITEST_NUMP, 2) .EQ. 1)) THEN
         PARTNER = MPI_UNDEFINED
      ELSE IF (MOD(MPITEST_ME, 2) .EQ. 0) THEN
         PARTNER = MPITEST_ME + 1
      ELSE
         PARTNER = MPITEST_ME - 1
      END IF

      SIDE = MOD(MPITEST_ME, 2)

      DO 100 K=1,MPITEST_NUMP
        VALUE_DSPLSARY(K) = K-1
        RESULT_DSPLSARY(K) = K-1
        COUNTS_ARY(K) = 1
  100 CONTINUE


C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Allgather'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      VALUE = -1
      DO 1000 K=1,MPITEST_NUMP
        RESULT_ARY(K) = 1
 1000 CONTINUE

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_ALLGATHER(VALUE, 1, MPI_INTEGER,
     $           RESULT_ARY, 1, MPI_INTEGER,
     $           MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_ALLGATHER() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      LOOP_FAIL = 0
      DO 1005 K=1,MPITEST_NUMP
        IF (RESULT_ARY(K) .NE. -1) LOOP_FAIL = LOOP_FAIL + 1
 1005 CONTINUE

      IF (LOOP_FAIL .NE. 0) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,98) 'MPI_ALLGATHER() received', LOOP_FAIL,
     $                    ' bad values'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3


C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Allgatherv'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      VALUE = -1
      DO 1020 K=1,MPITEST_NUMP
        RESULT_ARY(K) = 0
 1020 CONTINUE

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_ALLGATHERV(VALUE, 1, MPI_INTEGER,
     $           RESULT_ARY, COUNTS_ARY, RESULT_DSPLSARY, MPI_INTEGER,
     $           MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_ALLGATHERV() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      LOOP_FAIL = 0
      DO 1025 K=1,MPITEST_NUMP
        IF (RESULT_ARY(K) .NE. -1) LOOP_FAIL = LOOP_FAIL + 1
 1025 CONTINUE

      IF (LOOP_FAIL .NE. 0) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,98) 'MPI_ALLGATHERV() received', LOOP_FAIL,
     $                    ' bad values'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3


C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Allreduce'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      VALUE = -1
      RESULT = 0

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_ALLREDUCE(VALUE, RESULT, 1, MPI_INTEGER, MPI_SUM,
     $           MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_ALLREDUCE() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      IF (RESULT .NE. -1*MPITEST_NUMP) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,97) 'MPI_ALLREDUCE() received', RESULT,
     $                    ' expected', -1*MPITEST_NUMP
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3



C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Alltoall'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      DO 1040 K=1,MPITEST_NUMP
        VALUE_ARY(K) = -1
        RESULT_ARY(K) = 0
 1040 CONTINUE

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_ALLTOALL(VALUE_ARY, 1, MPI_INTEGER,
     $           RESULT_ARY, 1, MPI_INTEGER,
     $           MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_ALLTOALL() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      LOOP_FAIL = 0
      DO 1045 K=1,MPITEST_NUMP
        IF (RESULT_ARY(K) .NE. -1) LOOP_FAIL = LOOP_FAIL + 1
 1045 CONTINUE

      IF (LOOP_FAIL .NE. 0) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,98) 'MPI_ALLTOALL() received', LOOP_FAIL,
     $                    ' bad values'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3


C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Alltoallv'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      DO 1060 K=1,MPITEST_NUMP
        VALUE_ARY(K) = -1
        RESULT_ARY(K) = 0
 1060 CONTINUE

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_ALLTOALLV(VALUE_ARY, COUNTS_ARY, VALUE_DSPLSARY,
     $           MPI_INTEGER,
     $           RESULT_ARY, COUNTS_ARY, RESULT_DSPLSARY, MPI_INTEGER,
     $           MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_ALLTOALLV() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      LOOP_FAIL = 0
      DO 1065 K=1,MPITEST_NUMP
        IF (RESULT_ARY(K) .NE. -1) LOOP_FAIL = LOOP_FAIL + 1
 1065 CONTINUE

      IF (LOOP_FAIL .NE. 0) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,98) 'MPI_ALLTOALLV() received', LOOP_FAIL,
     $                    ' bad values'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3


C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Barrier'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_BARRIER() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL


C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Bcast'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      VALUE = MPITEST_ME

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_BCAST(VALUE, 1, MPI_INTEGER, 0,
     $           MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_BCAST() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF

      IF (VALUE .NE. 0) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,97) 'MPI_BCAST() received', RESULT,
     $                    ' expected', 0
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      LOOP_CNT = LOOP_CNT + 3

C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Gather'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      VALUE = -1
      DO 1080 K=1,MPITEST_NUMP
        RESULT_ARY(K) = 1
 1080 CONTINUE

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_GATHER(VALUE, 1, MPI_INTEGER,
     $           RESULT_ARY, 1, MPI_INTEGER, 0,
     $           MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_GATHER() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF

      LOOP_FAIL = 0
      IF (MPITEST_ME .EQ. 0) THEN
        DO 1085 K=1,MPITEST_NUMP
          IF (RESULT_ARY(K) .NE. -1) LOOP_FAIL = LOOP_FAIL + 1
 1085   CONTINUE
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      IF (LOOP_FAIL .NE. 0) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,98) 'MPI_GATHER() received', LOOP_FAIL,
     $                    ' bad values'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3

C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Gatherv'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      VALUE = -1
      DO 1100 K=1,MPITEST_NUMP
        RESULT_ARY(K) = 1
 1100 CONTINUE

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_GATHERV(VALUE, 1, MPI_INTEGER,
     $           RESULT_ARY, COUNTS_ARY, RESULT_DSPLSARY, MPI_INTEGER,
     $           0, MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_GATHERV() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      LOOP_FAIL = 0
      IF (MPITEST_ME .EQ. 0) THEN
        DO 1105 K=1,MPITEST_NUMP
          IF (RESULT_ARY(K) .NE. -1) LOOP_FAIL = LOOP_FAIL + 1
 1105   CONTINUE
      END IF

      IF (LOOP_FAIL .NE. 0) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,98) 'MPI_GATHERV() received', LOOP_FAIL,
     $                    ' bad values'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3


C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Reduce'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      VALUE = -1
      RESULT = 0

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_REDUCE(VALUE, RESULT, 1, MPI_INTEGER,
     $           MPI_SUM, 0, MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_REDUCE() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      IF ((RESULT .NE. -1*MPITEST_NUMP) .AND.
     $    (MPITEST_ME .EQ. 0)) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,97) 'MPI_REDUCE() received', RESULT,
     $                    ' expected', -1*MPITEST_NUMP
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3



C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Reduce_scatter'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      RESULT = 0
      DO 1120 K=1,MPITEST_NUMP
        VALUE_ARY(K) = -1
 1120 CONTINUE

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_REDUCE_SCATTER(VALUE_ARY, RESULT, COUNTS_ARY,
     $           MPI_INTEGER, MPI_SUM, MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_REDUCE_SCATTER() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      IF (RESULT .NE. -1*MPITEST_NUMP) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,97) 'MPI_REDUCE_SCATTER() received', RESULT,
     $                    ' expected', -1*MPITEST_NUMP
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3



C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Scan'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      VALUE = -1
      RESULT = 0

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_SCAN(VALUE, RESULT, 1, MPI_INTEGER,
     $           MPI_SUM, MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_SCAN() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      IF (RESULT .NE. -1*MPITEST_ME-1) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,97) 'MPI_SCAN() received', RESULT,
     $                    ' expected', -1*MPITEST_ME-1
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3


C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Scatter'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      RESULT = 0
      DO 1140 K=1,MPITEST_NUMP
        VALUE_ARY(K) = -1
 1140 CONTINUE

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_SCATTER(VALUE_ARY, 1, MPI_INTEGER,
     $          RESULT, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_SCATTER() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      IF (RESULT .NE. -1) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,97) 'MPI_SCATTER() received', RESULT,
     $                    ' expected', -1
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3


C -------------------------------------------------------------------
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'MPI_Scatterv'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF

      RESULT = 0
      DO 1160 K=1,MPITEST_NUMP
        VALUE_ARY(K) = -1
 1160 CONTINUE

      CALL PRECOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL
      CALL MPI_SCATTERV(VALUE_ARY, COUNTS_ARY, VALUE_DSPLSARY,
     $          MPI_INTEGER, RESULT, 1, MPI_INTEGER, 0,
     $          MPI_COMM_WORLD, IERR)
      IF (IERR .NE. 0) THEN
        WRITE(INFOBUF,99) 'MPI_SCATTER() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      CALL POSTCOLL(PARTNER, SIDE, SENDBUF, RECVBUF, 0,
     $                   REQ1, REQ2, LOOP_FAIL)
      FAIL = FAIL + LOOP_FAIL

      IF (RESULT .NE. -1) THEN
        INFOBUF = ' '
        WRITE(INFOBUF,97) 'MPI_SCATTER() received', RESULT,
     $                    ' expected', -1
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      LOOP_CNT = LOOP_CNT + 3


C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END  
  
  
