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
C                   Test for MPI_Issend()
C
C This test verifies that the basic blocking MPI_Ssend operation makes progress
C when matched by an MPI_Irecv, even though the MPI_Wait does not occur until
C later (taken from Example 3.14 from the MPI Spec).
C 
C Rank 0 does an Ssend followed by a Send, Rank 1 does an Irecv followed by
C a Recv, then the Wait for the first Irecv.  According to the Spec., this
C should work.
C 
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

      LOGICAL    FLAG

      INTEGER    RECV1_STAT(MPI_STATUS_SIZE)
      INTEGER    RECV2_STAT(MPI_STATUS_SIZE)
      INTEGER    REQUEST

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

      INTEGER ITERATIONS
      PARAMETER (ITERATIONS=65535)

      INTEGER BUFF1(3)
      INTEGER BUFF2(4)

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
      TESTNAME = 'MPI_Issend_overtake2'
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

C
C          Send a synchronous message
C
          CALL MPI_SSEND(BUFF1, 3, MPI_INTEGER, 1, 1,
     $           MPI_COMM_WORLD, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_SSEND() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF

C
C         Send a reqular message
C
          CALL MPI_SEND(BUFF2, 4, MPI_INTEGER, 1, 2,
     $         MPI_COMM_WORLD, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_SEND() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF

          LOOP_CNT = 1

        ELSE
C
C         Receiver
C
          CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

C
C         Receive the Issend message
C
          CALL MPI_IRECV(BUFF1, 3, MPI_INTEGER, 0, 1,
     $         MPI_COMM_WORLD, REQUEST, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_IRECV() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF

C
C         Post a receive for the second message
C
          CALL MPI_RECV(BUFF2, 4, MPI_INTEGER, 0, 2,
     $         MPI_COMM_WORLD, RECV2_STAT, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_IRECV() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF

          CALL MPI_WAIT(REQUEST, RECV1_STAT, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_WAIT() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF
         

          LOOP_CNT = 1

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

