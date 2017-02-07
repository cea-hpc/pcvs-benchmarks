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
C                   Test for MPI_Bsend()
C
C This test verifies that the basic blocking MPI_Bsend operation with MPI_Recv
C delivers messages in the order they were sent.
C
C This test uses the first 2 ranks in MPI_COMM_WORLD, first sending a large
C message then a number of short ones, and ensures that they are received
C in the proper order by verifying the data received.
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
      INTEGER    ERRORS

      INTEGER    RECV_STAT(MPI_STATUS_SIZE)

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME

      INTEGER NUMSHORT
      PARAMETER (NUMSHORT=20)

      INTEGER LONGLENGTH
      PARAMETER (LONGLENGTH=8192)

      INTEGER BUFFLENGTH, BL
      PARAMETER (BUFFLENGTH=
     $         2*LONGLENGTH + (NUMSHORT+1)*(MPI_BSEND_OVERHEAD+1))

      MPITEST_BUF_TYPE VALUE

      CHARACTER*(NUMSHORT+1) SHORTB
      CHARACTER*(LONGLENGTH+1) BUFFER
      CHARACTER*(BUFFLENGTH) BSEND_BUFF
      CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

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
      TESTNAME = 'MPI_Bsend_overtake'
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
          CALL MPI_BUFFER_ATTACH(BSEND_BUFF, BUFFLENGTH, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_BUFFER_ATTACH() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF


          CALL MPITEST_DATATEMPLATE_INIT(VALUE, 0, MPITEST_CHARACTER)
          CALL MPITEST_INIT_BUFFER_INC(MPITEST_CHARACTER, LONGLENGTH,
     $          VALUE, BUFFER)
          CALL MPITEST_DATATEMPLATE_INIT(VALUE, 1, MPITEST_CHARACTER)
          CALL MPITEST_INIT_BUFFER_INC(MPITEST_CHARACTER, NUMSHORT,
     $          VALUE, SHORTB)

          CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

C
C          Send a long message
C
          CALL MPI_BSEND(BUFFER, LONGLENGTH, MPI_CHARACTER, 1, 1,
     $           MPI_COMM_WORLD, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_BSEND() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF

C
C         Send a number of short messages
C
          DO 200 I=1, NUMSHORT
            CALL MPI_BSEND(SHORTB(I:I), 1, MPI_CHARACTER, 1, 1,
     $           MPI_COMM_WORLD, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,99) 'MPI_BSEND() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            END IF
 200      CONTINUE

          LOOP_CNT = 1 + NUMSHORT

          BL = BUFFLENGTH
C         The second argument of MPI_BUFFER_DETACH is an OUTPUT variable
C         and must not be a Fortran PARAMETER value (SEGV under g77, 
C         probably others)
          CALL MPI_BUFFER_DETACH(BSEND_BUFF, BL, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_BUFFER_DETACH() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF


        ELSE
C
C         Receiver
C
          CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1, MPITEST_CHARACTER)
          CALL MPITEST_INIT_BUFFER(MPITEST_CHARACTER, LONGLENGTH,
     $          VALUE, BUFFER)
          CALL MPITEST_INIT_BUFFER(MPITEST_CHARACTER, NUMSHORT,
     $          VALUE, SHORTB)


          CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

C
C         Receive the long message
C
          CALL MPI_RECV(BUFFER, LONGLENGTH, MPI_CHARACTER, 0, 1,
     $         MPI_COMM_WORLD, RECV_STAT, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_RECV() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          END IF

C
C         Receive a number of short messages
C
          DO 300 I=1, NUMSHORT
            CALL MPI_RECV(SHORTB(I:I), 1, MPI_CHARACTER, 0, 1,
     $           MPI_COMM_WORLD, RECV_STAT, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,99) 'MPI_RECV() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            END IF
 300      CONTINUE

          LOOP_CNT = 1 + NUMSHORT

C
C         Check received data
C
          CALL MPITEST_DATATEMPLATE_INIT(VALUE, 0, MPITEST_CHARACTER)
          CALL MPITEST_BUFFER_ERRORS_INC(MPITEST_CHARACTER, LONGLENGTH,
     $         VALUE, BUFFER, ERRORS)

          IF (ERRORS .NE. 0) THEN
            FAIL = FAIL + 1
            INFOBUF=' '
            WRITE (INFOBUF,189) 'Found', ERRORS,
     $         ' errors in first (long) buffer '
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          END IF

          CALL MPITEST_DATATEMPLATE_INIT(VALUE, 1, MPITEST_CHARACTER)
          CALL MPITEST_BUFFER_ERRORS_INC(MPITEST_CHARACTER, NUMSHORT,
     $         VALUE, SHORTB, ERRORS)

          IF (ERRORS .NE. 0) THEN
            FAIL = FAIL + ERRORS
            INFOBUF=' '
            WRITE (INFOBUF,189) 'Found', ERRORS,
     $         ' short messages received incorrectly '
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
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

