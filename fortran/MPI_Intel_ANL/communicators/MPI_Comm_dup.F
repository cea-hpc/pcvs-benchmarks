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
C***********************************************************************
C 			  Test for MPI_Comm_dup
C 
C This test verifies that MPI_Comm_dup creates a seperate commuicator,
C and that message traffic is sorted by communicator.  Each configured
C communicator is duplicated and tested.
C 
C As a consequence of this test, MPI_Comm_free, MPI_Comm_compare, 
C MPI_Comm_size, MPI_Comm_test_inter and MPI_Comm_rank are also 
C tested here.  Testing of cached information is done in the tests 
C for cached information calls.
C 
C Test history:
C    1  08/28/96     gt/jh       Original version
C 
C **********************************************************************

#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'
C
      PROGRAM MAIN
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
C
C  Program constants (Parameters)
C
      CHARACTER*32          TESTNAME
      PARAMETER( TESTNAME = 'MPI_COMM_DUP')
C
C  General variables that occur in all programs
C
      INTEGER    CNUMBR
C                               index for communications loop
      INTEGER    COMM_INDEX
C                               the array index of the current comm
      INTEGER    COMM_TYPE
C                               the index of the current communicator type
      INTEGER    COMM_COUNT
C                               loop counter for communicator loop
      INTEGER    TEST_NUMP
C                               number of ranks in the current communicator
      INTEGER    LOOP_COUNT
C                               counts total number of failures, loops
      INTEGER    COMM
C                               MPITEST  communicator under test
      INTEGER    IERR
      INTEGER    ERR
C                               return value from MPI calls
      INTEGER    ERRORCLASS
      INTEGER    SIZE 
C                               error class of IERR
      INTEGER    ERRSIZE
C                               length of error message
      INTEGER    FAIL              
      INTEGER    VERIFY
C                           counts total number of PASSes/FAILures/Verifies
      CHARACTER*(IOSIZE)    INFOBUF
C                           buffer for passing messages to MPITEST
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
C                           string describing error flag
C
C  Variables specific to this program
C
      LOGICAL    INTERCOMM
      LOGICAL    INTERCOMM2
C                               intercommunicator flag
      INTEGER    RESULT
C                               communicator test output
      INTEGER    RESULTDUP
C                               communicator test output
      INTEGER    BUFFER1
      INTEGER    BUFFER2
      INTEGER    BUFFER3
C                               Message buffers for ISENDs
      INTEGER    COMM2
C                               for dup'ing communicator
      INTEGER    REQUEST1
      INTEGER    REQUEST2
      INTEGER    REQUEST3
C                               Communication request handles for ISENDs
      INTEGER    STATUS1(MPI_STATUS_SIZE)
      INTEGER    STATUS2(MPI_STATUS_SIZE)
      INTEGER    STATUS3(MPI_STATUS_SIZE)
C                               Status arrays for WAITs
C-----------------------------------------------------------------------------
 98   FORMAT(A,A )
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT, A, INT_FMT)
 102  FORMAT(A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT)
 103  FORMAT(A, INT_FMT, A, A, A, INT_FMT)
 104  FORMAT(A, A, A, INT_FMT, A)
 105  FORMAT(A, INT_FMT, A, A, INT_FMT, A)
 106  FORMAT(A, INT_FMT, INT_FMT, INT_FMT, A, A, INT_FMT)
C===========================================================================
C
C  Initialize the MPI environment
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C
C  Initialize the MPITEST environment
C
      CALL MPITEST_INIT(IERR)
C
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF
C
C  Get number of communicators
C
      CALL MPITEST_NUM_COMM_SIZES(COMM_COUNT)
C
      FAIL = 0
      VERIFY = 0
      LOOP_COUNT = 0
C
C---------------------  Loop over Communicators  ------------------------
C
      DO 4000 CNUMBR = 1,COMM_COUNT
        CALL MPITEST_GET_COMM_INDEX(CNUMBR, COMM_INDEX)
        CALL MPITEST_GET_COMM_TYPE(CNUMBR, COMM_TYPE)
        CALL MPITEST_GET_COMMUNICATOR(COMM_TYPE, COMM_INDEX, COMM,
     $        TEST_NUMP)
C
C       Skip to end of loop if this node is not part of current
C       communicator
C
        IF (MPITEST_CURRENT_RANK .EQ. MPI_UNDEFINED ) GO TO 3800
C
C  Dup the communicator
C
C
        CALL MPI_COMM_DUP(COMM, 
     $                    COMM2, 
     $                    IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_DUP ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1

C
C  Compare the two communicators
C
        CALL MPI_COMM_COMPARE(COMM, 
     $                        COMM2, 
     $                        RESULT, 
     $                        IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR,
     $      ')  From:  MPI_COMM_COMPARE ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF ( RESULT .NE. MPI_CONGRUENT ) THEN
          WRITE(INFOBUF,102) 'MPI_Comm_compare returned', RESULT, 
     $      ',  expected', MPI_CONGRUENT, 
     $      ' Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Check for inter-communicator
C
        CALL MPI_COMM_TEST_INTER(COMM, 
     $                           INTERCOMM, 
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_TEST_INTER #1 ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        CALL MPI_COMM_TEST_INTER(COMM2, 
     $                           INTERCOMM2, 
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_TEST_INTER #2 ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF ( INTERCOMM .NEQV. INTERCOMM2 ) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_TEST_INTER returned', INTERCOMM2, 
     $      ',  expected', INTERCOMM, 
     $      ' Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Check the Size
C
        CALL MPI_COMM_SIZE(COMM2, 
     $                     RESULTDUP, 
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_SIZE ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        IF(INTERCOMM) THEN
C                             An inter-communicator TEST_NUMP will be too big
C
          CALL MPI_COMM_SIZE(COMM, 
     $                       RESULT, 
     $                       IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_COMM_SIZE ( COMM_INDEX ', 
     $        COMM_INDEX
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
        ELSE
          RESULT = TEST_NUMP
        END IF
C
        IF(RESULT .NE. RESULTDUP) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_SIZE returned', RESULTDUP, 
     $      ',  expected', RESULT, 
     $      ' Comm_Index = ', COMM_INDEX 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Check my rank 
C
        CALL MPI_COMM_RANK(COMM2, RESULTDUP, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Comm_rank ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF (RESULTDUP .NE. MPITEST_CURRENT_RANK) THEN
          WRITE(INFOBUF,102) 'MPI_Comm_rank returned', RESULTDUP, 
     $      ',  expected', MPITEST_CURRENT_RANK, 
     $      ' Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C   Now Isend 3 messages from rank 0 to rank 1, one per communicator.
C   Recv them in the opposite order, to make sure messages are
C   filtered to the proper communicator. To keep things simple
C   here, we'll test intercommunicators in a different test.
C   Repeat for Send 

        IF((TEST_NUMP .GE. 2)  .AND.  (.NOT. INTERCOMM )) THEN
          IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
C     Sender
            BUFFER1=1
            BUFFER2=2
            BUFFER3=3
C
            CALL MPI_ISEND(BUFFER1, 1, MPI_INTEGER, 1, 1, COMM,  
     $                                                REQUEST1, IERR)
            CALL MPI_ISEND(BUFFER2, 1, MPI_INTEGER, 1, 1, COMM2, 
     $                                                REQUEST2, IERR)
            CALL MPI_ISEND(BUFFER3, 1, MPI_INTEGER, 1, 1, COMM2,
     $                                                REQUEST3, IERR)
C
            CALL MPI_BARRIER(COMM2, IERR)
C
            CALL MPI_WAIT(REQUEST1, STATUS1, IERR)
            CALL MPI_WAIT(REQUEST2, STATUS2, IERR)
            CALL MPI_WAIT(REQUEST3, STATUS3, IERR)
C
            CALL MPI_SEND(BUFFER1, 1, MPI_INTEGER, 1, 1, COMM,  IERR)
            CALL MPI_SEND(BUFFER2, 1, MPI_INTEGER, 1, 1, COMM,  IERR)
            CALL MPI_SEND(BUFFER3, 1, MPI_INTEGER, 1, 1, COMM2, IERR)
C
          ELSE IF (MPITEST_CURRENT_RANK .EQ. 1) THEN
C     Receiver 
            BUFFER1=0
            BUFFER2=0
            BUFFER3=0
C
            CALL MPI_BARRIER(COMM2, IERR)
C
            CALL MPI_IRECV(BUFFER2, 1, MPI_INTEGER, 0, 1, COMM2, 
     $                                                 REQUEST2, IERR)
            CALL MPI_IRECV(BUFFER3, 1, MPI_INTEGER, 0, 1, COMM2, 
     $                                                 REQUEST3, IERR)
            CALL MPI_IRECV(BUFFER1, 1, MPI_INTEGER, 0, 1, COMM,  
     $                                                 REQUEST1, IERR)
            CALL MPI_WAIT(REQUEST1, STATUS1, IERR)
            CALL MPI_WAIT(REQUEST2, STATUS2, IERR)
            CALL MPI_WAIT(REQUEST3, STATUS3, IERR)
C
            IF ((BUFFER1 .NE. 1) .OR.  (BUFFER2 .NE. 2) .OR. 
     $                                  (BUFFER3 .NE.3)) THEN
              WRITE(INFOBUF,106) 
     $          'Receive comms misfiltered messages (Sends):', 
     $          BUFFER1, BUFFER2, BUFFER3,
     $          ',  Expected:  1/2/3  ',
     $          ' Comm_Index = ', COMM_INDEX 
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF
            LOOP_COUNT = LOOP_COUNT + 1
C
            BUFFER1 = 0
            BUFFER2 = 0
            BUFFER3 = 0
C
            CALL MPI_IRECV(BUFFER1, 1, MPI_INTEGER, 0, 1, 
     $                                        COMM,  REQUEST2, IERR)
            CALL MPI_IRECV(BUFFER3, 1, MPI_INTEGER, 0, 1, 
     $                                        COMM2, REQUEST3, IERR)
            CALL MPI_IRECV(BUFFER2, 1, MPI_INTEGER, 0, 1,
     $                                        COMM,  REQUEST1, IERR)
            CALL MPI_WAIT(REQUEST1, STATUS1, IERR)
            CALL MPI_WAIT(REQUEST2, STATUS2, IERR)
            CALL MPI_WAIT(REQUEST3, STATUS3, IERR)
C
            IF ((BUFFER1 .NE. 1) .OR.  (BUFFER2 .NE. 2) .OR. 
     $                                  (BUFFER3 .NE.3)) THEN
              WRITE(INFOBUF,106) 
     $          'Receive comms misfiltered messages (Sends):', 
     $          BUFFER1, BUFFER2, BUFFER3,
     $          ',  Expected:  1/2/3  ',
     $          ' Comm_Index = ', COMM_INDEX 
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF
            LOOP_COUNT = LOOP_COUNT + 1
          ELSE
            CALL MPI_BARRIER(COMM2, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $          ')  From:  MPI_BARRIER ( COMM_INDEX ', 
     $            COMM_INDEX
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              ERRORSTRING = ' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF
          END IF	
C
        END IF
C     Lastly, leave a communication outstanding, free the comm,
C     then complete the communication 
C
        IF ((TEST_NUMP .GE. 2)  .AND.   (.NOT.INTERCOMM )) THEN
          IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
C  Sender
            CALL MPI_ISEND(BUFFER1, 1, MPI_INTEGER, 1, 1, COMM2, 
     $                                                 REQUEST1, IERR)
          ELSE IF (MPITEST_CURRENT_RANK .EQ. 1) THEN
C  Receiver
            BUFFER1 = 0
            CALL MPI_IRECV(BUFFER1, 1, MPI_INTEGER, 0, 1, COMM2, 
     $                                               REQUEST1, IERR)
          END IF
        END IF
C
C  Free the Dup'ed Communicator
C
        CALL MPI_COMM_FREE(COMM2, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_FREE ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF( COMM2 .NE. MPI_COMM_NULL ) THEN
          WRITE(INFOBUF,103) 
     $      'COMM2 = ', COMM2, '  after MPI_Comm_free, ', 
     $      ',  expected  MPI_COMM_NULL', 
     $      ' Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Now finish the communication started above 
        IF ((TEST_NUMP .GE.  2)  .AND.  (.NOT.INTERCOMM)) THEN
          IF (MPITEST_CURRENT_RANK .EQ.  0) THEN
C     Sender
            CALL MPI_WAIT(REQUEST1, STATUS1, IERR)
C
          ELSE IF (MPITEST_CURRENT_RANK .EQ. 1)  THEN
C     Receiver
            CALL MPI_WAIT(REQUEST1, STATUS1, IERR)
            IF (BUFFER1 .NE.  1) THEN
              WRITE(INFOBUF,102) 
     $          'Receive while freeing comm received incorrect data',
     $          BUFFER1, ',  expected  1,  Comm_Index = ', 
     $          COMM_INDEX 
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF
            LOOP_COUNT = LOOP_COUNT + 1
          END IF
        END IF
C
        CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
 3800   CONTINUE
C                Node defined in current communicator
 4000 CONTINUE
C                End communicator loop
C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_COUNT - FAIL, FAIL, VERIFY, TESTNAME)
C      
      CALL MPI_FINALIZE(IERR)
C
      END
