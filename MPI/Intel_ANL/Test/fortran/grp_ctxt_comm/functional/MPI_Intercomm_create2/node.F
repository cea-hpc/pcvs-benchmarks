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
C  			  Test for MPI_Intercomm_create
C  
C  This test verifies that MPI_Intercomm_create combines two eperate 
C  commuicators, and that message traffic is sorted by communicator. 
C  Each configured intra- communicator (with more than 2 ranks) is split, 
C  an intercommunicator is created and tested.
C  
C  There are 2 create tests.  The difference is communicators are split the
C  following way:
C   1)  split in half
C   2)  split in thirds: rank 0 in 1 comm, ranks 2 through (size - 1) in second
C  	comm, rank 1 not in either.
C  
C  As a consequence of this test, MPI_Comm_free, MPI_Comm_compare, 
C  MPI_Comm_size, MPI_Comm_remote_size, MPI_Comm_rank, MPI_Comm_test_inter 
C  and rank are also tested here.  In addition, MPI_Comm_dup is tested with
C  intercommunicators here, also verifying that cached information in the 
C  parent communicator is inhereted by the child communicator.
C  
C  Test history:
C     1  08/28/96     gt       Original version
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
C
      CHARACTER*32          TESTNAME
      PARAMETER( TESTNAME = 'MPI_INTERCOMM_CREATE2')
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
      INTEGER    PASS
      INTEGER    FAIL              
      INTEGER    VERIFY
C                           counts total number of PASSes/FAILures
      CHARACTER*(IOSIZE)    INFOBUF
C                           buffer for passing messages to MPITEST
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
C                           string describing error flag
C
C  Variables specific to this program
C
      LOGICAL    INTERCOMM
C                               intercommunicator flag
      LOGICAL    FOUND
C                               FLAG from MPI_Attr_get
      INTEGER    RESULT
C                               output from MPI_Comm_compare
      INTEGER    GROUP
C                               group handle
      INTEGER    COMMINTER
      INTEGER    COMM2
C                               for split communicator
      INTEGER    KEY
C                               MPI_Key
      INTEGER    COMMSIZE
C                               communicator size
      INTEGER    EXTRA
C                               for keyval creation/use
      INTEGER    COLOR
      INTEGER    LRANK
      INTEGER    FTEST_NUMP
      INTEGER    COMMSPLIT
      INTEGER    RESULTSPLIT
      INTEGER    RESULTINTER

      INTEGER    BUFFER1
      INTEGER    BUFFER2
      INTEGER    BUFFER3
C
C-----------------------------------------------------------------------------
 98   FORMAT(A,A )
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT, A, INT_FMT)
 102  FORMAT(A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT ,
     $       A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT)
 103  FORMAT(A, INT_FMT, A, A, A, INT_FMT)
 104  FORMAT(A, A, A, INT_FMT, A)
 105  FORMAT(A, INT_FMT, A, A, INT_FMT, A)
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

      FAIL = 0
      LOOP_COUNT = 0
      VERIFY = 0
C
C---------------------  Loop over Communicators  ------------------------
C
      DO 4000 CNUMBR = 1,COMM_COUNT
        CALL MPITEST_GET_COMM_INDEX(CNUMBR, COMM_INDEX)
        CALL MPITEST_GET_COMM_TYPE(CNUMBR, COMM_TYPE)
        CALL MPITEST_GET_COMMUNICATOR(COMM_TYPE, COMM_INDEX, COMM,
     $        FTEST_NUMP)
C
C       Skip to end of loop if this node is not part of current
C       communicator
C
        IF (MPITEST_CURRENT_RANK .EQ. MPI_UNDEFINED ) GO TO 3800

C
C     Check for intercommunicator and skip (continue) if so
C     An inter-communicator cannot be split  
C
        CALL MPI_COMM_TEST_INTER(COMM, 
     $                           INTERCOMM, 
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_TEST_INTER  ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1

        IF((INTERCOMM .EQV. .TRUE.)  .OR.  (FTEST_NUMP .LT. 3)) THEN
          CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
          GO TO 4000
        END IF
C
C     Split the communicator 
C
        COLOR = 0
        IF(MPITEST_CURRENT_RANK .EQ. 0) THEN
          COLOR = FTEST_NUMP - 1
        ELSE IF(MPITEST_CURRENT_RANK .EQ. 1) THEN
          COLOR = MPI_UNDEFINED
        END IF
C
        CALL MPI_COMM_RANK( COMM, LRANK, IERR )
        CALL MPI_COMM_SPLIT(COMM,
     $                      COLOR,
     $                      LRANK,
     $                      COMMSPLIT, 
     $                      IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_SPLIT ( COMM_INDEX ', 
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
        IF(MPITEST_CURRENT_RANK .EQ. 1) THEN
          IF(COMMSPLIT .NE. MPI_COMM_NULL) THEN
            WRITE(INFOBUF,105) 
     $      'MPI_Comm_split w/color = MPI_UNDEFINED returned ',
     $      COMMSPLIT,  
     $      '  Expected:  MPI_COMM_NULL',  
     $      ' Comm_Index = ', COMM_INDEX 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          GO TO 3600
        END IF
C  Combine the two new communicators to create an intercommunicator

C
        IF(MPITEST_CURRENT_RANK .EQ. 0) THEN
C     First, split 1 rank
C     0 Must be local leader, last rank is remote leader 

          CALL MPI_INTERCOMM_CREATE(COMMSPLIT,
     $                              0,
     $                              COMM,
     $                              FTEST_NUMP - 1, 
     $                              65534, 
     $                              COMMINTER, 
     $                              IERR)
        ELSE
C
C     Second split, all ranks but 2
C     last rank is local leader, 0 is remote leader 

          CALL MPI_INTERCOMM_CREATE(COMMSPLIT,
     $                              FTEST_NUMP - 3, 
     $                              COMM,
     $                              0,
     $                              65534, 
     $                              COMMINTER, 
     $                              IERR)
C
        END IF

        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_INTERCOMM_CREATE ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Compare the intercommunicator with the original
C
        CALL MPI_COMM_COMPARE(COMM, 
     $                        COMMINTER, 
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
        END IF


        IF(RESULT .NE. MPI_UNEQUAL) THEN
          WRITE(INFOBUF,105) 
     $      'MPI_COMM_COMPARE returned', RESULT, 
     $      ',  expected  MPI_UNEQUAL', 
     $      ' Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check for inter-communicator -- We know it is
C
        CALL MPI_COMM_TEST_INTER(COMMINTER, 
     $                           INTERCOMM, 
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_TEST_INTER #2 ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1

        IF(INTERCOMM .NEQV. .TRUE.) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_TEST_INTER  did NOT return TRUE ( ', INTERCOMM,
     $      ')   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C  Check the Size
C
        CALL MPI_COMM_SIZE(COMMINTER, 
     $                     RESULTINTER, 
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_SIZE ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Size should be 1 (rank 0) or FTEST_NUMP - 2 (all others)
C

        IF(MPITEST_CURRENT_RANK .EQ. 0) THEN
          RESULT = 1
        ELSE
          RESULT = FTEST_NUMP - 2
        END IF
C
        IF ( RESULTINTER .NE. RESULT) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_SIZE returned', RESULTINTER, 
     $      ',  Expected ',  RESULT,
     $      '   Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C  Check the REMOTE Size
C
        CALL MPI_COMM_REMOTE_SIZE(COMMINTER, 
     $                            RESULTINTER, 
     $                            IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_REMOTE_SIZE ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        IF(MPITEST_CURRENT_RANK .EQ. 0) THEN
          RESULT = FTEST_NUMP - 2
        ELSE
          RESULT = 1
        END IF
C
        IF ( RESULTINTER .NE. RESULT) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_REMOTE_SIZE returned', RESULTINTER, 
     $      ',  Expected ',  RESULT,
     $      '   Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C  Check the rank, it should be the same value as in the split communicator 
C
        CALL MPI_COMM_RANK(COMMINTER,
     $                     RESULTINTER, 
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Comm_rank ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        RESULT = 0
        IF(MPITEST_CURRENT_RANK .GT. 0) 
     $                 RESULT = MPITEST_CURRENT_RANK- 2
C
        IF(RESULTINTER .NE. RESULT) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_RANK returned', RESULTINTER, 
     $      ',  Expected ',  RESULT,
     $      '   Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C  Try the new communicator
C
C Resultsplit is nowhere set!
C     This appears to be old code from the MPI_Comm_split case that was
C     never removed (but was used to perform the split used to 
C     create the intercommunicator)
CC        IF(RESULTSPLIT .EQ. 0) RESULT = COLOR
CCC
CC          CALL MPI_BCAST( RESULT, 
CC     $                    1, 
CC     $                    MPI_INTEGER, 
CC     $                    0, 
CC     $                    COMMSPLIT,
CC     $                    IERR )
CC          IF (IERR .NE. MPI_SUCCESS) THEN
CC            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
CC     $        ')  From:  MPI_Bcast '
CC            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
CC            ERRORSTRING = ' '
CC            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
CC            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
CC            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
CC            FAIL = FAIL + 1
CC          ELSE IF  (RESULT .NE. COLOR) THEN
CC            FAIL = FAIL + 1
CC            WRITE(INFOBUF,102) 
CC     $        'MPI_Bcast received incorrect data, Actual = ', RESULT,
CC     $        '  Expected', COLOR, 
CC     $        '  COMM_INDEX = ', COMM_INDEX 
CC            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
CC          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
C     Try to dup the new intercommunicator
C
          CALL MPI_COMM_DUP(COMMINTER, 
     $                    COMM2, 
     $                    IERR)
C
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_DUP ( COMM_INDEX ', 
     $        COMM_INDEX
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
C  Compare the two INTER-communicators
C
          CALL MPI_COMM_COMPARE(COMMINTER, 
     $                            COMM2, 
     $                            RESULT, 
     $                            IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,102) 'Non-Zero return code (', IERR,
     $      ')  From:  MPI_COMM_COMPARE #2 ( COMM_INDEX ', 
     $      COMM_INDEX
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              ERRORSTRING = ' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
              FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1

          IF(RESULT .NE. MPI_CONGRUENT) THEN
            WRITE(INFOBUF,102) 
     $        'MPI_COMM_COMPARE #2 for one rank  returned', RESULT, 
     $        ',  expected  MPI_CONGRUENT(', MPI_CONGRUENT,
     $        ')   Comm_Index = ', COMM_INDEX 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
C  Free all  Communicators
C
          CALL MPI_COMM_FREE(COMMSPLIT, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_COMM_FREE(COMMSPLIT)  ( COMM_INDEX ', 
     $        COMM_INDEX
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
          IF(COMMSPLIT .NE. MPI_COMM_NULL)  THEN
            WRITE(INFOBUF,105) 
     $        'COMMSPLIT = ', COMMSPLIT,  ' after MPI_Comm_free', 
     $        ',  expected MPI_COMM_NULL ( Comm_Index = ',
     $        COMM_INDEX, ')' 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
          CALL MPI_COMM_FREE(COMMINTER, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_COMM_FREE(COMMINTER)  ( COMM_INDEX ', 
     $        COMM_INDEX
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
          IF(COMMINTER .NE. MPI_COMM_NULL)  THEN
            WRITE(INFOBUF,105) 
     $        'COMMINTER = ', COMMINTER,  ' after MPI_Comm_free', 
     $        ',  expected MPI_COMM_NULL ( Comm_Index = ',
     $        COMM_INDEX, ')' 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
          CALL MPI_COMM_FREE(COMM2, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_COMM_FREE(COMM2)  ( COMM_INDEX ', 
     $        COMM_INDEX
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
          IF(COMM2 .NE. MPI_COMM_NULL)  THEN
            WRITE(INFOBUF,105) 
     $        'COMM2 = ', COMM2,  ' after MPI_Comm_free', 
     $        ',  expected MPI_COMM_NULL ( Comm_Index = ',
     $        COMM_INDEX, ')' 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
 3600     CONTINUE
C     Rank not 1 in this test
C
        CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
 3800   CONTINUE
C                Node defined in current communicator
 4000 CONTINUE
C                End communicator loop
C
C     11-11-02: This appears to be code that was left in this file by
C     mistake.
C     Since no keyval is ever created, none can be freed
C  Free the Keyval
C
CC      CALL   MPI_KEYVAL_FREE(KEY, IERR)
CC      IF (IERR .NE. MPI_SUCCESS) THEN
CC        FAIL = FAIL + 1
CC        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
CC     $    ')  From:  MPI_Keyval_free'
CC        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
CC        ERRORSTRING = ' '
CC        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
CC        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
CC        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
CC      END IF
C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_COUNT - FAIL, FAIL, 0, TESTNAME)
      
      CALL MPI_FINALIZE(IERR)

      END
