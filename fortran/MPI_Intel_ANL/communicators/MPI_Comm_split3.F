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
C  
C  			  Test for MPI_Comm_split
C  
C  This test verifies that MPI_Comm_split creates seperate commuicators,
C  and that message traffic is sorted by communicator.  
C  Each configured intra-communicator is split and tested.
C  
C  There are 4 split tests.  The difference is communicators are split the
C  following way:
C   1)  each rank into a new communicator
C   2)  split into one (same color for all)
C   3)  split in half
C   4)  split in thirds, 2 new comms and 1/3 not in any comm.
C  
C  As a consequence of this test, MPI_Comm_free, MPI_Comm_compare,
C  MPI_Comm_size, MPI_Comm_test_inter and MPI_Comm_rank are also 
C  tested here.  In addition, this test verifies that cached information 
C  in the parent communicator is not inhereted by the split communicators.
C  
C  Test history:
C     1  08/28/96     gt       Original version
C***********************************************************************

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
      PARAMETER( TESTNAME = 'MPI_COMM_SPLIT3')
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
      INTEGER    FTEST_NUMP
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
C                               intercommunicator test flags
      INTEGER    KEY
C                               MPI key
      LOGICAL    FOUND
C                               from MPI_Attr_get
      INTEGER    COLOR
C                               for MPI_Comm_split
      INTEGER    RESULT
      INTEGER    RESULTSPLIT
C                               MPI calls output variables
      INTEGER    COMM2
C                               split communicator handle
      INTEGER    EXTRA
C                               for keyval creation/use
C-----------------------------------------------------------------------------
 98   FORMAT(A,A )
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT, A, INT_FMT)
 102  FORMAT(A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT)
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
C
      FAIL = 0
      VERIFY = 0
      LOOP_COUNT = 0
C
C-------------------------  Create a Keyval  ----------------------------
C

        CALL   MPI_KEYVAL_CREATE(MPI_DUP_FN,
     $                           MPI_NULL_DELETE_FN,
     $                           KEY,
     $                           EXTRA,
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,103) 'Non-Zero return code (', IERR, 
     $     ')  From:  MPI_KEYVAL_CREATE'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        END IF
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
C     Check for inter-communicator
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

        IF(INTERCOMM  .OR.  FTEST_NUMP .LT. 2) THEN
          CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
          GO TO 4000
        END IF

C
        RESULT = 0
        CALL MPI_ATTR_PUT(COMM, 
     $                    KEY, 
     $                    RESULT, 
     $                    IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_ATTR_PUT ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        END IF
C
C     Split the communicator in half
C
 	COLOR = MPITEST_CURRENT_RANK - (MPITEST_CURRENT_RANK/2*2)
C
        CALL MPI_COMM_SPLIT(COMM,
     $                      COLOR,
     $                      -37,
     $                      COMM2, 
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
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        IF(RESULT .NE. MPI_UNEQUAL) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_COMPARE  returned', RESULT, 
     $      ',  expected  MPI_UNEQUAL(', MPI_SIMILAR,
     $      ')   Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Test for intercomm - we know it isn't
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
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        IF (INTERCOMM2) THEN
          WRITE(INFOBUF,105) 
     $      'MPI_COMM_TEST_INTER #2  returned', INTERCOMM2, 
     $      ',  expected  .FALSE. ', 
     $      ' Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Check the Size
C
        CALL MPI_COMM_SIZE(COMM2, 
     $                     RESULTSPLIT, 
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
C     Size should be half the original, plus one if there are an
C     odd number of ranks and this is an even-numbered rank 
C
        RESULT = FTEST_NUMP / 2
        IF (( (FTEST_NUMP - (FTEST_NUMP/2*2)) .EQ. 1)  .AND.
     $  (MPITEST_CURRENT_RANK - (MPITEST_CURRENT_RANK/2*2)) .EQ. 0)
     $                                            RESULT = RESULT + 1
        IF ( RESULTSPLIT .NE. RESULT ) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_SIZE returned', RESULTSPLIT, 
     $      ',  expected ', RESULT, 
     $      ' Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
C  Check the rank 
C
        CALL MPI_COMM_RANK(COMM2,
     $                     RESULTSPLIT, 
     $                     IERR)
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
        END IF
        IF (RESULTSPLIT .NE.  MPITEST_CURRENT_RANK/2) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_Comm_rank returned', RESULTSPLIT, 
     $      ',  Expected   ', MPITEST_CURRENT_RANK/2,
     $      ' Comm_Index = ', COMM_INDEX 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Try the new communicator
C
          IF(RESULTSPLIT .EQ. 0) RESULT = COLOR
C
          CALL MPI_BCAST( RESULT, 
     $                    1, 
     $                    MPI_INTEGER, 
     $                    0, 
     $                    COMM2,
     $                    IERR )
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_Bcast '
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          ELSE IF  (RESULT .NE. COLOR) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,102) 
     $        'MPI_Bcast received incorrect data, Actual = ', RESULT,
     $        '  Expected', COLOR, 
     $        '  COMM_INDEX = ', COMM_INDEX 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
C  Get the value for the attribute; it should NOT be there
C
            CALL MPI_ATTR_GET( COMM2,
     $                         KEY,
     $                         RESULTSPLIT,
     $                         FOUND,
     $                         IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $          ')  From:  MPI_Attr_get '
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              ERRORSTRING = ' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            ELSE IF  (FOUND .NEQV. .FALSE.) THEN
              FAIL = FAIL + 1
              WRITE(INFOBUF,104) 
     $          'MPI_Attr_get found attribute in split comm', 
     $          ' it should not(', 'COMM_INDEX', COMM_INDEX, ')' 
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            END IF
            LOOP_COUNT = LOOP_COUNT + 1
C
C  Free the split communicator
C
            CALL MPI_COMM_FREE(COMM2, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $          ')  From:  MPI_COMM_FREE ( COMM_INDEX ', 
     $          COMM_INDEX
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              ERRORSTRING = ' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF
C
            IF(COMM2 .NE. MPI_COMM_NULL)  THEN
              WRITE(INFOBUF,105) 
     $          'Comm2 = ', COMM2,  ' after MPI_Comm_free', 
     $          ',  expected MPI_COMM_NULL ( Comm_Index = ',
     $          COMM_INDEX, ')' 
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF
            LOOP_COUNT = LOOP_COUNT + 1
C
C  Delete the Attribute
C
            CALL MPI_ATTR_DELETE(COMM, KEY, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $          ')  From:  MPI_ATTR_DELETE ( COMM_INDEX ', 
     $          COMM_INDEX
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              ERRORSTRING = ' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF
C
            CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
 3800       CONTINUE
C                Node defined in current communicator
 4000     CONTINUE
C                End communicator loop
C
C  Free the Keyval
C
          CALL   MPI_KEYVAL_FREE(KEY, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_Keyval_free'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          END IF
C
C     Report overall results
C
          CALL MPITEST_REPORT(LOOP_COUNT - FAIL, FAIL, VERIFY, TESTNAME)
          
          CALL MPI_FINALIZE(IERR)

          END
