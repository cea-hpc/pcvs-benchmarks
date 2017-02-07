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
C  			  Test for MPI_Comm_create
C  
C  This test verifies that MPI_Comm_create creates a correct communicator
C  from an arbitrary group.  
C  Each configured communicator is tested.
C  
C  As a consequence of this test, MPI_Group_free, MPI_Comm_size, MPI_Comm_rank,
C  MPI_Group_free and the non-propogation of attributes are tested here.
C  
C  Test history:
C     1  08/28/96     gt/jh       Original version
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
      PARAMETER( TESTNAME = 'MPI_COMM_COMPARE')
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
      INTEGER    COMM_SIZE
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
      INTEGER    PASS
      INTEGER    FAIL              
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
      INTEGER    COMM2
C                               for split communicator
      INTEGER    KEY
C                               MPI_Key
      INTEGER    COMMSIZE
C                               communicator size
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
     $        COMM_SIZE)
C
C       Skip to end of loop if this node is not part of current
C       communicator
C
        IF (MPITEST_CURRENT_RANK .EQ. MPI_UNDEFINED ) GO TO 3800
C
C  Set a value for the attribute
C
        RESULT = 0
        LOOP_COUNT = LOOP_COUNT + 1
C
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
C  Create a new group
C
        LOOP_COUNT = LOOP_COUNT + 1
        CALL MPI_COMM_GROUP(COMM, GROUP, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR,
     $      ')  From:  MPI_COMM_GROUP ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
C
C  Check for inter-communicator
C
        LOOP_COUNT = LOOP_COUNT + 1
        CALL MPI_COMM_TEST_INTER(COMM, 
     $                           INTERCOMM, 
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_TEST_INTER ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        END IF
C
C  Skip if inter-communicator
C     You can only create a communicator from an intra-communicator
C     And we want to create a new communicator within the group
C
        IF(.NOT.INTERCOMM ) THEN
C
C
C--------------------------  MPI_Comm_create  ------------------------
C
          LOOP_COUNT = LOOP_COUNT + 1
          CALL MPI_COMM_CREATE(COMM, 
     $                         GROUP,
     $                         COMM2,
     $                         IERR)

          IF (IERR .NE. MPI_SUCCESS) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,103) 'Non-Zero return code (', IERR, 
     $        ')  From:  ', TESTNAME, ' ( COMM_INDEX ', 
     $        COMM_INDEX
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
C
C  Check my rank in the new communicator
C
            LOOP_COUNT = LOOP_COUNT + 1
            CALL MPI_COMM_RANK(COMM2, RESULT, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              FAIL = FAIL + 1
              WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $          ')  From:  MPI_Comm_rank ( COMM_INDEX ', 
     $          COMM_INDEX
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              ERRORSTRING = ' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ELSE IF (RESULT .NE. MPITEST_CURRENT_RANK) THEN
              FAIL = FAIL + 1
              WRITE(INFOBUF,102) 'MPI_Comm_rank returned', RESULT, 
     $          ',  expected', MPITEST_CURRENT_RANK, 
     $          ' Comm_Index = ', COMM_INDEX 
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            END IF
C
C  Check the size of the new communicator
C
            CALL MPI_COMM_SIZE(COMM, COMMSIZE, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,102) 'Non-Zero return code (', IERR,  
     $          ')  From:  MPI_Comm_size #1 ( COMM_INDEX ', 
     $          COMM_INDEX
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              ERRORSTRING = ' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF
C
            CALL MPI_COMM_SIZE(COMM2, RESULT, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $          ')  From:  MPI_Comm_size #2 ( COMM_INDEX ', 
     $          COMM_INDEX
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              ERRORSTRING = ' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
              WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              FAIL = FAIL + 1
            END IF
            LOOP_COUNT = LOOP_COUNT + 1
C
            IF(RESULT .NE. COMMSIZE)  THEN
              WRITE(INFOBUF,102) 'MPI_Comm_size  returned', RESULT, 
     $          ',  expected', COMMSIZE, 
     $          '  Comm_Index = ', COMM_INDEX 
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            END IF
            LOOP_COUNT = LOOP_COUNT + 1
C
C  Get the value for the attribute; it should NOT be there
C
            CALL MPI_ATTR_GET( COMM2,
     $                         KEY,
     $                         RESULT,
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
     $        'MPI_Attr_get found attribute in created comm', 
     $        ' it should not(', 'COMM_INDEX', COMM_INDEX, ')' 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
C  Try the new communicator
C
          IF(MPITEST_CURRENT_RANK  .EQ. 0) RESULT = 127
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
          ELSE IF  (RESULT .NE. 127) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,102) 
     $        'MPI_Bcast received incorrect data, Actual = ', RESULT,
     $        '  127 for  COMM_INDEX = ', COMM_INDEX 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
C  Free the group
C
          CALL MPI_GROUP_FREE(GROUP, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_GROUP_FREE ( COMM_INDEX ', 
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
          IF(GROUP .NE. MPI_GROUP_NULL)  THEN
            WRITE(INFOBUF,105) 
     $        'Group = ', GROUP,  ' after MPI_Group_free', 
     $        ',  expected MPI_GROUP_NULL ( Comm_Index = ',
     $        COMM_INDEX, ')' 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
C
C  Try something again with the first group free
C
          CALL MPI_BARRIER(COMM2, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_BARRIER ( COMM_INDEX ', 
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
C  Free the Created Communicator
C
          CALL MPI_COMM_FREE(COMM2, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_COMM_FREE ( COMM_INDEX ', 
     $        COMM_INDEX
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
        ELSE 
C                            else for not inter-communicator 
C                            ( do following if inter-communicator)
          CALL MPI_GROUP_FREE(GROUP, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From:  MPI_GROUP_FREE ( COMM_INDEX ', 
     $        COMM_INDEX
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          ELSE IF ( GROUP .NE. MPI_GROUP_NULL) THEN
            WRITE(INFOBUF,105)
     $        'Group = ', GROUP,  ' after MPI_Group_free', 
     $        ',  expected MPI_GROUP_NULL ( Comm_Index = ', 
     $        COMM_INDEX, ')' 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1
        END IF
      END IF
C                End of IF inter-communicator
C
C  Delete the Attribute
C
      CALL MPI_ATTR_DELETE(COMM, KEY, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_ATTR_DELETE ( COMM_INDEX ', 
     $      COMM_INDEX
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
C
      CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
 3800 CONTINUE
C                Node defined in current communicator
 4000 CONTINUE
C                End communicator loop
C
C  Free the Keyval
C
      CALL   MPI_KEYVAL_FREE(KEY, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_Keyval_free'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      END IF
C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_COUNT - FAIL, FAIL, 0, TESTNAME)
C      
      CALL MPI_FINALIZE(IERR)
C
      END
