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
C  			  Test for MPI_Group_difference
C  
C  This test verifies that MPI_Group_difference works correctly with
C  itself, MPI_EMPTY_GROUP and the group of COMM_WORLD:
C  
C  (group, group) => empty
C  (empty, group) => empty
C  (group, empty) => group
C  (group, world) => empty
C  
C  Other cases are tested elsewhere.  This test is repeated for all configured
C  communicators.
C  
C  Test history:
C     1  08/28/96     gt       Original version
C 
C **********************************************************************
C
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
      PARAMETER( TESTNAME = 'MPI_GROUP_DIFFERENCE1')
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
      INTEGER    RESULT
      INTEGER    GROUP
      INTEGER    GROUP2
      INTEGER    GROUPWORLD
C                               group handles
C-----------------------------------------------------------------------------
 98   FORMAT(A,A )
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT, A, INT_FMT, A, INT_FMT, A)
 102  FORMAT(A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT)
 103  FORMAT(A, INT_FMT, A, A, A, INT_FMT)
 104  FORMAT(A, INT_FMT, A, A, INT_FMT, A, INT_FMT)
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
      VERIFY = 0
      LOOP_COUNT = 0
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
C Make a Group
C
        CALL MPI_COMM_GROUP(COMM, 
     $                      GROUP, 
     $                      IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_GROUP ( COMM_INDEX ', 
     $      COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C   Do a GROUP_DIFFERENCE with itself
C
        CALL MPI_GROUP_DIFFERENCE(GROUP, 
     $                            GROUP, 
     $                            GROUP2, 
     $                            IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,104) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_DIFFERENCE (GROUP, GROUP)',
     $      '  ( COMM_INDEX ',   COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check the result
C
        IF(GROUP2 .NE. MPI_GROUP_EMPTY) THEN
          WRITE(INFOBUF,101) 
     $      'MPI_GROUP_DIFFERENCE(GROUP,GROUP) did not return' ,
     $      'MPI_GROUP_EMPTY)  ( COMM_INDEX )',   COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C   Do a GROUP_DIFFERENCE with  GROUP_EMPTY
C
        CALL MPI_GROUP_DIFFERENCE(MPI_GROUP_EMPTY, 
     $                            GROUP, 
     $                            GROUP2, 
     $                            IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,104) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_DIFFERENCE (EMPTY, GROUP)',
     $      '  ( COMM_INDEX ',   COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check the result
C
        IF(GROUP2 .NE. MPI_GROUP_EMPTY) THEN
          WRITE(INFOBUF,101) 
     $      'MPI_GROUP_DIFFERENCE(EMPTY,GROUP) did not return' ,
     $      'MPI_GROUP_EMPTY) ( COMM_INDEX )', COMM_INDEX,
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C   Do a reverse DIFFERENCE with  GROUP_EMPTY
C
        CALL MPI_GROUP_DIFFERENCE(GROUP, 
     $                            MPI_GROUP_EMPTY, 
     $                            GROUP2, 
     $                            IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,104) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_DIFFERENCE (GROUP, EMPTY)',
     $      '  ( COMM_INDEX ',   COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        CALL MPI_GROUP_COMPARE(GROUP, 
     $                         GROUP2, 
     $                         RESULT,
     $                         IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,104) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_COMPARE(GROUP, EMPTY)',
     $      '  ( COMM_INDEX ',   COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check the result
C
        IF(RESULT .NE. MPI_IDENT) THEN
          WRITE(INFOBUF,101) 
     $      'MPI_GROUP_DIFFERENCE(GROUP, EMPTY) did not return' ,
     $      'identical group ( COMM_INDEX )',   COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Free the Group
C
        CALL MPI_GROUP_FREE(GROUP2,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE(GROUP, EMPTY)  ( COMM_INDEX ', 
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
C Make a Group with MPI_COMM_WORLD
C
        CALL MPI_COMM_GROUP(COMM, 
     $                      GROUPWORLD, 
     $                      IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_GROUP(WORLD) ( COMM_INDEX ', 
     $      COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C   Do a GROUP_DIFFERENCE with GROUPWORLD
C
        CALL MPI_GROUP_DIFFERENCE(GROUP, 
     $                            GROUPWORLD, 
     $                            GROUP2, 
     $                            IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,104) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_DIFFERENCE (GROUP, WORLD)',
     $      '  ( COMM_INDEX ',   COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check the result
C
        IF(GROUP2 .NE. MPI_GROUP_EMPTY) THEN
          WRITE(INFOBUF,101) 
     $      'MPI_GROUP_DIFFERENCE(GROUP, WORLD) did not return' ,
     $      'MPI_GROUP_EMPTY) ( COMM_INDEX )',   COMM_INDEX,
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Free the groups
C
        CALL MPI_GROUP_FREE(GROUPWORLD,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE for GROUPWORLD ( COMM_INDEX ', 
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
        CALL MPI_GROUP_FREE(GROUP,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE for GROUP ( COMM_INDEX ', 
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
        CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
 3800   CONTINUE
C                Node defined in current communicator
 4000 CONTINUE
C                End communicator loop
C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_COUNT - FAIL, FAIL, 0, TESTNAME)
C      
      CALL MPI_FINALIZE(IERR)
C
      END
