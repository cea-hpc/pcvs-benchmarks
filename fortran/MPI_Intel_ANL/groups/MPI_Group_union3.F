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
C  			  Test for MPI_Group_union
C  
C  This test verifies that MPI_Group_union works correctly with
C  overlapping groups created from a subset of MPI_COMM_WORLD in both
C  directions.
C  
C  group1 = all ranks EXCEPT 0, 1 and the last, in reverse order.
C  group2 = ranks 2, 0, then every other rank, starting at rank 4, excluding
C  	 the last rank if size is odd.
C  
C  which would contain the following ranks in MPI_COMM_WORLD:
C  # ranks:     4       5        6           7             8             9
C  group1       2      3,2     4,3,2      5,4,3,2      6,5,4,3,2    7,6,5,4,3,2
C  group2      2,0     2,0     2,0,4       2,0,4        2,0,4,6       2,0,4,6
C  
C  What should be an identical group is compared to that created by
C  MPI_Group_union.
C  
C  Test history:
C     1  08/28/96     gt       Original version
C 
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
      PARAMETER( TESTNAME = 'MPI_GROUP_UNION3')
C
C  General variables that occur in all programs
C
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
      INTEGER    I, J
      INTEGER    RESULT
      INTEGER    EXPECT
      INTEGER    WORLDSIZE
      INTEGER    WORLDRANK
      INTEGER    NEWSIZE
C
      INTEGER    GROUP       
C                             result of union of groups 1 and 2
      INTEGER    GROUPMINE
C                            test will create an identical group
      INTEGER    GROUP1
      INTEGER    GROUP2      
      INTEGER    GROUPWORLD
C                                group handles
      INTEGER    RANKS(0:2, 0:2)
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
      FAIL = 0
      VERIFY = 0
      LOOP_COUNT = 0
      DO 260 I = 0,2
        DO 240 J = 0,2
          RANKS(I,J) = 0
 240    CONTINUE
 260  CONTINUE
C
C     Check for minimum required nodes
C
        IF(MPITEST_NUMP .LT. 4) THEN
          WRITE(INFOBUF, 101) 
     $      ' ERROR: not enough ranks to run test, Need  4',
     $      '  You have', MPITEST_NUMP
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
C
C-------------------------------  MPI_Comm_Group  ---------------------------
C
        CALL MPI_COMM_GROUP(MPI_COMM_WORLD, 
     $                      GROUPWORLD, 
     $                      IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_GROUP '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C Create two groups
C
C---------------------------  MPI_Group_range_incl #1  ----------------------
C
        RANKS(0,0) = MPITEST_NUMP - 2
        RANKS(1,0) =  2
        RANKS(2,0) = -1
C
        CALL MPI_GROUP_RANGE_INCL(GROUPWORLD, 
     $                            1, 
     $                            RANKS(0,0), 
     $                            GROUP1, 
     $                            IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_RANGE_INCL #1'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C---------------------------  MPI_Group_range_incl #2  ----------------------
C
        RANKS(0,0) =  2
        RANKS(1,0) =  0
        RANKS(2,0) = -2
        RANKS(0,1) =  4
        RANKS(1,1) = MPITEST_NUMP - 1
        IF(MOD(MPITEST_NUMP,2) .EQ. 1) 
     $                               RANKS(1,1) = RANKS(1,1) - 1
        RANKS(2,1) = 2
C
        IF(MPITEST_NUMP .LT. 6) THEN
          CALL MPI_GROUP_RANGE_INCL(GROUPWORLD, 
     $                              1, 
     $                              RANKS, 
     $                              GROUP2, 
     $                              IERR)
        ELSE
          CALL MPI_GROUP_RANGE_INCL(GROUPWORLD, 
     $                              2, 
     $                              RANKS, 
     $                              GROUP2, 
     $                              IERR)
        END IF
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_RANGE_INCL #2'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C---------------------------  MPI_Group_union #1   --------------------------
C
        CALL MPI_GROUP_UNION(GROUP1, 
     $                       GROUP2, 
     $                       GROUP, 
     $                       IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_UNION #1 (GROUP1, GROUP2) ' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C---------------------------  MPI_Group_range_incl #3  ----------------------
C
        RANKS(0,0) = MPITEST_NUMP - 2
        RANKS(1,0) =  2
        RANKS(2,0) = -1
C
        RANKS(0,1) =  0
        RANKS(1,1) =  0
        RANKS(2,1) =  1
C
        CALL MPI_GROUP_RANGE_INCL(GROUPWORLD, 
     $                            2, 
     $                            RANKS, 
     $                            GROUPMINE, 
     $                            IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_RANGE_INCL #3'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C   Compare Groups #1
C
        CALL MPI_GROUP_COMPARE(GROUPMINE, 
     $                         GROUP,
     $                         RESULT, 
     $                         IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 
     $      'Non-Zero return code (', IERR, 
     $      ')  MPI_GROUP_COMPARE #1 (GROUP1, GROUP2)  ' 
           CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check the result
C
        EXPECT = MPI_IDENT
        IF(RESULT .NE. EXPECT) THEN
          WRITE(INFOBUF,102) 
     $      'Group compare #1 (GROUP1, GROUP2) returned (', RESULT,
     $      ')  not equal to MPI_IDENT (', EXPECT, 
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Check my rank in the new group
C
        CALL MPI_GROUP_RANK(GROUP,
     $                      RESULT,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_RANK #1 (GROUP1, GROUP2) '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Calculate the expected rank and verify
C
        EXPECT = MPITEST_NUMP - MPITEST_ME - 2
 
        IF(MPITEST_ME .EQ. 1  .OR. 
     $                 MPITEST_ME .EQ. MPITEST_NUMP - 1) THEN
          EXPECT = MPI_UNDEFINED
        ELSE IF(MPITEST_ME .EQ. 0) THEN
          EXPECT = MPITEST_NUMP - 3
        END IF
C
        IF(RESULT .NE. EXPECT) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_GROUP_RANK #1 (GROUP1, GROUP2) returned ', RESULT,
     $      '    Expected',   EXPECT
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        CALL MPI_GROUP_FREE(GROUP,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE #1 for (GROUPMINE) '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        IF(GROUPMINE .NE. MPI_GROUP_EMPTY) THEN
          CALL MPI_GROUP_FREE(GROUPMINE,
     $                        IERR)
        END IF
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE  (GROUPMINE) '
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
          LOOP_COUNT = LOOP_COUNT + 1

C------------------------------------------------------------------------------
C                         REVERSE UNION
C------------------------------------------------------------------------------
C
C---------------------------  MPI_Group_union #2   --------------------------
C
        CALL MPI_GROUP_UNION(GROUP2, 
     $                       GROUP1, 
     $                       GROUP, 
     $                       IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_UNION #2 (GROUP2, GROUP1) ' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C---------------------------  MPI_Group_range_incl #4  ----------------------
C
        RANKS(0,0) =  2
        RANKS(1,0) =  0
        RANKS(2,0) = -2
C
        RANKS(0,1) =  4
        RANKS(1,1) =  MPITEST_NUMP - 2
        RANKS(2,1) =  2
C
        IF(MOD(MPITEST_NUMP,2) .EQ. 0) THEN
          RANKS(0,2) = MPITEST_NUMP - 3
        ELSE
          RANKS(0,2) = MPITEST_NUMP - 2
        END IF
        RANKS(1,2) = 3
        RANKS(2,2) = -2
C
        IF(MPITEST_NUMP .EQ. 4)  THEN
          CALL MPI_GROUP_RANGE_INCL(GROUPWORLD, 
     $                              1, 
     $                              RANKS, 
     $                              GROUPMINE, 
     $                              IERR)
        ELSE IF(MPITEST_NUMP .EQ. 5) THEN
          RANKS(2,0) = 3
C
          CALL MPI_GROUP_INCL(GROUPWORLD, 
     $                              3, 
     $                              RANKS, 
     $                              GROUPMINE, 
     $                              IERR)
        ELSE
          CALL MPI_GROUP_RANGE_INCL(GROUPWORLD, 
     $                              3, 
     $                              RANKS, 
     $                              GROUPMINE, 
     $                              IERR)
        END IF
        
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_RANGE_INCL #4'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        CALL MPI_GROUP_COMPARE(GROUP, 
     $                         GROUPMINE, 
     $                         RESULT,
     $                         IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,104) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_COMPARE(GROUP2, GROUP1)'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check the result
C
        IF(RESULT .NE. MPI_IDENT) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_GROUP_COMPARE(GROUP2, GROUP1) Returned' ,
     $      RESULT,  '  Expected  MPI_IDENT '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Check my rank in the new group
C
        CALL MPI_GROUP_RANK(GROUP,
     $                      RESULT,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_RANK  ( GROUP2, GROUP1 ) ' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Calculate the expected rank and verify
C
        EXPECT = MPITEST_NUMP - 2 -  MPITEST_ME/2
        IF(MPITEST_ME .EQ. 2) THEN
          EXPECT = 0
        ELSE IF(MPITEST_ME .EQ. 0) THEN
          EXPECT = 1
        ELSE IF(MPITEST_ME .EQ. 1  .OR. 
     $                 MPITEST_ME .EQ. MPITEST_NUMP - 1) THEN
          EXPECT = MPI_UNDEFINED
        ELSE IF(MOD(MPITEST_ME,2) .EQ. 0) THEN
          EXPECT = MPITEST_ME/2
        END IF
C
        IF(RESULT .NE. EXPECT) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_GROUP_RANK #2 (GROUP2, GROUP1) returned ', RESULT,
     $      '    Expected',   EXPECT
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Free the Groups
C
        CALL MPI_GROUP_FREE(GROUP,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE(GROUP) '

          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        CALL MPI_GROUP_FREE(GROUPMINE,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE(GROUPMINE)'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        CALL MPI_GROUP_FREE(GROUPWORLD,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE(GROUPWORLD)'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        CALL MPI_GROUP_FREE(GROUP1,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE(GROUP1)'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
        CALL MPI_GROUP_FREE(GROUP2,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE(GROUP2)'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_COUNT - FAIL, FAIL, 0, TESTNAME)
      
      CALL MPI_FINALIZE(IERR)

      END
