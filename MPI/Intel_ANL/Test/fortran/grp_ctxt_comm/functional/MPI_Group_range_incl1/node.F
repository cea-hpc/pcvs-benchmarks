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
C  			  Test for MPI_Group_range_incl
C  
C  This test verifies that MPI_Group_range_incl creates a correct group from an
C  arbitrary commuicator.  This test creates a group consisting of the last
C  and first rank of MPI_COMM_WORLD.  This test also verifies the correct
C  operation when n (number of ranges)=0.
C  
C  As a consequence of this test, MPI_Group_size, MPI_Group_rank, MPI_Group_free,
C  MPI_Group_compare, MPI_Comm_create and MPI_Comm_free are also tested here.
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
      PARAMETER( TESTNAME = 'MPI_GROUP_RANGE_INCL1')
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
      INTEGER    COMM2
C                                communicator handle
      INTEGER    GROUP
      INTEGER    GROUPWORLD
C                                 group handles
      INTEGER    RANGES(0:2, 0:2)
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
          RANGES(I,J) = 0
 240    CONTINUE
 260  CONTINUE
C
C     Make a Group comparable to MPI_COMM_WORLD
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
C  Get the size of the application and my rank within
C
        CALL MPI_COMM_RANK(MPI_COMM_WORLD,
     $                     WORLDRANK,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_RANK #1 '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD,
     $                     WORLDSIZE,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_SIZE #1 '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C  Create a new group from the old group consisting of the first and last
C  ranks
C
        NEWSIZE = 1
        IF(WORLDSIZE .GT. 1) NEWSIZE = 2
C
          RANGES(0,0) = WORLDSIZE - 1
          RANGES(1,0) = WORLDSIZE - 1
          RANGES(2,0) = 1
C
          RANGES(0,1) = 0
          RANGES(1,1) = 0
          RANGES(2,1) = 1
C
        IF(WORLDSIZE .LT. 2) THEN
          CALL MPI_GROUP_RANGE_INCL(GROUPWORLD, 
     $                              1, 
     $                              RANGES, 
     $                              GROUP, 
     $                              IERR)
        ELSE
          CALL MPI_GROUP_RANGE_INCL(GROUPWORLD, 
     $                              2, 
     $                              RANGES, 
     $                              GROUP, 
     $                              IERR)
        END IF
C
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
C     Check the size of the new group
C
        CALL MPI_GROUP_SIZE(GROUP,
     $                      RESULT,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_SIZE '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Check the result
C
        IF(RESULT .NE. NEWSIZE) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_GROUP_SIZE  Returned' ,
     $      RESULT,  '  Expected ',  NEWSIZE
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check my rank in the new group
C
        CALL MPI_GROUP_RANK(GROUP,
     $                     RESULT,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_RANK '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
        EXPECT = MPI_UNDEFINED
        IF(WORLDRANK .EQ. WORLDSIZE - 1) THEN
          EXPECT = 0
        ELSE IF(WORLDRANK .EQ. 0) THEN
          EXPECT = 1

        END IF
C
        IF(RESULT .NE. EXPECT) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_GROUP_RANK  Returned' ,
     $      RESULT,  '  Expected ',  EXPECT
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        CALL MPI_GROUP_COMPARE(GROUP, 
     $                         GROUPWORLD, 
     $                         RESULT,
     $                         IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,104) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_COMPARE'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
        IF((WORLDSIZE .EQ. 1  .AND.  RESULT .NE. MPI_IDENT)   .OR.
     $     (WORLDSIZE .EQ. 2  .AND.  RESULT .NE. MPI_SIMILAR) .OR.
     $     (WORLDSIZE .GT. 2  .AND.  RESULT .NE. MPI_UNEQUAL)) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_GROUP_COMPARE  Returned incorrect value' ,
     $      RESULT
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C--------------------------  MPI_Comm_create  ------------------------
C
        CALL MPI_COMM_CREATE(MPI_COMM_WORLD, 
     $                       GROUP,
     $                       COMM2,
     $                       IERR)

        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $        ')  From MPI_COMM_CREATE ' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C-----------------------------------------------------------------------------
        IF(EXPECT .EQ. MPI_UNDEFINED) THEN
C
          IF(COMM2 .NE. MPI_COMM_NULL) THEN
            WRITE(INFOBUF,101) 
     $        'MPI_COMM_CREATE did not return MPI_COMM_NULL to ranks',
     $        'not in new communicator'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          END IF
        ELSE
C
C     Get the size of the application and my rank within
C
        CALL MPI_COMM_RANK(COMM2,
     $                     RESULT,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_RANK #2 '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
         IF(RESULT .NE. EXPECT) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_RANK #2 Returned',  RESULT,
     $      '  Expected', EXPECT
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
        CALL MPI_COMM_SIZE(COMM2,
     $                     RESULT,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_SIZE #2 '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
        IF(RESULT .NE. NEWSIZE) THEN 
          WRITE(INFOBUF,102) 
     $      'MPI_COMM_SIZE #2 Returned',  RESULT,
     $      '  Expected', NEWSIZE
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Try the new communicator
C
        IF(EXPECT .EQ. 0) RESULT = 29
C
C----------------------------  MPI_BCAST  ----------------------------
C
        CALL MPI_BCAST(RESULT, 1, MPI_INTEGER, 0, COMM2, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_BCAST '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF(RESULT .NE. 29) THEN
         WRITE(INFOBUF,102) 
     $      'MPI_BCAST received incorrect data',  RESULT,
     $      '  Expected  29'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1          
C
C     Free the created communicator
C
        CALL MPI_COMM_FREE(COMM2,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_FREE'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
      END IF
C-----------------------------------------------------------------------------
C
C     Free the new group
C
      CALL MPI_GROUP_FREE(GROUP,
     $                    IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_GROUP_FREE)'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1
C
      IF(GROUP .NE. MPI_GROUP_NULL) THEN
        WRITE(INFOBUF,102) 
     $    'After MPI_GROUP_FREE, Group = ',  GROUP,
     $    '  Expected  MPI_GROUP_NULL'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1          
C
C     Now try removing all the ranks
C
      CALL MPI_GROUP_RANGE_INCL(GROUPWORLD, 
     $                          0, 
     $                          RANGES, 
     $                          GROUP, 
     $                          IERR)
C
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_RANGE_INCL #2'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1
C
        IF(GROUP .NE. MPI_GROUP_EMPTY) THEN
          WRITE(INFOBUF,101) 
     $      'After MPI_GROUP_INCL of size 0 did not return ',
     $      'MPI_GROUP_EMPTY'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1          
C
C     Check the size of the new group
C
        CALL MPI_GROUP_SIZE(GROUP,
     $                      RESULT,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_SIZE(empty) '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
        IF(RESULT .NE. 0) THEN 
          WRITE(INFOBUF,102) 
     $      'MPI_GROUP_SIZE(empty)  Returned',  RESULT,
     $      '  Expected  0'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check my rank in  the new group
C
        CALL MPI_GROUP_RANK(GROUP,
     $                      RESULT,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_RANK(empty) '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
        IF(RESULT .NE. MPI_UNDEFINED) THEN 
          WRITE(INFOBUF,102) 
     $      'MPI_GROUP_RANK(empty)  Returned',  RESULT,
     $      '  Expected  MPI_UNDEFINED(', MPI_UNDEFINED, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Free   GROUPWORLD
C
      CALL MPI_GROUP_FREE(GROUPWORLD,
     $                    IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_GROUP_FREE)'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1

      IF(GROUPWORLD .NE. MPI_GROUP_NULL) THEN
        WRITE(INFOBUF,102) 
     $    'After MPI_GROUP_FREE, Groupworld = ',  GROUPWORLD,
     $    '  Expected  MPI_GROUP_NULL'
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
