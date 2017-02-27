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
C  		  Test for MPI_Group_translate_ranks
C  
C  This test verifies that MPI_Group_translate_ranks translates correctly 
C  against another group from an arbitrary commuicator.  Each configured
C  communicator is tested.
C  
C  The following comparisons are made:
C   1 - group against group w/ all ranks reversed
C   2 - group w/ all ranks reversed against group
C   3 - same as #2, but only one rank
C   4 - same as #1, but only one rank
C   5 - self against group w/ all ranks reversed
C   6 - group against MPI_GROUP_EMPTY 
C   7 - group against self (all ranks)
C   8 - group against itself.
C  
C  As a consequence of this test, MPI_Group_size, MPI_Group_rank,
C  MPI_Group_incl, and MPI_Group_free are also tested here.
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
      PARAMETER( TESTNAME = 'MPI_GROUP_TRANSLATE_RANKS')
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
      INTEGER    RANKS1(0:MAX_RANKS)
      INTEGER    RANKS2(0:MAX_RANKS)
C                               array of ranks in group
      INTEGER    RESULT
      INTEGER    EXPECT
      INTEGER    I
C
      INTEGER    COMM_SIZE
      INTEGER    GROUP_SIZE
C
      INTEGER    GROUP
      INTEGER    GROUP2
      INTEGER    GROUPSELF
C                               group handles
C-----------------------------------------------------------------------------
 98   FORMAT(A,A )
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT, A, INT_FMT)
 102  FORMAT(A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT)
 103  FORMAT(A, INT_FMT, A, A, A, INT_FMT)
 104  FORMAT(A, INT_FMT, A, A, INT_FMT)
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
C   Get the size of the current communicator
C
        CALL MPI_COMM_SIZE(COMM, 
     $                     COMM_SIZE, 
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_SIZE #1 ( COMM_INDEX ', 
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
C-------------------------------  MPI_Comm_group  ---------------------------
C
        CALL MPI_COMM_GROUP(COMM, 
     $                      GROUP, 
     $                      IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_GROUP #1 ( COMM_INDEX ', 
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
C   Get the size of the new group
C
        CALL MPI_GROUP_SIZE(GROUP, 
     $                      GROUP_SIZE, 
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_SIZE #1 ( COMM_INDEX ', 
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
C     Reverse the ranks
C
        DO 400 I = 0, GROUP_SIZE-1
          RANKS1 (I) = GROUP_SIZE -1 - I
 400    CONTINUE
C
        CALL MPI_GROUP_INCL(GROUP, 
     $                      GROUP_SIZE, 
     $                      RANKS1, 
     $                      GROUP2, 
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_INCL #1 ( COMM_INDEX ', 
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
C   Check the size of the new group
C
        CALL MPI_GROUP_SIZE(GROUP2, 
     $                      RESULT, 
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_SIZE #2 ( COMM_INDEX ', 
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
        IF(RESULT .NE. GROUP_SIZE) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_Group_size #2 returned (', RESULT,
     $      ')  not equal to Expected (', EXPECT, 
     $      ')   (COMM_INDEX = ', COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Check my rank in the new group
C
        CALL MPI_GROUP_RANK(GROUP2,
     $                      RESULT,
     $                      IERR)
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
C
        IF(RESULT .NE. GROUP_SIZE - 1 - MPITEST_CURRENT_RANK) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_Group_rank returned (', RESULT,
     $      ')   (COMM_INDEX = ', COMM_INDEX, ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Translate all the new ranks
C
        DO 520 I = 0,  GROUP_SIZE-1
          RANKS1(I) = I
 520    CONTINUE
C
C-----------------------  MPI_Group_translate_ranks  -----------------------
C
        CALL MPI_GROUP_TRANSLATE_RANKS(GROUP, 
     $                                 GROUP_SIZE,
     $                                 RANKS1,
     $                                 GROUP2,
     $                                 RANKS2,
     $                                 IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_TRANSLATE_RANKS # 1 ( COMM_INDEX ', 
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
C     Check the translation
C
        VERIFY = 0
        DO 580 I = 0, GROUP_SIZE-1
          IF(RANKS2(I) .NE. GROUP_SIZE - 1 - I) VERIFY = VERIFY + 1
 580    CONTINUE
C
        IF(VERIFY .NE. 0) THEN
          WRITE(INFOBUF,102) 
     $      'Translate_ranks #1 Errors = (', VERIFY,
     $      ')    (COMM_INCEX =',         COMM_INDEX, 
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Translate IN REVERSE
C
C
C-----------------------  MPI_Group_translate_ranks  -----------------------
C
        CALL MPI_GROUP_TRANSLATE_RANKS(GROUP2, 
     $                                 GROUP_SIZE,
     $                                 RANKS1,
     $                                 GROUP,
     $                                 RANKS2,
     $                                 IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_TRANSLATE_RANKS #2 ( COMM_INDEX ', 
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
C     Check the translation
C
        VERIFY = 0
        DO 640 I = 0, GROUP_SIZE-1
          IF(RANKS2(I) .NE. GROUP_SIZE - 1 - I) VERIFY = VERIFY + 1
 640    CONTINUE
C
        IF(VERIFY .NE. 0) THEN
          WRITE(INFOBUF,102) 
     $      'Translate_ranks #2 Errors = (', VERIFY,
     $      ')    (COMM_INCEX =',         COMM_INDEX, 
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Just Translate my own rank
C
        RANKS1(0) = MPITEST_CURRENT_RANK
C
C-----------------------  MPI_Group_translate_ranks  -----------------------
C
        CALL MPI_GROUP_TRANSLATE_RANKS(GROUP2, 
     $                                 1,
     $                                 RANKS1,
     $                                 GROUP,
     $                                 RANKS2,
     $                                 IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_TRANSLATE_RANKS #3 ( COMM_INDEX ', 
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
C     Check the translation
C
        EXPECT = GROUP_SIZE - 1 - MPITEST_CURRENT_RANK
        IF(RANKS2(0) .NE. EXPECT) THEN
          WRITE(INFOBUF,102) 
     $      'Translate_ranks #3 Error:  Actual = (', RANKS2(0),
     $      ')    Expected = (', EXPECT,
     $      ')    (COMM_INCEX =',         COMM_INDEX, 
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Translate against MPI_COMM_SELF
C
C
C-------------------------------  MPI_Comm_group  ---------------------------
C
        CALL MPI_COMM_GROUP(MPI_COMM_SELF, 
     $                      GROUPSELF, 
     $                      IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_GROUP(SELF) ( COMM_INDEX ', 
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
        RANKS1(0) = MPITEST_CURRENT_RANK
C
C-----------------------  MPI_Group_translate_ranks  -----------------------
C
        CALL MPI_GROUP_TRANSLATE_RANKS(GROUP, 
     $                                 1,
     $                                 RANKS1,
     $                                 GROUPSELF,
     $                                 RANKS2,
     $                                 IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_TRANSLATE_RANKS #4 ( COMM_INDEX ', 
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
C     Check the translation
C
        EXPECT = 0
        IF(RANKS2(0) .NE. EXPECT) THEN
          WRITE(INFOBUF,102) 
     $      'Translate_ranks(SELF) #4 Error:  Actual = (', RANKS2(0),
     $      ')    Expected = (', EXPECT,
     $      ')    (COMM_INCEX =',         COMM_INDEX, 
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Translate against MPI_COMM_SELF in Reverse
C
C
        RANKS1(0) = 0
C
C-----------------------  MPI_Group_translate_ranks  -----------------------
C
        CALL MPI_GROUP_TRANSLATE_RANKS(GROUPSELF, 
     $                                 1,
     $                                 RANKS1,
     $                                 GROUP2,
     $                                 RANKS2,
     $                                 IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_TRANSLATE_RANKS #5 ( COMM_INDEX ', 
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
C     Check the translation
C
        EXPECT = GROUP_SIZE - 1 - MPITEST_CURRENT_RANK
        IF(RANKS2(0) .NE. EXPECT) THEN
          WRITE(INFOBUF,102) 
     $      'Translate_ranks(SELF) #4 Error:  Actual = (', RANKS2(0),
     $      ')    Expected = (', EXPECT,
     $      ')    (COMM_INCEX =',         COMM_INDEX, 
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Translate against MPI_GROUP_EMPTY   -- all should be undefined
C
        
        DO 720 I = 0, GROUP_SIZE-1
          RANKS1(I) = I
 720    CONTINUE
C
C-----------------------  MPI_Group_translate_ranks  -----------------------
C
        CALL MPI_GROUP_TRANSLATE_RANKS(GROUP, 
     $                                 GROUP_SIZE,
     $                                 RANKS1,
     $                                 MPI_GROUP_EMPTY,
     $                                 RANKS2,
     $                                 IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_TRANSLATE_RANKS #6 ( COMM_INDEX ', 
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
C     Check the translation
C
        VERIFY = 0
        DO 740 I = 0, GROUP_SIZE-1
          IF(RANKS2(I) .NE. MPI_UNDEFINED) VERIFY = VERIFY + 1
 740    CONTINUE
C
        IF(VERIFY .NE. 0) THEN
          WRITE(INFOBUF,102) 
     $      'Translate_ranks #6 Errors = (', VERIFY,
     $      ')    (COMM_INCEX =',         COMM_INDEX, 
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Translate against MPI_COMM_SELF   -- all UNDEFINED, but my rank
C
        DO 820 I = 0, GROUP_SIZE-1
          RANKS1(I) = I
 820    CONTINUE
C
C-----------------------  MPI_Group_translate_ranks  -----------------------
C
        CALL MPI_GROUP_TRANSLATE_RANKS(GROUP, 
     $                                 GROUP_SIZE,
     $                                 RANKS1,
     $                                 GROUPSELF,
     $                                 RANKS2,
     $                                 IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_TRANSLATE_RANKS #7 ( COMM_INDEX ', 
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
C     Check the translation
C
        VERIFY = 0
        DO 840 I = 0, GROUP_SIZE-1
          IF( I .NE. MPITEST_CURRENT_RANK  .AND.
     $        RANKS2(I) .NE. MPI_UNDEFINED) THEN
            VERIFY = VERIFY + 1
          ELSE IF( I .EQ. MPITEST_CURRENT_RANK  .AND.
     $            RANKS2(I) .NE. 0) THEN
            VERIFY = VERIFY + 1
          END IF            
 840    CONTINUE
C
        IF(VERIFY .NE. 0) THEN
          WRITE(INFOBUF,102) 
     $      'Translate_ranks #7 Errors = (', VERIFY,
     $      ')    (COMM_INCEX =',         COMM_INDEX, 
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C
C     Finally, the group against itself
C
C
        DO 920 I = 0, GROUP_SIZE-1
          RANKS1(I) = group_size - I - 1
 920    CONTINUE
C
C-----------------------  MPI_Group_translate_ranks  -----------------------
C
        CALL MPI_GROUP_TRANSLATE_RANKS(GROUP, 
     $                                 GROUP_SIZE,
     $                                 RANKS1,
     $                                 GROUP,
     $                                 RANKS2,
     $                                 IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_TRANSLATE_RANKS #8 ( COMM_INDEX ', 
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
C     Check the translation
C
        VERIFY = 0
        DO 940 I = 0, GROUP_SIZE-1
          IF(RANKS2(I) .NE. RANKS1(I)) VERIFY = VERIFY + 1
 940    CONTINUE
C
        IF(VERIFY .NE. 0) THEN
          WRITE(INFOBUF,102) 
     $      'Translate_ranks #8 Errors = (', VERIFY,
     $      ')    (COMM_INCEX =',         COMM_INDEX, 
     $      ')'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF

C------------------------------------------------------------------------------
C------------------------------------------------------------------------------
C
C  Free the Groups
C
        CALL MPI_GROUP_FREE(GROUPSELF,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE #1 for GROUPSELF ( COMM_INDEX ', 
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
        CALL MPI_GROUP_FREE(GROUP2,
     $                      IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_GROUP_FREE #2 for GROUP2 ( COMM_INDEX ', 
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
     $      ')  From:  MPI_GROUP_FREE #3 for GROUP ( COMM_INDEX ', 
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
      
      CALL MPI_FINALIZE(IERR)

      END
