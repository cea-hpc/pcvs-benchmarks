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
C  		  Test for MPI keyvals and attributes.
C  
C  This test creates MPI Keyvals (create and free), and
C  assigns attributes (get, put, delete) to them on various communicator 
C  types as provided bt the MPITEST environment.
C  
C  It tests: 1) User copy function, flag=1 / user delete function
C            2) MPI_NULL_COPY_FN           / user delete function
C            3) MPI_DUP_FN                 / user delete function
C            4) User copy function, flag=0 / MPI_NULL_DELETE_FN
C  on the Keyval_create call, calling Attr_get, Attr_put and Attr_free on
C  each communicator and a dup'ed communicator and checking for the appropriate
C  results of each.  The extra_state is incremented in each user-defined
C  callback function, to validate that it was indeed called.
C  
C  
C  Test history:
C     1  08/28/96     gt       Original version
C  
C NOTE:  9/96
C    The MPI Standard(6/95), Section 5.7.1(Page 169)  gives a prototype
C    for a Copy callback function (SUBROUTINE COPY_FUNCTION), and
C    a Callback deletion function (SUBROUTINE DELETE_FUNCTION).
C    The last formal parameter in both of these subroutines is an
C    INTEGER error variable, IERR.  This variable is not in the INTEL MPI
C    code, and when run with the IERR parameter included in the declaration,
C    the program crashes.  This program references Callback-function
C    calls to subroutines at the end of this file 
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
C
      CHARACTER*32          TESTNAME
      PARAMETER( TESTNAME = 'MPI_KEYVAL1')
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
      INTEGER    PASS
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
      INTEGER    ATTR
C                               MPI_Key
      INTEGER    COMM_SIZE
C                               communicator size
      INTEGER    EXTRA1
      INTEGER    EXTRA2
      INTEGER    EXTRA3
      INTEGER    EXTRA4
C                               for keyval creation/use
      INTEGER    KEY1, VALUE1
      INTEGER    KEY2, VALUE2
      INTEGER    KEY3, VALUE3
      INTEGER    KEY4, VALUE4


      EXTERNAL   COPY_FUNCTION1, COPY_FUNCTION2
      EXTERNAL   DELETE_FUNCTION
C                               for Keval_create call
      

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
     $        COMM_SIZE)
C
C       Skip to end of loop if this node is not part of current
C       communicator
C
        IF (MPITEST_CURRENT_RANK .EQ. MPI_UNDEFINED ) GO TO 3800

        EXTRA1 = 0
        EXTRA2 = 0
        EXTRA3 = 0
        EXTRA4 = 0
C
C-------------------------  Create Keyval #1  ----------------------------
C

        CALL   MPI_KEYVAL_CREATE(COPY_FUNCTION1,
     $                           DELETE_FUNCTION,
     $                           KEY1,
     $                           EXTRA1,
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,103) 'Non-Zero return code (', IERR, 
     $     ')  From:  MPI_KEYVAL_CREATE #1'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
C
C     Keyval exists, but no attribute set yet, so expect found = 0
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY1,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY1) before put ',
     $      ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY1) reported KEY1 found before ', 
     $      'initialise   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Set a value for the attribute
C
        VALUE1 = MPITEST_ME
C
        CALL MPI_ATTR_PUT(COMM, 
     $                    KEY1, 
     $                    VALUE1, 
     $                    IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_ATTR_PUT(KEY1)   ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        END IF
C
C     Get the value of the attribute
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY1,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY1) AFTER put ',
     $      ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .TRUE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY1) reported KEY1 NOT found after ', 
     $      'initialised   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (ATTR .NE. VALUE1) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY1) VALUE =  ',  ATTR,
     $      '  Expected =', VALUE1,
     $      '  (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Dup the Communicator, attribute should follow
C
        CALL MPI_COMM_DUP(COMM, 
     $                    COMM2, 
     $                    IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_DUP ( COMM_INDEX =  ', 
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
C  Get the value for the attribute
C
        CALL MPI_ATTR_GET( COMM2,
     $                     KEY1,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY1) on dup''d communicator '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .TRUE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get reported KEY1, but not found on dup''d ', 
     $      'communicator   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Delete the Attribute on COMM 
C
        CALL MPI_ATTR_DELETE(COMM,
     $                       KEY1, 
     $                       IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_ATTR_DELETE (1)(KEY1)  ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Keyval exists, but no longer is  attribute set yet, so expect found = 0
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY1,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY1) after deleted ',
     $      ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY1) reported KEY1 found after deleted', 
     $      '   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Get the value for the attribute on the dup'ed comm, should still be there
C
        CALL MPI_ATTR_GET( COMM2,
     $                     KEY1,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY1) on dup''ed communicator '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .TRUE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get #2 reported key not found on dup''ed ',
     $      'communicator   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (ATTR .NE. VALUE1) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_Attr_get(KEY1) #2 on dup''ed comm, ATTR =',  
     $      ATTR, '  Expected = ', VALUE1, 
     $      '  ( COMM_INDEX =', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Free the dup'ed  Communicator
C
        CALL MPI_COMM_FREE(COMM2, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_FREE(KEY1)  ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1

C-----------------------------------------------------------------------------
C                             KEYVAL2
C-----------------------------------------------------------------------------
C
C-------------------------  Create Keyval #2  ----------------------------
C

        CALL   MPI_KEYVAL_CREATE(MPI_NULL_COPY_FN,
     $                           DELETE_FUNCTION,
     $                           KEY2,
     $                           EXTRA2,
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,103) 'Non-Zero return code (', IERR, 
     $     ')  From:  MPI_KEYVAL_CREATE #1'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
C
C     Keyval exists, but no attribute set yet, so expect found = 0
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY2,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $          ')  From:  MPI_Attr_get(KEY2) before put ',
     $        ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY2) reported KEY2 found before ', 
     $      'initialise   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Set a value for the attribute
C
        VALUE2 = MPITEST_ME
C
        CALL MPI_ATTR_PUT(COMM, 
     $                    KEY2, 
     $                    VALUE2, 
     $                    IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_ATTR_PUT(KEY2)   ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        END IF
C
C     Get the value of the attribute
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY2,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY2) AFTER put ',
     $      ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .TRUE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY2) reported KEY2 NOT found after ', 
     $      'initialised   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (ATTR .NE. VALUE2) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY2) VALUE =  ',  ATTR,
     $      '  Expected =', VALUE2,
     $      '  (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Dup the Communicator, attribute should NOT follow
C
        CALL MPI_COMM_DUP(COMM, 
     $                    COMM2, 
     $                    IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_DUP (KEY2) (COMM_INDEX =', 
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
C  Get the value for the attribute, it should NOT be found
C     
        CALL MPI_ATTR_GET( COMM2,
     $                     KEY2,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY2) on dup''d communicator '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,101) 
     $      'MPI_Attr_get reported KEY2 found on dup''ed  ', 
     $      'communicator   (COMM_INDEX =', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Delete the Attribute on COMM 
C
        CALL MPI_ATTR_DELETE(COMM,
     $                       KEY2, 
     $                       IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_ATTR_DELETE (2)  ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Keyval exists, but no longer is  attribute set yet, so expect found = 0
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY2,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY2) after deleted ',
     $        ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY2) reported KEY2 found after deleted', 
     $      '   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Get the value for the attribute on the dup'ed comm, 
C     should still NOT be there
C
        CALL MPI_ATTR_GET( COMM2,
     $                     KEY2,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY2) #2 on dup''ed communicator'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,101) 
     $      'MPI_Attr_get #2 reported KEY2  found on dup''ed ',
     $      'communicator   (COMM_INDEX =', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Free the dup'ed  Communicator
C
        CALL MPI_COMM_FREE(COMM2, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_FREE(KEY2)  ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C-----------------------------------------------------------------------------
C                           KEYVAL3
C-----------------------------------------------------------------------------
C
C-------------------------  Create Keyval #3  ----------------------------
C
        CALL   MPI_KEYVAL_CREATE(MPI_DUP_FN,
     $                           DELETE_FUNCTION,
     $                           KEY3,
     $                           EXTRA3,
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,103) 'Non-Zero return code (', IERR, 
     $     ')  From:  MPI_KEYVAL_CREATE #1'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
C
C     Keyval exists, but no attribute set yet, so expect found = 0
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY3,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY3) before put ',
     $      ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY3) reported KEY3 found before ', 
     $      'initialised  (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Set a value for the attribute
C
        VALUE3 = MPITEST_ME
C
        CALL MPI_ATTR_PUT(COMM, 
     $                    KEY3, 
     $                    VALUE3, 
     $                    IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_ATTR_PUT(KEY3)   ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        END IF
C
C     Get the value of the attribute
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY3,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY3) AFTER put ',
     $      ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .TRUE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY3) reported KEY3 NOT found after ', 
     $      'initialised   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (ATTR .NE. VALUE3) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY3) VALUE =  ',  ATTR,
     $      '  Expected =', VALUE3,
     $      '  (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Dup the Communicator, attribute should follow
C
        CALL MPI_COMM_DUP(COMM, 
     $                    COMM2, 
     $                    IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_DUP (COMM_INDEX = ', 
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
C  Get the value for the attribute
C
        CALL MPI_ATTR_GET( COMM2,
     $                     KEY3,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF(IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY3) on dup''ed communicator '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF(FOUND .NEQV.  .TRUE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get reported KEY3, but not found on dup''ed ', 
     $      'communicator   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (ATTR .NE. VALUE3) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY3) VALUE =  ',  ATTR,
     $      '  Expected =', VALUE3,
     $      '  (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Delete the Attribute on COMM 
C
        CALL MPI_ATTR_DELETE(COMM,
     $                       KEY3, 
     $                       IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_ATTR_DELETE(1) (KEY3)  ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Keyval exists, but no longer is  attribute set yet, so expect found = 0
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY3,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY3) after deleted ',
     $      ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY3) reported KEY3 found after deleted', 
     $      '   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Get the value for the attribute on the dup'ed comm, should still be there
C
        CALL MPI_ATTR_GET( COMM2,
     $                     KEY3,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY3) on dup''ed communicator '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .TRUE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get #2 reported key not found on dup''ed ',
     $      'communicator   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (ATTR .NE. VALUE3) THEN
          WRITE(INFOBUF,102) 
     $      'MPI_Attr_get(KEY3) #2 on dup''ed comm, ATTR =',  
     $          ATTR, '  Expected = ', VALUE3, 
     $      '   (COMM_INDEX = ', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Free the dup'ed  Communicator
C
        CALL MPI_COMM_FREE(COMM2, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_FREE(KEY3)  ( COMM_INDEX ', 
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
C---------------------------------------------------------------------------
C                             KEYVAL 4
C---------------------------------------------------------------------------
C
C--------------------------  Create Keyval #4  -----------------------------
C

        CALL   MPI_KEYVAL_CREATE(COPY_FUNCTION2,
     $                           MPI_NULL_DELETE_FN,
     $                           KEY4,
     $                           EXTRA4,
     $                           IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,103) 'Non-Zero return code (', IERR, 
     $     ')  From:  MPI_KEYVAL_CREATE #4'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
C
C     Keyval exists, but no attribute set yet, so expect found = 0
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY4,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $          ')  From:  MPI_Attr_get(KEY4) before put ',
     $        ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY4) reported KEY4 found before ', 
     $      'initialised   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Set a value for the attribute
C
        VALUE4 = MPITEST_ME
C
        CALL MPI_ATTR_PUT(COMM, 
     $                    KEY4, 
     $                    VALUE4, 
     $                    IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_ATTR_PUT(KEY4)   ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        END IF
C
C     Get the value of the attribute
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY4,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY4) AFTER put ',
     $      ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .TRUE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY4) reported KEY4 NOT found after ', 
     $      'initialised   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (ATTR .NE. VALUE4) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY4) VALUE =  ',  ATTR,
     $      '  Expected =', VALUE4,
     $      '  (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Dup the Communicator, attribute should NOT follow
C
        CALL MPI_COMM_DUP(COMM, 
     $                    COMM2, 
     $                    IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_DUP (KEY4) (COMM_INDEX =', 
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
C  Get the value for the attribute, it should NOT be found
C     
        CALL MPI_ATTR_GET( COMM2,
     $                     KEY4,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY4) on dup''d communicator '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF(FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get reported KEY4 found on dup''ed  ', 
     $      'communicator   ( COMM_INDEX =', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Delete the Attribute on COMM 
C
        CALL MPI_ATTR_DELETE(COMM,
     $                       KEY4, 
     $                       IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_ATTR_DELETE (2)  ( COMM_INDEX ', 
     $      COMM_INDEX
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
C
C     Keyval exists, but no longer is  attribute set yet, so expect found = 0
C
        CALL MPI_ATTR_GET( COMM,
     $                     KEY4,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY4) after deleted ',
     $        ' (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,104) 
     $      'MPI_Attr_get(KEY4) reported KEY4 found after deleted', 
     $      '   (', 'COMM_INDEX', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C     Get the value for the attribute on the dup'ed comm, 
C     should still NOT be there
C
        CALL MPI_ATTR_GET( COMM2,
     $                     KEY4,
     $                     ATTR,
     $                     FOUND,
     $                     IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_Attr_get(KEY4) #2 on dup''ed communicator'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE IF  (FOUND .NEQV.  .FALSE.) THEN
          WRITE(INFOBUF,101) 
     $      'MPI_Attr_get #2 reported KEY4  found on dup''ed ',
     $      'communicator   (COMM_INDEX =', COMM_INDEX, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Free the dup'ed  Communicator
C
        CALL MPI_COMM_FREE(COMM2, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_FREE(KEY4)  ( COMM_INDEX ', 
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
C     Free all KEYVALS
C
      CALL   MPI_KEYVAL_FREE(KEY1, 
     $                       IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_Keyval_free (KEY1)',
     $    '  ( COMM_INDEX =',  COMM_INDEX, ')'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      END IF
C
      IF(KEY1 .NE. MPI_KEYVAL_INVALID)  THEN
        WRITE(INFOBUF,101) 
     $    'KEY1 not set to MPI_KEYVAL_INVALID by MPI_KEYVAL_FREE', 
     $    ',  ( Comm_Index = ',  COMM_INDEX, ')' 
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1

      CALL   MPI_KEYVAL_FREE(KEY2, 
     $                       IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_Keyval_free (KEY2)',
     $    '  ( COMM_INDEX =',  COMM_INDEX, ')'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      END IF
C
      IF(KEY2 .NE. MPI_KEYVAL_INVALID)  THEN
        WRITE(INFOBUF,101) 
     $    'KEY2 not set to MPI_KEYVAL_INVALID by MPI_KEYVAL_FREE', 
     $    ',  ( Comm_Index = ',  COMM_INDEX, ')' 
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1

      CALL   MPI_KEYVAL_FREE(KEY3, 
     $                       IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_Keyval_free (KEY3)',
     $    '  ( COMM_INDEX =',  COMM_INDEX, ')'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      END IF
C
      IF(KEY3 .NE. MPI_KEYVAL_INVALID)  THEN
        WRITE(INFOBUF,101) 
     $    'KEY3 not set to MPI_KEYVAL_INVALID by MPI_KEYVAL_FREE', 
     $    ',  ( Comm_Index = ',  COMM_INDEX, ')' 
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1

      CALL   MPI_KEYVAL_FREE(KEY4, 
     $                       IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF,105) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_Keyval_free (KEY4)',
     $    '  ( COMM_INDEX =',  COMM_INDEX, ')'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      END IF
C
      IF(KEY4 .NE. MPI_KEYVAL_INVALID)  THEN
        WRITE(INFOBUF,101) 
     $    'KEY4 not set to MPI_KEYVAL_INVALID by MPI_KEYVAL_FREE', 
     $    ',  ( Comm_Index = ',  COMM_INDEX, ')' 
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1
C
C     Check the number of tines the callback frunctions were called
C     This is different for each
C
C------------------------------------------------------------------------------
      IF(EXTRA1 .NE. 3) THEN
        WRITE(INFOBUF,105) 
     $    'Callback functions called',  EXTRA1,
     $    '  times for KEY1, Expected 3', 
     $    '   ( Comm_Index = ',  COMM_INDEX, ')' 
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1

C------------------------------------------------------------------------------
      IF(EXTRA2 .NE. 1) THEN
        WRITE(INFOBUF,105) 
     $    'Callback functions called',  EXTRA2,
     $    '  times for KEY2, Expected 1', 
     $    '   ( Comm_Index = ',  COMM_INDEX, ')' 
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1
C------------------------------------------------------------------------------
      IF(EXTRA3 .NE. 2) THEN
        WRITE(INFOBUF,105) 
     $    'Callback functions called',  EXTRA3,
     $    '  times for KEY3, Expected 2', 
     $    '   ( Comm_Index = ',  COMM_INDEX, ')' 
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1
C------------------------------------------------------------------------------
      IF(EXTRA4 .NE. 1) THEN
        WRITE(INFOBUF,105) 
     $    'Callback functions called',  EXTRA4,
     $    '  times for KEY4, Expected 1', 
     $    '   ( Comm_Index = ',  COMM_INDEX, ')' 
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1

      CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM)
 3800 CONTINUE
C                Node defined in current communicator
 4000 CONTINUE
C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_COUNT - FAIL, FAIL, 0, TESTNAME)
      
      CALL MPI_FINALIZE(IERR)

      END
C------------------------------------------------------------------------------
      SUBROUTINE COPY_FUNCTION1(OLDCOMM,
     $                          KEYVAL,
     $                          EXTRA_STATE,
     $                          ATTRIBUTE_VAL_IN,
     $                          ATTRIBUTE_VAL_OUT,
     $                          FLAG, IERR)
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
C
      INTEGER  OLDCOMM
      INTEGER  KEYVAL
      INTEGER  EXTRA_STATE
      INTEGER  ATTRIBUTE_VAL_IN
      INTEGER  ATTRIBUTE_VAL_OUT
      INTEGER  IERR
C
      LOGICAL  FLAG
C
      ATTRIBUTE_VAL_OUT = ATTRIBUTE_VAL_IN
      FLAG = .TRUE.
      EXTRA_STATE = EXTRA_STATE + 1
      IERR = MPI_SUCCESS
      RETURN
      END
C------------------------------------------------------------------------------
      SUBROUTINE COPY_FUNCTION2(OLDCOMM,
     $                          KEYVAL,
     $                          EXTRA_STATE,
     $                          ATTRIBUTE_VAL_IN,
     $                          ATTRIBUTE_VAL_OUT,
     $                          FLAG, IERR)
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
C
      INTEGER  OLDCOMM
      INTEGER  KEYVAL
      INTEGER  EXTRA_STATE
      INTEGER  ATTRIBUTE_VAL_IN
      INTEGER  ATTRIBUTE_VAL_OUT
      INTEGER  IERR
C
      LOGICAL  FLAG
C
      FLAG = .FALSE.
      EXTRA_STATE = EXTRA_STATE + 1
      IERR = MPI_SUCCESS
      RETURN
      END
C------------------------------------------------------------------------------
      SUBROUTINE DELETE_FUNCTION(COMM,
     $                           KEYVAL,
     $                           ATTRIBUTE_VAL,
     $                           EXTRA_STATE, IERR)
C
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
C
      INTEGER  COMM
      INTEGER  KEYVAL
      INTEGER  ATTRIBUTE_VAL
      INTEGER  EXTRA_STATE
C
      INTEGER  IERR
C
      EXTRA_STATE = EXTRA_STATE + 1
      IERR = MPI_SUCCESS
      RETURN
      END
