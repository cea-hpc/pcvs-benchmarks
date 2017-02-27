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
C          Test for MPI keyvals and attributes.
C  
C  This test verifies that errors from copy or delete functions are returned to
C  an application.  It checks that the correct error is returned.
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
      PARAMETER( TESTNAME = 'MPI_KEYVAL3')
C
C  General variables that occur in all programs
C
      INTEGER    LOOP_COUNT
C                               counts total number of failures, loops
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
      INTEGER    COMM1
      INTEGER    COMM2
C                               for split communicator
      INTEGER    VALUE
      INTEGER    ATTR
      INTEGER    KEY
C                               MPI_Key
      INTEGER    EXTRA
C                               for keyval creation/use
      EXTERNAL   COPY_FUNCTION
      EXTERNAL   DELETE_FUNCTION
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

      FAIL = 0
      LOOP_COUNT = 0
      VERIFY = 0
      EXTRA = 0
C
C-------------------------  Create a Keyval  ----------------------------
C

      CALL   MPI_KEYVAL_CREATE(COPY_FUNCTION,
     $                         DELETE_FUNCTION,
     $                         KEY,
     $                         EXTRA,
     $                         IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF,103) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_KEYVAL_CREATE'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      END IF
C
C     Dup the COMM_WORLD, for use as test communicator
C
        CALL MPI_COMM_DUP(MPI_COMM_WORLD, 
     $                    COMM1, 
     $                    IERR)
C
        IF (IERR .NE. MPI_SUCCESS) THEN
          WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $      ')  From:  MPI_COMM_DUP '
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C     
C     Set a value for the attribute
C
      VALUE = 0
C
      CALL MPI_ATTR_PUT(COMM1, 
     $                  KEY,
     $                  VALUE, 
     $                  IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_ATTR_PUT   ', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      END IF
C
C     Try to DUP it, it should not be allowed
C
        CALL MPI_COMM_DUP(COMM1, 
     $                    COMM2, 
     $                    IERR)
C
        IF (IERR .NE. MPI_ERR_OTHER) THEN
          WRITE(INFOBUF,102) 'Unexpected  return code (', IERR, 
     $      ')  From:  MPI_COMM_DUP -- Expected MPI_ERR_OTHER'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        END IF
        LOOP_COUNT = LOOP_COUNT + 1
C
C  Delete the Attribute, should not be allowed
C
      CALL MPI_ATTR_DELETE(COMM1,
     $                     KEY, 
     $                     IERR)
C
      IF (IERR .NE. MPI_ERR_OTHER) THEN
        WRITE(INFOBUF,102) 'Unexpected  return code (', IERR, 
     $    ')  From:  MPI_ATTR_DELETE -- Expected MPI_ERR_OTHER'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1
C
C  Set a NEW value for the attribute, should not be allowed
C
      VALUE = 1
C     
      CALL MPI_ATTR_PUT(COMM1, 
     $                  KEY, 
     $                  VALUE, 
     $                  IERR)

      IF (IERR .NE. MPI_ERR_OTHER) THEN
        WRITE(INFOBUF,102) 'Unexpected  return code (', IERR, 
     $    ')  From:  MPI_ATTR_PUT -- Expected MPI_ERR_OTHER'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1
C
C  Try to free the COMM,  it should not be allowed
C
      CALL MPI_COMM_FREE(COMM1, IERR)
C       WDG-11/03: Formally, an error return from the attribute routines
C       makes the program erroneous (see clarification in MPI 1.2).  
C       Thus, rather than testing
C       for an error return of MPI_ERR_OTHER, we must check only for
C       MPI_SUCCESS
C    
      IF (IERR .EQ. MPI_SUCCESS) THEN
        WRITE(INFOBUF,102) 'Unexpected  return code (', IERR, 
     $    ')  From:  MPI_COMM_FREE -- Expected MPI_ERR_OTHER'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1
C
C  Delete the Attribute
C
      EXTRA = 2
      CALL MPI_ATTR_DELETE(COMM1,
     $                     KEY, 
     $                     IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_ATTR_DELETE after free '

        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
C
C
C  Try to free the COMM,  it should  be allowed
C
      CALL MPI_COMM_FREE(COMM1,
     $                   IERR)
C
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_COMM_FREE(COMM1) '

        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
      LOOP_COUNT = LOOP_COUNT + 1
C
C  Free the Keyval
C
      CALL   MPI_KEYVAL_FREE(KEY,
     $                       IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF,102) 'Non-Zero return code (', IERR, 
     $    ')  From:  MPI_Keyval_free(2)'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      END IF

      IF(KEY .NE. MPI_KEYVAL_INVALID) THEN
        WRITE(INFOBUF,104) 
     $    'Key not set to MPI_KEYVAL_INVALID by MPI_KEYVAL_FREE'
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
C------------------------------------------------------------------------------
      SUBROUTINE COPY_FUNCTION(OLDCOMM,
     $                          KEYVAL,
     $                          EXTRA_STATE,
     $                          ATTRIBUTE_VAL_IN,
     $                          ATTRIBUTE_VAL_OUT,
     $                          FLAG, IERR)
C
      INCLUDE 'mpif.h'
      INTEGER  OLDCOMM
      INTEGER  KEYVAL
      INTEGER  EXTRA_STATE
      INTEGER  ATTRIBUTE_VAL_IN
      INTEGER  ATTRIBUTE_VAL_OUT
      INTEGER  IERR
C
      LOGICAL  FLAG
C
      IERR = MPI_ERR_OTHER
      RETURN
      END

C------------------------------------------------------------------------------
      SUBROUTINE DELETE_FUNCTION(COMM,
     $                           KEYVAL,
     $                           ATTRIBUTE_VAL,
     $                           EXTRA_STATE, IERR)

      INCLUDE 'mpif.h'
      INTEGER  COMM
      INTEGER  KEYVAL
      INTEGER  ATTRIBUTE_VAL
      INTEGER  EXTRA_STATE
      INTEGER  IERR
C
      IF(EXTRA_STATE .EQ. 0) THEN
        IERR = MPI_ERR_OTHER
      ELSE
        IERR = MPI_SUCCESS
      END IF
C
      RETURN
      END

