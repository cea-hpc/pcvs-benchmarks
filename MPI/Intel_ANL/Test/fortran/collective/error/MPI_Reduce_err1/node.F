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
C                    Error test for MPI_REDUCE()
C
C This test verifies that the correct error is returned if MPI_REDUCE()
C is called with an invalid argument.
C
C 1)  Invalid type/operator pairs.
C 2)  Use mis-matched types.
C 3)  Use invalid datatype.
C 4)  Use invalid size (-1).
C 5)  Use invalid rank (-1).
C 6)  Use invalid rank (> num ranks).
C 7)  Use MPI_COMM_NULL.
C 8)  Use a freed communicator.
C 9)  Use an inter-communicator.
C
C In all cases, expect to receive appropriate error.
C
C Test history:
C    1  02/12/96     gt   Created
C
C **********************************************************************
#include "foptions.h"

      SUBROUTINE TEST_OP(TYPE, OP, PASS, FAIL, VERIFY)
C **********************************************************************
C     Test invalid datatype/operation pairs.
C
C **********************************************************************
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER TYPE
      INTEGER OP
      INTEGER PASS
      INTEGER FAIL
      INTEGER VERIFY
      INTEGER INPUT(MAX_RANKS)
      INTEGER OUTPUT(MAX_RANKS)
      INTEGER SIZE
      INTEGER IERR
      INTEGER ERR
      INTEGER ERRORCLASS

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING

 100  FORMAT(A200)
 198  FORMAT(A,INT_FMT,A,INT_FMT,A)
 199  FORMAT(A,INT_FMT,A,INT_FMT,A,INT_FMT,A)

      IERR = 0
      CALL MPI_REDUCE(INPUT, OUTPUT, 1, TYPE, OP, 0, MPI_COMM_WORLD,
     $         IERR)

      IF (IERR .EQ. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 198) 'MPI_REDUCE() w/ type ', TYPE, ' op ',
     $      OP, ' did not FAIL'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      ELSE
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
        IF (ERRORCLASS .NE. MPI_ERR_TYPE .AND. 
     $      ERRORCLASS .NE. MPI_ERR_OP) THEN 
          FAIL = FAIL + 1
          WRITE(INFOBUF, 199) 'MPI_REDUCE() w/ type ', TYPE, ' op ',
     $      OP, ' returned ', ERRORCLASS, ' expected MPI_ERR_TYPE'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING=' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ELSE
          PASS = PASS + 1
        END IF
      END IF


      END

C ***********************************************************************

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      EXTERNAL TEST_OP

C                           counts total number of PASSes/FAILures/VERIFY
      INTEGER PASS
      INTEGER FAIL              
      INTEGER VERIFY
C                           return value from MPI calls
      INTEGER IERR
      INTEGER ERR
C                           error class of IERR
      INTEGER ERRORCLASS
C                           used to split a communicator
      INTEGER COLOR
      INTEGER SIZE 

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
      CHARACTER*32   TESTNAME


C                           MPI communicators
      INTEGER COMM

C                           Input to REDUCE
      INTEGER INPUT(MAX_RANKS)
      INTEGER OUTPUT(MAX_RANKS)

 98   FORMAT(A,INT_FMT,A)
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)


C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TESTNAME='MPI_Reduce_err1'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      PASS = 0
      FAIL = 0
      VERIFY = 0
C 
C  Set an errorhandler so we get control back.  
C 
      CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD, MPI_ERRORS_RETURN, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 99) ' MPI_ERRORHANDLER_SET returned ', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

C
C  Invalid type for operation
C
      CALL TEST_OP(MPI_BYTE, MPI_MAX, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_BYTE, MPI_MAXLOC, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_BYTE, MPI_MIN, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_BYTE, MPI_MINLOC, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_BYTE, MPI_SUM, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_BYTE, MPI_PROD, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_BYTE, MPI_LAND, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_BYTE, MPI_LOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_BYTE, MPI_LXOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_MAX, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_MAXLOC, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_MIN, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_MINLOC, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_LAND, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_LOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_LXOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_BAND, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_BOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_COMPLEX, MPI_BXOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_LOGICAL, MPI_MAX, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_LOGICAL, MPI_MAXLOC, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_LOGICAL, MPI_MIN, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_LOGICAL, MPI_MINLOC, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_LOGICAL, MPI_PROD, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_LOGICAL, MPI_SUM, PASS, FAIL, VERIFY)
c      CALL TEST_OP(MPI_LOGICAL, MPI_BAND, PASS, FAIL, VERIFY)
c      CALL TEST_OP(MPI_LOGICAL, MPI_BOR, PASS, FAIL, VERIFY)
c      CALL TEST_OP(MPI_LOGICAL, MPI_BXOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_INTEGER, MPI_MAXLOC, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_INTEGER, MPI_MINLOC, PASS, FAIL, VERIFY)
c      CALL TEST_OP(MPI_INTEGER, MPI_LAND, PASS, FAIL, VERIFY)
c      CALL TEST_OP(MPI_INTEGER, MPI_LOR, PASS, FAIL, VERIFY)
c      CALL TEST_OP(MPI_INTEGER, MPI_LXOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_REAL, MPI_MAXLOC, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_REAL, MPI_MINLOC, PASS, FAIL, VERIFY)
c      CALL TEST_OP(MPI_REAL, MPI_LAND, PASS, FAIL, VERIFY)
c      CALL TEST_OP(MPI_REAL, MPI_LOR, PASS, FAIL, VERIFY)
c      CALL TEST_OP(MPI_REAL, MPI_LXOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_REAL, MPI_BAND, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_REAL, MPI_BOR, PASS, FAIL, VERIFY)
      CALL TEST_OP(MPI_REAL, MPI_BXOR, PASS, FAIL, VERIFY)

C
C  Report overall results
C
      CALL MPITEST_REPORT(PASS, FAIL, VERIFY, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
