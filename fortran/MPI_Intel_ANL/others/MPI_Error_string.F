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
C                   Test for MPI_Error_string()
C 
C This test calls MPI_Error_string() for each valid error class and
C prints it.  You must manually verify that the error string is indeed
C appropriate for the error being tested.
C 
C Test history:
C    1  05/02/96     gt   Created
C
C **********************************************************************
#include "foptions.h"


      SUBROUTINE TEST_ERROR(ERROR, ERRSTR, FAIL)

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER ERROR
      CHARACTER*(*) ERRSTR
      INTEGER FAIL

      INTEGER LENGTH
      INTEGER IERR
      INTEGER ERR
      INTEGER ERRSIZE


      CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING
      CHARACTER*(IOSIZE) INFOBUF

 98   FORMAT(A,INT_FMT,A)
 99   FORMAT(A,INT_FMT)

      ERRORSTRING = ERRSTR
      CALL MPITEST_MESSAGE(MPITEST_VERIFY, ERRORSTRING)
      ERRORSTRING = ' '
      CALL MPI_ERROR_STRING(ERROR, ERRORSTRING, LENGTH, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99) 'MPI_ERROR_STRING() RETURNED', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      ELSE IF (LENGTH .GT. MPI_MAX_ERROR_STRING) THEN
        WRITE(INFOBUF, 98) 'Returned length ', LENGTH, 
     $        'is greater than MPI_MAX_ERROR_STRING'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      ELSE
        INFOBUF = ' '
        WRITE(INFOBUF, 99) 'Returned string length = ', LENGTH
        CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
        CALL MPITEST_MESSAGE(MPITEST_VERIFY, ERRORSTRING)
      END IF

      RETURN

      END



      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

C                           counts total number of PASSes/FAILures/VERIFY
      INTEGER LOOP_CNT
      INTEGER FAIL              
C                           return value from MPI calls
      INTEGER IERR
C                           error class of IERR
      INTEGER ERRORCLASS
      INTEGER LEN           
      INTEGER SIZE 

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
      CHARACTER*32   TESTNAME

      EXTERNAL TEST_ERROR

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

      TESTNAME='MPI_Error_string'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
        INFOBUF = 'Please verify the following error strings'
        CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
      END IF

      LOOP_CNT = 0
      FAIL = 0

      IF (MPITEST_ME .NE. 0) THEN
        CALL MPITEST_REPORT(0, 0, 0, TESTNAME)
      ELSE
        CALL TEST_ERROR(MPI_SUCCESS, 'MPI_SUCCESS', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_BUFFER, 'MPI_ERR_BUFFER', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_COUNT, 'MPI_ERR_COUNT', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_TYPE, 'MPI_ERR_TYPE', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_TAG, 'MPI_ERR_TAG', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_COMM, 'MPI_ERR_COMM', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_RANK, 'MPI_ERR_RANK', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_REQUEST, 'MPI_ERR_REQUEST', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_ROOT, 'MPI_ERR_ROOT', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_GROUP, 'MPI_ERR_GROUP', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_OP, 'MPI_ERR_OP', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_TOPOLOGY, 'MPI_ERR_TOPOLOGY', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_DIMS, 'MPI_ERR_DIMS', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_UNKNOWN, 'MPI_ERR_UNKNOWN', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_TRUNCATE, 'MPI_ERR_TRUNCATE', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_OTHER, 'MPI_ERR_OTHER', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_INTERN, 'MPI_ERR_INTERN', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_IN_STATUS, 'MPI_ERR_IN_STATUS', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        CALL TEST_ERROR(MPI_ERR_PENDING, 'MPI_ERR_PENDING', FAIL)
        LOOP_CNT = LOOP_CNT + 1
        

C
C  Report overall results
C
        CALL MPITEST_REPORT(LOOP_CNT-FAIL, FAIL, LOOP_CNT - FAIL,
     $                      TESTNAME)
      END IF

      CALL MPI_FINALIZE(IERR)

      END
