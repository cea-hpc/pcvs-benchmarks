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
C                       MPI_Comm_free_err1
C  
C  This test verifies that the correct error is returned if MPI_Comm_free()
C  is called with invalid arguments.
C  
C  MPI_Comm_free error tests
C  -----------------------------------
C  1)  Call with MPI_COMM_NULL.........[MPI_ERR_COMM]
C  2)  Call with MPI_COMM_WORLD........[MPI_ERR_COMM/MPI_ERR_ARG/MPI_ERR_OTHER]
C  3)  Call with MPI_COMM_SELF.........[MPI_ERR_COMM/MPI_ERR_ARG/MPI_ERR_OTHER]
C  
C  In all cases, expect to receive appropriate error.
C  
C  Rank 0 will call MPI_Comm_free with MPI_COMM_NULL.
C  The resulting error code will then be checked and the corresponding
C  error class will be verified to make sure it is MPI_ERR_COMM.
C  
C  All other rank(s) will simply do nothing.
C  
C  MPI Calls dependencies for this test:
C    MPI_Comm_free(), MPI_Init(), MPI_Finalize()
C    MPI_Error_string()
C  
C***********************************************************************

#include "foptions.h"

      PROGRAM MAIN
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
C
      CHARACTER  TEST_DESC*(*)
      PARAMETER( TEST_DESC ='MPI_comm_free: with MPI_COMM_NULL')
C
      INTEGER PASS
      INTEGER FAIL              
C                           counts total number of PASSes/FAILures
      INTEGER IERR
      INTEGER ERR
C                           return value from MPI calls
      INTEGER ERRORCLASS
      INTEGER SIZE 
C                           error class of IERR
      CHARACTER*(IOSIZE)  INFOBUF
C                           buffer for passing messages to MPITEST
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
C                           string describing error flag
      CHARACTER*32   TESTNAME
C                           name of this test
      INTEGER COMM
C                           communicators
C-----------------------------------------------------------------------------
 98   FORMAT(A,A)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT)
 102  FORMAT(A, INT_FMT, A, INT_FMT)
 103  FORMAT(A, INT_FMT)
 104  FORMAT(A, INT_FMT, A)
C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,103) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TESTNAME='MPI_Comm_free_err1'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      PASS = 0
      FAIL = 0
C 
C  Set an errorhandler so we get control back.  
C 
      CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD, MPI_ERRORS_RETURN, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 103) 'MPI_ERRORHANDLER_SET returned ', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      IF(MPITEST_ME .EQ. 0) THEN
        INFOBUF=
     $    'Calling MPI_Comm_free with MPI_COMM_NULL'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
C
C-----------------------  MPI_comm_free  -----------------------------
C

        COMM = MPI_COMM_NULL

        CALL MPI_COMM_FREE(COMM, IERR)
        IF (IERR .EQ. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF, 98) TEST_DESC, ' did not FAIL'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ELSE
          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          IF (ERRORCLASS .NE. MPI_ERR_COMM) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,101) TEST_DESC, ' Returned',  ERRORCLASS 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            WRITE(INFOBUF,104) 
     $        'Expected MPI_ERR_COMM(',   MPI_ERR_COMM, ')'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ELSE
            PASS = PASS + 1
            WRITE(INFOBUF,102) 'IERR = ', IERR, '  ERRORCLASS = ',
     $        ERRORCLASS
            CALL MPITEST_MESSAGE(MPITEST_INFO2, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
          END IF
        END IF
      END IF
      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)
      CALL MPI_FINALIZE(IERR)
      END
