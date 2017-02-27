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
C                  Test for MPI_Finalize()
C
C This test calls MPI_Finalize(), then attempts to call MPI_Init() and
C MPI_Barrier() afterwards.  These should not succeed.
C 
C Test history:
C    1  05/02/96     gt   Created
C
C **********************************************************************
#include "foptions.h"


      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

C                           counts total number of PASSes/FAILures/VERIFY
      INTEGER FAIL              
C                           return value from MPI calls
      INTEGER IERR
      INTEGER ERR
C                           error class of IERR
      INTEGER ERRORCLASS
      INTEGER ERRSIZE 

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING


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

      FAIL = 0

      TESTNAME='MPI_Finalize'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
        INFOBUF =
     $    'This test will print the verify message, call MPI_Finalize()'
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
        INFOBUF =
     $    'Then attempt to call MPI_Init() again.  This should fail.'
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF


C
C  Report overall results
C
      CALL MPITEST_REPORT(0, 0, 1, TESTNAME)

      CALL MPI_FINALIZE(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_FINALIZE() FAILED, returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      CALL MPI_INIT(IERR)
      IF (IERR .EQ. MPI_SUCCESS) THEN
        INFOBUF =
     $    'MPI_INIT() after MPI_FINALIZE FAILED, reurned MPI_SUCCESS'
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
      IF (IERR .EQ. MPI_SUCCESS) THEN
        INFOBUF =
     $    'MPI_BARRIER() after MPI_FINALIZE FAILED, reurned MPI_SUCCESS'
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      END
