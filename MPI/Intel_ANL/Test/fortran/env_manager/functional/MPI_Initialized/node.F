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
C                   Test for MPI_Initialized()
C 
C This test calls MPI_Initialized() before a call to MPI_Init(), and after, and
C verifies the proper return value.
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
      INTEGER LOOP_CNT
      INTEGER FAIL              
C                           return value from MPI calls
      INTEGER SAVEDIERR
      INTEGER IERR
      INTEGER ERR
      LOGICAL FLAG
C                           error class of IERR
      INTEGER ERRORCLASS
      INTEGER ERRSIZE 

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
      CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING


 98   FORMAT(A,INT_FMT,A)
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)


      CALL MPI_INITIALIZED(FLAG, SAVEDIERR)
C  Can't process errors until after we call MPITEST_INIT
C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TESTNAME='MPI_Initialized'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      LOOP_CNT = 2
      FAIL = 0

      IF (SAVEDIERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99)
     $     'MPI_INITIALIZED() before MPI_INIT return code', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      IF (FLAG .NEQV. .FALSE.) THEN
        WRITE(INFOBUF, 100)
     $      'MPI_INITIALIZED() before MPI_INIT returned .TRUE.'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF


      CALL MPI_INITIALIZED(FLAG, SAVEDIERR)
      IF (SAVEDIERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99)
     $     'MPI_INITIALIZED() after MPI_INIT return code', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
        FAIL = FAIL + 1
      END IF
      IF (FLAG .NEQV. .TRUE.) THEN
        WRITE(INFOBUF, 100)
     $      'MPI_INITIALIZED() after MPI_INIT returned .FALSE.'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
C
C  Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT-FAIL, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
