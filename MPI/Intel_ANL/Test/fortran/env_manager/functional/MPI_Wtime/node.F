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
C                   Test for MPI_Wtime() and MPI_Wtick()
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

C                           counts total number of PASSes/FAILures
      INTEGER PASS
      INTEGER FAIL              
      INTEGER VERIFY
C                           return value from MPI calls
      INTEGER IERR
      INTEGER ERR
      INTEGER ERRSIZE
C                           error class of IERR
      INTEGER ERRORCLASS
      INTEGER LEN           
      INTEGER SIZE 
      INTEGER I

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
      CHARACTER*32   TESTNAME

      DOUBLE PRECISION TIME1
      DOUBLE PRECISION TIME2
      DOUBLE PRECISION TICK

 98   FORMAT(A,INT_FMT,A)
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 102  FORMAT(A,F14.9)
 103  FORMAT(A,F14.9,A)


C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TESTNAME='MPI_Wtime'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      PASS = 0
      FAIL = 0
      VERIFY = 0

      TIME1 = MPI_WTIME()
      TICK = MPI_WTICK()

      DO 200 I = 0, MPITEST_NUMP - 1
        IF (MPITEST_ME .EQ. I) THEN
          INFOBUF=' '
          WRITE(INFOBUF,103)'MPI_WTICK() returned', TICK, ' seconds.'
          CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
        END IF
        CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
 200  CONTINUE
      VERIFY = 1

      TIME2 = MPI_WTIME()

      IF (TIME1 .LT. 0) THEN
        INFOBUF=' '
        WRITE(INFOBUF,102)'MPI_WTIME returned a negative number',TIME1
        CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
        FAIL = FAIL + 1
      END IF

      DO 300 I = 0, MPITEST_NUMP - 1
        IF (MPITEST_ME .EQ. I) THEN
          IF (TIME1 .GE. TIME2) THEN
            INFOBUF=' '
            WRITE(INFOBUF,102)'Successive calls to MPI_WTIME returned',
     $               TIME1
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            WRITE(INFOBUF,102)'expected incremented value, not',TIME2
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          ELSE
            INFOBUF=' '
            WRITE(INFOBUF,102)'Successive calls to MPI_WTIME returned',
     $               TIME1
            CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
            INFOBUF=' '
            WRITE(INFOBUF,102)'and after output above', TIME2
            CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
            PASS = PASS + 1
          END IF
        END IF
        CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
 300  CONTINUE


C
C  Report overall results
C
      CALL MPITEST_REPORT(PASS, FAIL, VERIFY, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
