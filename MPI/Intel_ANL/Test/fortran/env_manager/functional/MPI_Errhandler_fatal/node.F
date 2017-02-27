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
C                   Test for MPI_Set_errhandler(MPI_ERRORS_ARE_FATAL)
C 
C The Errhandler for MPI_COMM_WORLD is set to MPI_ERRORS_RETURN and is dup'ed.
C 
C The MPI errorhandler on the dup'ed communicator is set to MPI_ERRORS_ARE_FATAL
C then an illegal call is made.  If the call returns, the test is considered a
C failure.
C
C Test history:
C    1  03/12/96     gt   Created
C
C **********************************************************************
#include "foptions.h"

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

C                           counts total number of PASSes/FAILures/VERIFY
      INTEGER PASS
      INTEGER FAIL              
C                           return value from MPI calls
      INTEGER IERR
      INTEGER ERR
C                           error class of IERR
      INTEGER ERRORCLASS
C                           
      INTEGER SIZE 

      INTEGER ERRH

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
      CHARACTER*32   TESTNAME


C                           MPI communicators
      INTEGER COMM

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

      TESTNAME='MPI_Errhandler_fatal'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
        INFOBUF = 'This test should abort after printing the results'
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
        INFOBUF = 'message, otherwise a f.a.i.l.u.r.e is noted'
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      PASS = 1
      FAIL = 0
C 
C  Set an errorhandler so we get control back.  
C 
      CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD, MPI_ERRORS_RETURN, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 99) ' MPI_ERRORHANDLER_SET #1 returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

C
C  Dup MPI_COMM_WORLD, and make failures FATAL
C
      CALL MPI_COMM_DUP(MPI_COMM_WORLD, COMM, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 99) ' MPI_COMM_DUP returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      CALL MPI_ERRHANDLER_SET(COMM, MPI_ERRORS_ARE_FATAL, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 99) ' MPI_ERRORHANDLER_SET #2 returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

C
C  Report overall results
C
      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)

C Make sure that the message writing completes before we trigger the error 
      CALL MPI_BARRIER( COMM, IERR )
C      
      CALL MPI_SEND(IERR, 1, MPI_INTEGER, MPITEST_NUMP, 0, COMM, IERR)
      INFOBUF = 'Test should have aborted, FAILED'
      CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      FAIL = FAIL + 1


      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
