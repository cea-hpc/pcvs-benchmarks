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
C                  Test for MPI_Errhandler_free
C
C This is the same as the test for MPI_Errhandler_set, except that
C MPI_Errhandler_free is called.  Note that the errorhandler should still
C be used while the communicator is in use.
C 
C MPI_COMM_WORLD is Dup'ed, and each are given a unique errorhandler.
C 
C An error is generated in both comms, plus one with MPI_COMM_NULL (which should
C correspond to MPI_COMM_WORLD's error handler).  This test verifies each
C handler is called correctly.
C
C Test history:
C    1  05/02/96     gt   Created
C
C **********************************************************************
#include "foptions.h"


      INTEGER FUNCTION ERR1(COMMIN, CODE)

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER COMMIN
      INTEGER CODE
      INTEGER IERR

      INTEGER PASS
      INTEGER FAIL
      INTEGER COUNT1
      INTEGER COUNT2
 
      INTEGER COMM

      COMMON /ERRCOM/PASS, FAIL, COUNT1, COUNT2, COMM

      INTEGER CLASS

      CHARACTER*(IOSIZE) INFOBUF
      CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

C
C   Handles errors for MPI_COMM_WORLD
C
      IF (COUNT1 .EQ. 0) THEN
C         Bad length on MPI_SEND
        CALL MPI_ERROR_CLASS(CODE, CLASS, IERR)
        IF (CLASS .NE. MPI_ERR_COUNT) THEN
          INFOBUF='First error returned incorrect code'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE
         PASS = PASS + 1
        END IF
      ELSE IF (COUNT1 .EQ. 1) THEN
C         Bad communicator in MPI_BARRIER
        CALL MPI_ERROR_CLASS(CODE, CLASS, IERR)
        IF (CLASS .NE. MPI_ERR_COMM) THEN
          INFOBUF='Second error returned incorrect code'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE
         PASS = PASS + 1
        END IF
      ELSE
        INFOBUF='Errhandler 1 called too many times'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      IF (COMMIN .NE. MPI_COMM_WORLD) THEN
        INFOBUF='Errhandler 1 called w/ incorrect communicator'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      COUNT1 = COUNT1 + 1
      ERR1 = 0

      END



      INTEGER FUNCTION ERR2(COMMIN, CODE)

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER COMMIN
      INTEGER CODE

      INTEGER IERR
      INTEGER PASS
      INTEGER FAIL
      INTEGER COUNT1
      INTEGER COUNT2
      INTEGER COMM

      COMMON /ERRCOM/PASS, FAIL, COUNT1, COUNT2, COMM

      INTEGER CLASS

      CHARACTER*(IOSIZE) INFOBUF

C
C   Handles errors for Dup'd Comm
C
      IF (COUNT2 .EQ. 0) THEN
C         Bad rank on MPI_SEND
        CALL MPI_ERROR_CLASS(CODE, CLASS, IERR)
        IF (CLASS .NE. MPI_ERR_RANK) THEN
          INFOBUF='Third error returned incorrect code'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          FAIL = FAIL + 1
        ELSE
         PASS = PASS + 1
        END IF
      ELSE
        INFOBUF='Errhandler 2 called too many times'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      IF (COMMIN .NE. COMM) THEN
        INFOBUF='Errhandler 1 called w/ incorrect communicator'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      COUNT2 = COUNT2 + 1
      ERR2 = 0

      END


      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      EXTERNAL ERR1
      EXTERNAL ERR2

C                           counts total number of PASSes/FAILures/VERIFY
      INTEGER PASS
      INTEGER FAIL              
C                           counts number of times errhandler is called
      INTEGER COUNT1
      INTEGER COUNT2

      INTEGER COMM

      COMMON /ERRCOM/PASS, FAIL, COUNT1, COUNT2, COMM

      INTEGER ERRH1
      INTEGER ERRH2
C                           return value from MPI calls
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


C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      PASS = 0
      FAIL = 0

      TESTNAME='MPI_Errhandler_free'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      CALL MPI_COMM_DUP(MPI_COMM_WORLD, COMM, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99)
     $     'MPI_COMM_DUP() non-zero return code', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
      END IF

C
C    Create 2 errorhandlers
C

      CALL MPI_ERRHANDLER_CREATE(ERR1, ERRH1, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99)
     $     'MPI_ERRHANDLER_CREATE() #1 non-zero return code', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
      END IF
      PASS = PASS + 1


      CALL MPI_ERRHANDLER_CREATE(ERR2, ERRH2, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99)
     $     'MPI_ERRHANDLER_CREATE() #2 non-zero return code', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
      END IF
      PASS = PASS + 1

C
C    Set 2 errorhandlers
C

      CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD, ERRH1, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99)
     $     'MPI_ERRHANDLER_SET() #1 non-zero return code', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
      END IF
      PASS = PASS + 1

      CALL MPI_ERRHANDLER_SET(COMM, ERRH2, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99)
     $     'MPI_ERRHANDLER_SET() #2 non-zero return code', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
      END IF
      PASS = PASS + 1

C
C   Free the 2 errhandlers
C
      CALL MPI_ERRHANDLER_FREE(ERRH1, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99)
     $     'MPI_ERRHANDLER_FREE() #1 non-zero return code', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
      ELSE IF (ERRH1 .NE. MPI_ERRHANDLER_NULL) THEN
        INFOBUF= 
     $     'Errhandler #1 .NE. MPI_ERRHANDLER_NULL after FREE call'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      ELSE
        PASS = PASS + 1
      END IF


      CALL MPI_ERRHANDLER_FREE(ERRH2, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF, 99)
     $     'MPI_ERRHANDLER_FREE() #2 non-zero return code', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        INFOBUF = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
      ELSE IF (ERRH2 .NE. MPI_ERRHANDLER_NULL) THEN
        INFOBUF= 
     $     'Errhandler #2 .NE. MPI_ERRHANDLER_NULL after FREE call'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      ELSE
        PASS = PASS + 1
      END IF

C
C   Now create some errors and verify that they work
C

C      MPI_COMM_WORLD
      CALL MPI_SEND(IERR, -1, MPI_INTEGER, 0, 0, MPI_COMM_WORLD, IERR)

C      This should also be handled by MPI_COMM_WORLD
      CALL MPI_BARRIER(MPI_COMM_NULL, IERR)

C      Dup'ed comm
      CALL MPI_SEND(IERR, 1, MPI_INTEGER, MPITEST_NUMP+1, 0, COMM, IERR)

      IF (COUNT1 .NE. 2) THEN
        INFOBUF = 'Errhandler 1 was not called correctly on error'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

      IF (COUNT2 .NE. 1) THEN
        INFOBUF = 'Errhandler 2 was not called correctly on error'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF

C
C  Report overall results
C
      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
