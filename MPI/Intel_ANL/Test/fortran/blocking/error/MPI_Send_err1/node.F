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
C                    Error test for MPI_SEND()
C
C This test verifies that the correct error is returned if MPI_Send is
C called with an invalid argument.
C
C 1)  Destination non-existent rank (rank negative)
C 2)  Destination rank greater than group size
C 3)  Call with MPI_COMM_NULL
C 4)  Call with a freed communicator
C 5)  Use a tag < 0
C 6)  Use a tag larger than MPI_TAG_UB
C 7)  Use a negative count argument
C 8)  Use a non-existent data type
C 9)  Use mismatching type signature at sender and receiver
C
C
C In all cases, expect to receive appropriate error.
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

C                           Input to SEND
      INTEGER BUFFER(10)
      INTEGER STATUS(MPI_STATUS_SIZE)
      INTEGER DEST
      LOGICAL FLAG

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

      TESTNAME='MPI_Send_err1'
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
C  Invalid rank (-1)
C
      IERR = 0
      DEST = -1 * (MPITEST_ME + 1)
      IF (DEST .EQ. MPI_PROC_NULL) DEST = DEST - 1
      CALL MPI_SEND(BUFFER, 1, MPI_INTEGER, DEST, 0, MPI_COMM_WORLD,
     $      IERR)

      IF (IERR .EQ. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 98) 'MPI_SEND() w/ rank ', DEST,
     $      ' did not FAIL'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      ELSE
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
        IF (ERRORCLASS .NE. MPI_ERR_RANK) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF, 98) 'MPI_SEND() w/ rank < 0 returned',
     $      ERRORCLASS, ' expected MPI_ERR_RANK'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING=' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ELSE
          PASS = PASS + 1
        END IF
      END IF


C
C  Report overall results
C
      CALL MPITEST_REPORT(PASS, FAIL, VERIFY, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
