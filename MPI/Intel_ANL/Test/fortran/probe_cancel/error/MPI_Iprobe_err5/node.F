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
C                    Error test for MPI_IPROBE()
C
C This test verifies that the correct error is returned if MPI_IPROBE()
C is called with an invalid argument.
C
C
C Blocking iprobe error tests
C ------------------------------------
C 1). Negative source rank ........... [MPI_ERR_RANK]
C 2)  Negative tag ................... [MPI_ERR_TAG]
C 3)  tag > MPI_TAG_UB ............... [MPI_ERR_TAG]
C 4)  Call with MPI_COMM_NULL ........ [MPI_ERR_COMM]
C 5)  source rank = comm_size  ....... [MPI_ERR_RANK]
C
C In all cases, expect to receive appropriate error.
C
C Test history:
C    1  06/18/96     simont       Original version
C
C *****************************************************************************/
C
#include "foptions.h"

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER DEST
      INTEGER PASS, FAIL
      INTEGER IERR, IERR2
      INTEGER ERRORCLASS
      INTEGER SOURCE
      INTEGER SIZE
      INTEGER COUNT
      INTEGER TAG
      LOGICAL FLAG

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME

      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING

      INTEGER STATUS(MPI_STATUS_SIZE), REQUEST

 98   FORMAT(A,INT_FMT,A)
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 110  FORMAT(A, INT_FMT, A, INT_FMT)
 150  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT)
 160  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A)

C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned ', IERR 
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING=' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
        WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TESTNAME='MPI_Iprobe_err5'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

C     Bad source rank
      SOURCE = MPITEST_NUMP

      PASS = 0
      FAIL = 0

C     Set an errhandler so we get control back.
      CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD, MPI_ERRORS_RETURN,
     $                        IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         FAIL = FAIL + 1
         WRITE(INFOBUF, 99) 'MPI_ERRHANDLER_SET() returned ',
     $        IERR
         CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
         ERRORSTRING=' '
         CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
         WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      DEST = 0
      COUNT = 1
      TAG = 1

C        MPI_IPROBE() call with the too large source rank
         WRITE(INFOBUF, 98)
     $        'Calling MPI_IPROBE() with too large source rank (',
     $        SOURCE, ') ...'
         CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

         CALL MPI_IPROBE(SOURCE, TAG, MPI_COMM_WORLD, FLAG,
     $                   STATUS, IERR2)
         IF (IERR2 .EQ. MPI_SUCCESS) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,98)
     $           'MPI_IPROBE() with too large source rank (', SOURCE,
     $           ') returned MPI_SUCCESS'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
         ELSE
            CALL MPI_ERROR_CLASS(IERR2, ERRORCLASS, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
               FAIL = FAIL + 1
               WRITE(INFOBUF,99) 'MPI_ERROR_CLASS() returned ', IERR 
               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
               ERRORSTRING=' '
               CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
               WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
               CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
            ELSE IF (ERRORCLASS .NE. MPI_ERR_RANK) THEN
               FAIL = FAIL + 1
               WRITE(INFOBUF,160)
     $              'MPI_IPROBE() with too large source rank (',
     $              SOURCE, ') returned error class ', ERRORCLASS,
     $              ', expecting MPI_ERR_RANK (', MPI_ERR_RANK, ')' 
               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
               ERRORSTRING=' '
               CALL MPI_ERROR_STRING(IERR2, ERRORSTRING, SIZE, IERR)
               WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ELSE
               PASS = PASS + 1
               WRITE(INFOBUF, 110)
     $              'ierr = ', IERR2, ', errorclass = ', ERRORCLASS
               CALL MPITEST_MESSAGE(MPITEST_INFO2, INFOBUF)
               ERRORSTRING=' '
               CALL MPI_ERROR_STRING(IERR2, ERRORSTRING, SIZE, IERR)
               WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            END IF 
         END IF

      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
