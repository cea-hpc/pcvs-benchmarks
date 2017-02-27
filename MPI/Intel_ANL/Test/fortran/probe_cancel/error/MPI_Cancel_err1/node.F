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
C                          Error test for MPI_CANCEL()
C 
C This test verifies that the correct error is returned if MPI_CANCEL is
C called with an invalid argument.
C 
C MPI_CANCEL error tests
C -----------------------------------
C 1)  Call with inactive request............[MPI_ERR_REQUEST]
C 2)  Call with freed request...............[MPI_ERR_REQUEST]
C 
C In all cases, expect to receive appropriate error.
C 
C Rank 0 will first MPI_Isend() an integer to rank 1.  It will then do a
C an MPI_Wait() making sure that the send has been completed and therefore the
C output request handle becomes inactive.  It then call MPI_CANCEL() with the
C inactive request handle.
C 
C All other rank(s) will simply do nothing.
C 
C MPI Calls dependencies for this test:
C   MPI_ISEND(), MPI_RECV(), MPI_CANCEL(), MPI_INIT(), MPI_FINALIZE()
C   MPI_ERROR_STRING(), 
C   [MPI_ALLREDUCE(), MPI_COMM_RANK(), MPI_COMM_SIZE()]
C 
C Test history:
C    1  06/19/96     simont       Original version
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

      INTEGER BUFFER(10)

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME

      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING

      INTEGER STATUS(MPI_STATUS_SIZE), REQUEST

 99   FORMAT(A,INT_FMT)
 100  FORMAT(A)
 101  FORMAT(A, A)
 110  FORMAT(A, INT_FMT, A, INT_FMT)
 111  FORMAT(A, INT_FMT, A, INT_FMT, A)
 150  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT)
 170  FORMAT(A, A, INT_FMT, A, INT_FMT, A)

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

      TESTNAME='MPI_Cancel_err1'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
         INFOBUF = 'Starting test '//TESTNAME
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      IF (MPITEST_NUMP .LT. 2) THEN
         INFOBUF='This test requires at least 2 ranks'
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
      PASS = 0
      FAIL = 0

C     Set an errhandler so we get control back.
      CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD, MPI_ERRORS_RETURN,
     $                        IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
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
      SOURCE = 1

      IF (MPITEST_ME .EQ. SOURCE) THEN
C        Initialize send data
         BUFFER(1) = 39
C
C        Send data
         WRITE(INFOBUF, 150) 'Sending message from source ',
     $        SOURCE, ' to ', DEST, ', tag: ', TAG
         CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

         CALL MPI_SEND_INIT(BUFFER, COUNT, MPI_INTEGER, DEST, TAG,
     $                  MPI_COMM_WORLD, REQUEST, IERR)
         IF (IERR .NE. MPI_SUCCESS) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,99) 'MPI_ISEND() returned ', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
            WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         WRITE(INFOBUF, 100)
     $        'Waiting for send request to be completed'
         CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

         CALL MPI_START(REQUEST, IERR)
         IF (IERR .NE. MPI_SUCCESS) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,99) 'MPI_START() returned ', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
            WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF
         CALL MPI_WAIT(REQUEST, STATUS, IERR)
         IF (IERR .NE. MPI_SUCCESS) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,99) 'MPI_WAIT() returned ', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
            WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF
C
C        Cancelling an inactive request
C
         WRITE(INFOBUF, 100)
     $        'Cancelling an inactive request ...'
         CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

         CALL MPI_CANCEL(REQUEST, IERR2)
         IF (IERR2 .EQ. MPI_SUCCESS) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF, 101)
     $           'MPI_CANCEL() returned MPI_SUCCESS with ',
     $           'inactive request input'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
         ELSE
            CALL MPI_ERROR_CLASS(IERR2, ERRORCLASS, IERR)

            IF (IERR .NE. MPI_SUCCESS) THEN
               WRITE(INFOBUF,99) 'MPI_ERROR_CLASS() returned ', IERR 
               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
               ERRORSTRING=' '
               CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
               WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
               CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
               FAIL = FAIL + 1
            ELSE IF (ERRORCLASS .NE. MPI_ERR_REQUEST) THEN
               WRITE(INFOBUF, 170)
     $           'MPI_CANCEL() w/ inactive request returned',
     $           'error class ',
     $           ERRORCLASS, ', Expecting MPI_ERR_REQUEST (',
     $           MPI_ERR_REQUEST, ')'
               FAIL = FAIL + 1
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
         CALL MPI_REQUEST_FREE(REQUEST, IERR2)
      ELSE IF (MPITEST_ME .EQ. DEST) THEN
C
C        Receive the message source sent
C
         WRITE(INFOBUF, 150) 'Receiving message from source ',
     $        SOURCE, ' to ', DEST, ', tag: ', TAG
         CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

         CALL MPI_RECV(BUFFER, COUNT, MPI_INTEGER, SOURCE, TAG,
     $                 MPI_COMM_WORLD, STATUS, IERR)
         IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF,99) 'MPI_RECV() returned ', IERR 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
            WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

      END IF

      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
