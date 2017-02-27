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
C                           Test for MPI_ADDRESS()
C
C All ranks in the current communicator used will allocate a buffer space
C which size of the buffer is determined by the input length arrays (config.h).
C The output address will then be verified so that it will be more than or
C equal to MPI_BOTTOM.
C
C This test may be run in any communicator with a minimum of 1 group members,
C with any data type, and with any non-negative message length.
C
C The MPITEST environment provides looping over communicator size,
C message length.  The properties of the loops are encoded in configuration
C arrays in the file config.h .
C
C MPI Calls dependencies for this test:
C   MPI_INIT(), MPI_FINALIZE(), MPI_ADDRESS(), MPI_ERROR_STRING(),
C   MPI_BARRIER(),
C   [MPI_GET_COUNT(), MPI_ALLREDUCE(), MPI_COMM_RANK(), MPI_COMM_SIZE()]
C
C Test history:
C    1  07/25/96     simont       Original version
C ******************************************************************************/
C
#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER LENGTH_COUNT
      INTEGER MAX_LENGTH_COUNT
      INTEGER LENGTH
      INTEGER FAIL
      INTEGER VERIFY
      INTEGER SIZE
      INTEGER LOOP_CNT
      INTEGER IERR, IERR2
C Note that for he Fortran binding of MPI_Address, the return value must
C be an INTEGER.  Fortran users should use MPI_GET_ADDRESS instead to 
C run on 64-bit platforms.  This code skips the tests if a Fortran 
C integer is smaller than a pointer
      INTEGER OUT_ADDR
      INTEGER SIZEOF_INTEGER

      INTEGER BUFFER(MAX_BUFF_SIZE), I

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32        TESTNAME

      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING

 99   FORMAT(A, INT_FMT)
 100  FORMAT(A200)
 110  FORMAT(A)
 120  FORMAT(A, INT_FMT, A, INT_FMT, A)
 130  FORMAT(A, INT_FMT, A, INT_FMT)

C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF,99) 'MPI_INIT() returned ', IERR 
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ENDIF

      TESTNAME='MPI_Address'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
         INFOBUF = 'Starting test '//TESTNAME
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      ENDIF

      FAIL = 0
      VERIFY = 0
      LOOP_CNT = 0
C
C     Check that we can run this test at all
      CALL MPI_TYPE_SIZE( MPI_INTEGER, SIZEOF_INTEGER, IERR )
      IF (SIZEOF_INTEGER .LT. SIZEOF_POINTER) THEN
         WRITE(INFOBUF, 130) 'Skipping MPI_ADDRESS test because '//
     *   'size of pointer is ', SIZEOF_POINTER, 
     *   ' and size of integer is ', SIZEOF_INTEGER 
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
         GOTO 700
      ENDIF

C     Found out the number of message length in config array
C
      CALL MPITEST_NUM_MESSAGE_LENGTHS(MAX_LENGTH_COUNT)

C     Looping throught each length input
      DO 500 LENGTH_COUNT = 1, MAX_LENGTH_COUNT, 1
         CALL MPITEST_GET_MESSAGE_LENGTH(LENGTH_COUNT, LENGTH)

C        Skipping length 0
C
         IF (LENGTH .EQ. 0) THEN
            IF (MPITEST_ME .EQ. 0) THEN
               WRITE(INFOBUF, 110) 'Skipping length = 0'
               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            END IF
         ELSE IF (LENGTH .GT. MAX_BUFF_SIZE) THEN
            WRITE(INFOBUF, 120) 'Skipping length = ', LENGTH,
     $         ' ( > Maximum buffer limit (', MAX_BUFF_SIZE,
     $         ')'
            CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
         ELSE
C           Print an informational message
C
            IF (MPITEST_ME .EQ. 0) THEN
               WRITE(INFOBUF, 130) '(', LENGTH_COUNT, ') LENGTH ',
     $            LENGTH
               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            END IF

            LOOP_CNT = LOOP_CNT + 1

            DO 600 I = 1, LENGTH, 1
               CALL MPI_ADDRESS(BUFFER(I), OUT_ADDR, IERR)
               IF (IERR .NE. MPI_SUCCESS) THEN
                  WRITE(INFOBUF, 99) 'MPI_ADDRESS() returned ',
     $               IERR
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                  ERRORSTRING=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE,
     $                                  IERR2)
                  WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
                  CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
               ENDIF

               IF (OUT_ADDR .LT. MPI_BOTTOM) THEN
                  VERIFY = VERIFY + 1
                  WRITE(INFOBUF, 120) 'Returned address (',
     $               OUT_ADDR,
     $               ') from MPI_ADDRESS is less than MPI_BOTTOM (',
     $               MPI_BOTTOM, ')'
                  CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
               END IF
 600        CONTINUE
         END IF

#ifdef MPITEST_SYNC
           CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
           IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF, 99) 'MPI_BARRIER() returned ',
     $         IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              ERRORSTRING=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE,
     $                              IERR2)
              WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
           ENDIF
#endif

 500  CONTINUE
C label 700 allows us to exit/skip the loop
 700  CONTINUE

C     Report overall result
C
      CALL MPITEST_REPORT(LOOP_CNT-FAIL-VERIFY, FAIL, VERIFY, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
