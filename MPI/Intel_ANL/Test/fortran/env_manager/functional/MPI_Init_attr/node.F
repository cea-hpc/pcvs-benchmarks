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
C                   Test for pre-defined MPI attributes.
C 
C This test calls MPI_Attr_get() on each rank for the following
C pre-defined attributes:
C   MPI_TAG_UB
C   MPI_HOST
C   MPI_WTIME_IS_GLOBAL
C   MPI_IO
C and prints the result.
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
      INTEGER VERIFY
C                           returned attribute, flag
      INTEGER ATTR
      LOGICAL FOUND
C                           return value from MPI calls
      INTEGER IERR
      INTEGER ERR
C                           error class of IERR
      INTEGER ERRORCLASS
      INTEGER LEN           
      INTEGER ERRSIZE 
      INTEGER I

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
      CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING


 98   FORMAT(A,INT_FMT,A)
 99   FORMAT(A,INT_FMT)


C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TESTNAME='MPI_Init_attr'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
        INFOBUF = 'Please verify the following attributes'
        CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
      END IF

      LOOP_CNT = 4
      FAIL = 0
      VERIFY = 0

C
C     MPI_TAG_UB
C
      CALL MPI_ATTR_GET(MPI_COMM_WORLD, MPI_TAG_UB, ATTR, FOUND, IERR)
      DO 200 I=0, MPITEST_NUMP-1
        IF (I .EQ. MPITEST_ME) THEN
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF, 99) 'MPI_ATTR_GET(MPI_TAG_UB) RETURNED', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          ELSE IF (FOUND .NEQV. .TRUE.) THEN
            INFOBUF = 
     $            'MPI_ATTR_GET reported MPI_TAG_UB not found'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          ELSE
            VERIFY = VERIFY + 1
            INFOBUF = ' '
            WRITE(INFOBUF, 99) 'MPI_TAG_UB = ', ATTR
            CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
          END IF
             
        END IF
        CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
 200  CONTINUE


C
C     MPI_HOST
C
      CALL MPI_ATTR_GET(MPI_COMM_WORLD, MPI_HOST, ATTR, FOUND, IERR)
      DO 300 I=0, MPITEST_NUMP-1
        IF (I .EQ. MPITEST_ME) THEN
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF, 99) 'MPI_ATTR_GET(MPI_HOST) RETURNED', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          ELSE IF (FOUND .NEQV. .TRUE.) THEN
            INFOBUF = 
     $            'MPI_ATTR_GET reported MPI_HOST not found'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          ELSE
            VERIFY = VERIFY + 1
            IF (ATTR .EQ. MPI_PROC_NULL) THEN
              INFOBUF = 'MPI_HOST = MPI_PROC_NULL'
            ELSE
              INFOBUF = ' '
              WRITE(INFOBUF, 99) 'MPI_HOST = ', ATTR
            END IF
            CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
          END IF
             
        END IF
        CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
 300  CONTINUE


C
C     MPI_WTIME_IS_GLOBAL
C
      CALL MPI_ATTR_GET(MPI_COMM_WORLD, MPI_WTIME_IS_GLOBAL, ATTR,
     $                  FOUND, IERR)
      DO 400 I=0, MPITEST_NUMP-1
        IF (I .EQ. MPITEST_ME) THEN
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF, 99)
     $          'MPI_ATTR_GET(MPI_WTIME_IS_GLOBAL) RETURNED', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          ELSE IF (FOUND .NEQV. .TRUE.) THEN
            INFOBUF = 
     $            'MPI_ATTR_GET reported MPI_WTIME_IS_GLOBAL not found'
            CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
            VERIFY = VERIFY + 1
          ELSE
            VERIFY = VERIFY + 1
            INFOBUF = ' '
            WRITE(INFOBUF, 99) 'MPI_WTIME_IS_GLOBAL = ', ATTR
            CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
          END IF
             
        END IF
        CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
 400  CONTINUE


C
C     MPI_IO
C
      CALL MPI_ATTR_GET(MPI_COMM_WORLD, MPI_IO, ATTR, FOUND, IERR)
      DO 500 I=0, MPITEST_NUMP-1
        IF (I .EQ. MPITEST_ME) THEN
          IF (IERR .NE. MPI_SUCCESS) THEN
            WRITE(INFOBUF, 99) 'MPI_ATTR_GET(MPI_IO) RETURNED', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          ELSE IF (FOUND .NEQV. .TRUE.) THEN
            INFOBUF = 
     $            'MPI_ATTR_GET reported MPI_IO not found'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            FAIL = FAIL + 1
          ELSE
            VERIFY = VERIFY + 1
            IF (ATTR .EQ. MPI_ANY_SOURCE) THEN
              INFOBUF = 'MPI_IO = MPI_ANY_SOURCE'
            ELSE
              INFOBUF = ' '
              WRITE(INFOBUF, 99) 'MPI_IO = ', ATTR
            END IF
            CALL MPITEST_MESSAGE(MPITEST_VERIFY, INFOBUF)
          END IF
             
        END IF
        CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
 500  CONTINUE



C
C  Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT-FAIL, FAIL, VERIFY, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
