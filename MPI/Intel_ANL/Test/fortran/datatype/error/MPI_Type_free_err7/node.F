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
C                      Error test for MPI_TYPE_FREE()
C 
C This test verifies that the correct error is returned if MPI_TYPE_FREE()
C is called with an invalid argument.
C 
C MPI_TYPE_FREE error tests
C -----------------------------------
C 1)  Call with MPI_DATTYPE_NULL......................[MPI_ERR_TYPE]
C 2)  Call with basic (MPI_BYTE) datatype.............[MPI_ERR_TYPE]
C 3)  Call with basic (MPI_CHARACTER) datatype........[MPI_ERR_TYPE]
C 4)  Call with basic (MPI_DOUBLE_PRECISION) datatype.[MPI_ERR_TYPE]
C 5)  Call with basic (MPI_REAL) datatype.............[MPI_ERR_TYPE]
C 6)  Call with basic (MPI_INTEGER) datatype..........[MPI_ERR_TYPE]
C 7)  Call with basic (MPI_PACKED) datatype...........[MPI_ERR_TYPE]
C 8)  Call with basic (MPI_COMPLEX) datatype..........[MPI_ERR_TYPE]
C 9)  Call with basic (MPI_DOUBLE_COMPLEX) datatype...[MPI_ERR_TYPE]
C 10) Call with basic (MPI_LOGICAL) datatype..........[MPI_ERR_TYPE]
C 
C In all cases, expect to receive appropriate error.
C
C Rank 0 will call MPI_Type_free() with a basic datatype.
C The resulting error code will then be checked and the corresponding error
C class will be verified to make sure it is MPI_ERR_TYPE.
C
C All other rank(s) will simply do nothing.
C 
C MPI Calls dependencies for this test:
C   MPI_Type_free(), MPI_Init(), MPI_Finalize(), MPI_Error_string(), 
C   [MPI_Allreduce(), MPI_Comm_rank(), MPI_Comm_size()]
C
C Test history:
C    1  07/22/96     simont       Original version
C ******************************************************************************/
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

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32        TESTNAME

      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING

      INTEGER OLDTYPE

 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 150  FORMAT(A, INT_FMT, A, INT_FMT)
 170  FORMAT(A, A, INT_FMT, A)
 180  FORMAT(A, A)

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

      TESTNAME='MPI_Type_free_err7'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      PASS = 0
      FAIL = 0

C     Set an errhandler so we get control back.
C
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

      SOURCE = 0

      IF (MPITEST_ME .EQ. SOURCE) THEN
         OLDTYPE = MPI_PACKED

C        MPI_TYPE_FREE with Basic (MPI_PACKED) type
C
         CALL MPI_TYPE_FREE(OLDTYPE, IERR2)
         IF (IERR2 .EQ. MPI_SUCCESS) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,180)
     $         'MPI_TYPE_FREE() with Basic (MPI_PACKED) ',
     $         'type returned MPI_SUCCESS'
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
            ELSE IF (ERRORCLASS .NE. MPI_ERR_TYPE) THEN
               FAIL = FAIL + 1
               WRITE(INFOBUF, 170)
     $            'MPI_TYPE_FREE() with Basic (MPI_PACKED) ',
     $            'type returned error class', ERRORCLASS,
     $            ', expected MPI_ERR_TYPE'
               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
               ERRORSTRING=' '
               CALL MPI_ERROR_STRING(IERR2, ERRORSTRING, SIZE, IERR)
               WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ELSE
               PASS = PASS + 1
               WRITE(INFOBUF, 150) 'ierr = ', IERR2, ' ERRORCLASS = ',
     $            ERRORCLASS
               CALL MPITEST_MESSAGE(MPITEST_INFO2, INFOBUF)            
               ERRORSTRING=' '
               CALL MPI_ERROR_STRING(IERR2, ERRORSTRING, SIZE, IERR)
               WRITE(INFOBUF, 100) ERRORSTRING(1:SIZE)
               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            END IF
         END IF
      END IF

C     Report overall result
C
      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END


