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
C***************************************************************************
C
C                          MPI_DIMS_CREATE_ERR4
C
C
C  Reference:  MPI Standard,  Chapter 6:  Process Topologies
C                             Section 6.5.2:  Cartesian Convenience Function
C
C  MPI_Dims_create error tests
C  -----------------------------------
C  1)  Nodes not a multiple of non-zero dimensions   ..... [DIMS/ARG/OTHER]
C  2)  Negative value in dimensions array  ............... [DIMS/ARG/OTHER]
C  3)  Nodes < 0 ......................................... [ARG/OTHER]
C  4)  Number of dimensions < 0  ......................... [DIMS/ARG/OTHER]
C
C***************************************************************************

#include "foptions.h"

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER    MPITEST_MAXDIMS
      INTEGER    MPITEST_DIMS
      INTEGER    MPITEST_MAXNODE
      PARAMETER( MPITEST_MAXDIMS = 10)
      PARAMETER( MPITEST_DIMS    =  4)
      PARAMETER( MPITEST_MAXNODE = 120)
      CHARACTER  TEST_DESC*(*)
      PARAMETER( TEST_DESC = 'MPI_DIMS_CREATE:  illegal node count')


C                           counts total number of PASSes/FAILures/VERIFY
      INTEGER PASS
      INTEGER FAIL              
C                           return value from MPI calls
      INTEGER IERR
      INTEGER ERR
C                           error class of IERR
      INTEGER ERRORCLASS
      INTEGER SIZE 

      INTEGER I
C                           general loop indices
      INTEGER NUM_DIMS
C                           number of dimensions to generate coordinates for
      INTEGER NNODES
C                           the number of Cartesian processes  

      CHARACTER*(IOSIZE)  INFOBUF
C                           buffer for passing messages to MPITEST
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
C                           string describing error flag
      CHARACTER*32   TESTNAME
C                           name of this test
      INTEGER COMM_CART
C                           Cartesian communicator
      INTEGER DIM_SIZE(MPITEST_MAXDIMS)
C                           processes in each dimension
C-----------------------------------------------------------------------------

 98   FORMAT(A,A)
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT)
 102  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A , INT_FMT)
 103  FORMAT(A, INT_FMT, A, A)
C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TESTNAME='MPI_DIMS_CREATE_ERR4'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      PASS = 0
      FAIL = 0

C 
C  Set an errorhandler so we get control back.  
C 
      CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD, MPI_ERRORS_RETURN, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 99) ' MPI_ERRORHANDLER_SET returned ', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      NUM_DIMS = MPITEST_DIMS
C
C--------------------------  MPI_CART_CREATE  ------------------------
C
      NNODES = 120
      NUM_DIMS = -1

      DIM_SIZE(1) = 4
      DIM_SIZE(2) = 5
      DIM_SIZE(3) = 1
      DIM_SIZE(4) = 6

      CALL MPI_DIMS_CREATE( NNODES,
     $                      NUM_DIMS,
     $                      DIM_SIZE,
     $                      IERR)

      IF (IERR .EQ. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 98) TEST_DESC, ' did not FAIL'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      ELSE
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
        IF (ERRORCLASS .NE. MPI_ERR_DIMS  .AND.   
     $    ERRORCLASS .NE. MPI_ERR_ARG   .AND.   
     $    ERRORCLASS .NE. MPI_ERR_OTHER)  THEN
          
          FAIL = FAIL + 1
          WRITE(INFOBUF,101) TEST_DESC, ' Returned',  ERRORCLASS 
          
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          WRITE(INFOBUF,102) 
     $      'Expected MPI_ERR_DIMS(',   MPI_ERR_DIMS,  
     $      ')  OR  MPI_ERR_ARG(',    MPI_ERR_ARG,  
     $      ')  OR  MPI_ERR_OTHER(',  MPI_ERR_OTHER, ')' 
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ELSE
          PASS = PASS + 1
        END IF
      END IF

      CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
C
C  REPORT OVERALL RESULTS
C
      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END
