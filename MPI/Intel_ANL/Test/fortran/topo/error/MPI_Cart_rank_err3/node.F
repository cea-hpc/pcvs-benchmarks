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
C***********************************************************************
C
C                          MPI_CART_RANK_err3
C
C
C  Reference:  MPI Standard,  Chapter 6:  Process Topologies
C                            Section 6.5.4:  Topology inquiry functions
C
C  MPI_Cart_rank error tests
C  -----------------------------------
C  1)  Communicator with no topology (MPI_COMM_WORLD) .... [TOPOLOGY]
C  2)  Call with MPI_COMM_NULL ........................... [COMM]
C  3)  Call with graph communicator ...................... [TOPOLOGY]
C
C***********************************************************************

#include "foptions.h"

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER    MPITEST_DIMS
      PARAMETER( MPITEST_DIMS = 1)
      CHARACTER  TEST_DESC*(*)
      PARAMETER( TEST_DESC = 'MPI_CART_RANK: with graph communicator')

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
      INTEGER TEST_NUMP
C                           the number of processors in current communicator  
      INTEGER CART_NUMP
C                           number of processors in Cartesian  communicator  
      INTEGER NNODES
C                           number of nodes in graph
      INTEGER  INDEX(2)
C                           graph cumulative connection count array
      INTEGER  EDGES(2)
C                           graph node neighbors list array
      INTEGER RANK_GRAPH
C                           rank in graph communicator
      INTEGER COMM_GRAPH
C                           graph communicator
      LOGICAL REORDER
C                           flag telling if ranks can be reordered
      CHARACTER*(IOSIZE)  INFOBUF
C                           buffer for passing messages to MPITEST
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
C                           string describing error flag
      CHARACTER*32   TESTNAME
C                           name of this test
      INTEGER DIM_SIZE(MPITEST_DIMS)
C                           processes in each dimension
      LOGICAL PERIODS(MPITEST_DIMS)
C                           flag to allow periodic assignment
      INTEGER COORDS(MPITEST_DIMS)
C                           Cartesian coordinates of calling process
C-----------------------------------------------------------------------------
 98   FORMAT(A,A)
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT)
 102  FORMAT(A, INT_FMT, A, INT_FMT, A)
 103  FORMAT(A, INT_FMT, A, A)
C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TESTNAME='MPI_CART_RANK_err3'
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

      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, TEST_NUMP, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
        WRITE(INFOBUF,102) 'MPI_COMM_SIZE Error (', IERR, 
     $       ')    ERRORCLASS = ', ERRORCLASS
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C
C  We need 2 nodes
C
      IF(TEST_NUMP .LT. 2) THEN
      WRITE(INFOBUF,103)  'Ranks =', TEST_NUMP, ' MPI_CART_RANK, ',
     $                                             'needs at least 2'
      CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C
C--------------------------  MPI_GRAPH_CREATE  ------------------------
C
      NUM_DIMS = 1
      NNODES   = 2

      INDEX(1) = 1
      INDEX(2) = 2

      EDGES(1) = 1
      EDGES(2) = 0

      REORDER = .TRUE.

      CALL  MPI_GRAPH_CREATE(MPI_COMM_WORLD,
     $                       NNODES,
     $                       INDEX,
     $                       EDGES,
     $                       REORDER,
     $                       COMM_GRAPH,
     $                       IERR)

      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
        WRITE(INFOBUF,102) 'MPI_GRAPH_CREATE ERROR (', IERR, 
     $       ')    ERRORCLASS = ', ERRORCLASS

        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
      END IF

C   skip if not rank 0 in comm_graph communicator 

      RANK_GRAPH = MAX_RANKS + 1
      IF( COMM_GRAPH .NE. MPI_COMM_NULL) THEN
        CALL MPI_COMM_RANK(COMM_GRAPH,  RANK_GRAPH, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          WRITE(INFOBUF,102) 'MPI_COMM_RANK Error (', IERR, 
     $       ')    ERRORCLASS = ', ERRORCLASS
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
      END IF
      IF (RANK_GRAPH .EQ. 0) THEN
C
C    ------------------------- MPI_CART_RANK   ------------------------
C
        CALL MPI_CART_RANK(COMM_GRAPH, 
     $                   COORDS,
     $                   RANK_GRAPH,
     $                   IERR)

        IF (IERR .EQ. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF, 98) TEST_DESC, ' did not FAIL'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ELSE
          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          IF (ERRORCLASS .NE. MPI_ERR_TOPOLOGY) THEN
            FAIL = FAIL + 1
            WRITE(INFOBUF,101) TEST_DESC, ' Returned',  ERRORCLASS 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            WRITE(INFOBUF,102) 
     $        'Expected MPI_ERR_TOPOLOGY(', MPI_ERR_TOPOLOGY,  ')'
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ELSE
            PASS = PASS + 1
          END IF
        END IF
      END IF
      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)
      CALL MPI_FINALIZE(IERR)
      END
