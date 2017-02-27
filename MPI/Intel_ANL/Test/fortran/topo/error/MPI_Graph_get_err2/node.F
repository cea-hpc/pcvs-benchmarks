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
C                         MPI_GRAPH_GET_err2
C
C  Reference:  MPI Standard,  Chapter 6:  Process Topologies
C                             Section 6.5.4:  Topology inquiry functions
C
C  MPI_Graph_get error tests
C  -----------------------------------
C  1)  Call with non-graphic communicator(MPI_COMM_WORLD)  [TOPOLOGY]
C  2)  Call with MPI_COMM_NULL ........................... [COMM]
C  3)  Call with Cartesian communicator .................. [TOPOLOGY]
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
      PARAMETER( TEST_DESC = 'MPI_GRAPH_GET: call with non-graph  comm')

C                           counts total number of PASSes/FAILures/VERIFY
      INTEGER PASS
      INTEGER FAIL              
C                           return value from MPI calls
      INTEGER IERR
      INTEGER ERR
C                           error class of IERR
      INTEGER ERRORCLASS
      INTEGER SIZE 

      INTEGER I, J
C                           general loop indices
      INTEGER MAXINDEX
C                           length of index vector
      INTEGER MAXEDGES
C                           length of edges vector
      INTEGER GRAPH_NODES(2)
C                           number of nodes from GRAPHDIMS_GET

      CHARACTER*(IOSIZE)  INFOBUF
C                           buffer for passing messages to MPITEST
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
C                           string describing error flag
      CHARACTER*32   TESTNAME
C                           name of this test
      INTEGER  GRAPH_EDGES(2) 
C                           graph edges from Graph_get
C-----------------------------------------------------------------------------
 98   FORMAT(A,A)
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT)
 102  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A )
 103  FORMAT(A, INT_FMT, A, A)
C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      TESTNAME='MPI_GRAPH_GET_err2'
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

      IF (MPITEST_ME .EQ. 0) THEN

C    ------------------------- MPI_Graph_get   ------------------------


        MAXINDEX = 2
        MAXEDGES = 2

        CALL MPI_GRAPH_GET(MPI_COMM_NULL,
     $                     MAXINDEX,
     $                     MAXEDGES,
     $                     GRAPH_NODES,
     $                     GRAPH_EDGES,
     $                     IERR)


        IF (IERR .EQ. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF, 98) TEST_DESC, ' did not FAIL'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ELSE

          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          IF (ERRORCLASS .NE. MPI_ERR_COMM) THEN   

            FAIL = FAIL + 1
            WRITE(INFOBUF,101) TEST_DESC, ' Returned',  ERRORCLASS 

            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            WRITE(INFOBUF,102) 
     $        'Expected MPI_ERR_COMM(', MPI_ERR_COMM, ')' 

            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)

          ELSE
            PASS = PASS + 1
            WRITE(INFOBUF,102) 'IERR = ', IERR, '  ERRORCLASS = ',
     $            ERRORCLASS
            CALL MPITEST_MESSAGE(MPITEST_INFO2, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
          END IF
        END IF
      END IF
      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)
      CALL MPI_FINALIZE(IERR)
      END
