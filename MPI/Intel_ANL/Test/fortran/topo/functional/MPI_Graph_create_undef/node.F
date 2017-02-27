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
C
C                          MPI_GRAPH_CREATE_undef
C
C Reference:  MPI Standard,  Chapter 6:  Process Topologies
C                            Section 6.5.2
C
C Uses only intra-communicators,  all inter-communicators are skipped
C Must have a minimum of MINNODE nodes in the communicator
C The test program is limited to testing not more than MAXNDIMS dimensions
C
C Model  Test with  reorder = TRUE
C
C For each communicator, we reduce the size of the communicator by 1 for
C construnction of the graph.  Thus, one node should have the status
C of MPI_UNDEFINED in the graph communicator.
C Use MPI_Reduce to transmit the test status of reorder for each rank
C in the communicator.  Each rank compares its rank in the original
C communicator with its rank in the newly created Cartesian communicator.
C It sends zero (0) to node zero of the communicator if the ranks are
C different, and one(1) if the ranks are the same.  It uses the MPI_SUM
C reduce operation.  Thus if the value at node zero of the communicator is
C not equal to the number of ranks in the communicator an error has occurred
C **********************************************************************

#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
C
      INTEGER  MPITEST_MAXDIMS
      PARAMETER  ( MPITEST_MAXDIMS = 4)
C                                      Maximum dimensions to generate
      INTEGER  MPITEST_MINNODE
      PARAMETER  ( MPITEST_MINNODE = 6)
C                                      Minimum nodes required by test
      INTEGER  MPITEST_MAXN
      PARAMETER  ( MPITEST_MAXN = 8)
C                                      Max number of neighbors per node
      CHARACTER  TEST_DESC*(*)
      PARAMETER  ( TEST_DESC = 'MPI_GRAPH_CREATE_undef')
C-----------------------------------------------------------------------------

      INTEGER  CNUMBR
C                           index for communications loop
      INTEGER  COMM_INDEX
C                           the array index of the current comm
      INTEGER  COMM_TYPE
C                           the index of the current communicator type
      INTEGER  COMM_COUNT
C                           loop counter for communicator loop
      INTEGER  COMM_SIZE
C                           number of ranks in the current communicator
      INTEGER  LOOP_CNT
C                           counts total number of failures, loops
      INTEGER  COMM
C                           MPI communicator under test
      INTEGER  ERRSIZE
C                           length of error message
      INTEGER PASS
      INTEGER FAIL              
C                           counts total number of PASSes/FAILures/VERIFY
      INTEGER IERR
      INTEGER ERR
C                           return value from MPI calls
      INTEGER ERRORCLASS
      INTEGER SIZE 
C                           error class of IERR
      INTEGER TEST_VAL
C                           test value to broadcast
      INTEGER I, J, K
C                           general loop indices
      INTEGER NNODES
C                           number of nodes in graph structure
      INTEGER WORLD_NUMP
C                           the number of processors in MPI_COMM_WORLD  
      INTEGER TEST_NUMP
C                           the number of processors in current communicator  
      INTEGER RANK_COMM
C                           rank in the current communicator  
      INTEGER ISDEFINED
C                           undefined node flag
      INTEGER SUM           
C                           sum of values in MPI_REDUCE
      LOGICAL INTER_FLAG
C                           Is comm under test an INTER communicator
      LOGICAL REORDER
C                           flag telling if ranks can be reordered
      CHARACTER*(IOSIZE)  INFOBUF
C                           buffer for passing messages to MPITEST
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
C                           string describing error flag
      CHARACTER*32   TESTNAME
C                           name of this test
      INTEGER COMM_GRAPH
C                           graph communicator
      INTEGER INDEX(MAX_RANKS)
C                           cumulative neighbor count
      INTEGER EDGES(MAX_RANKS * MPITEST_MAXN)
C                           node connections list
C-----------------------------------------------------------------------------
 98   FORMAT(A,A)
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT)
 102  FORMAT(A, INT_FMT, A, INT_FMT, A)
 103  FORMAT(A, INT_FMT, A, A)
C
C     Initialize the MPI environment
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C
C     Initialize the MPITEST environment
C
      CALL MPITEST_INIT(IERR)
      TESTNAME = 'MPI_GRAPH_CREATE_undef'
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF
C
C     Get number of communicators
C
      CALL MPITEST_NUM_COMM_SIZES(COMM_COUNT)
C
C     Can't run if not at least  MPITEST_MINNODE  nodes
C
      CALL MPI_COMM_SIZE(MPI_COMM_WORLD, WORLD_NUMP, IERR)
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
      IF(WORLD_NUMP .LT. MPITEST_MINNODE) THEN
        WRITE(INFOBUF,102) 
     $    'Ranks =', WORLD_NUMP,
     $    ' MPI_GRAPH_CREATE needs at least ', MPITEST_MINNODE
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
      FAIL = 0
      LOOP_CNT = 0
      TEST_VAL = -1
C
C---------------------  Loop over Communicators  ------------------------
C
      DO 4000 CNUMBR = 1,COMM_COUNT
        CALL MPITEST_GET_COMM_INDEX(CNUMBR, COMM_INDEX)
        CALL MPITEST_GET_COMM_TYPE(CNUMBR, COMM_TYPE)
        CALL MPITEST_GET_COMMUNICATOR(COMM_TYPE, COMM_INDEX, COMM,
     $    COMM_SIZE)
C
C       Skip to end of loop if this node is not part of current
C       communicator
        IF (COMM .EQ. MPI_COMM_NULL) GO TO 3400
C
C       Test for INTER-communicator
C
        CALL MPI_COMM_TEST_INTER(COMM, INTER_FLAG, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          INFOBUF=' '
          WRITE(INFOBUF,99) 'MPI_COMM_TEST_INTER() returned', IERR
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          INFOBUF=' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
          FAIL = FAIL + 1
        END IF
        IF(INTER_FLAG  .OR.  COMM_SIZE .LT. MPITEST_MINNODE) GO TO 3000
        CALL MPI_COMM_RANK(COMM,  RANK_COMM, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          WRITE(INFOBUF,102) 'MPI_COMM_RANK Error A (', IERR, 
     $      ')    ERRORCLASS = ', ERRORCLASS
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
C
C  Decide how many dimensions to generate
C
C  --------------  Set up index and edge arrays  ---------------------
        NNODES = COMM_SIZE - 1
C
C assume a single string of nodes with wraparound 
C
        J = 3
C
C set up edges for node 0
C
        EDGES(1) = NNODES - 1
        EDGES(2) = 1
        DO  320 I = 1, NNODES     
          EDGES(J) = I - 1
          EDGES(J + 1) = I + 1
          J = J + 2
 320    CONTINUE 
C
C  set up edges for last node
C
        EDGES(2 * NNODES - 1) = NNODES - 2
        EDGES(2 * NNODES) = 0
      DO 340 I = 1, NNODES
        INDEX(I) = I * 2
  340 CONTINUE
C-----------------------------  MPI_Graph_create  --------------------------*/
      LOOP_CNT = LOOP_CNT + 1
      REORDER  = .TRUE.      
      CALL  MPI_GRAPH_CREATE(COMM,
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
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C##############################################################################

      ISDEFINED = 0
      IF(COMM_GRAPH .EQ. MPI_COMM_NULL) ISDEFINED = 1
C
C     Use reduce to check for one undifined node 
C
      CALL MPI_REDUCE(ISDEFINED,SUM,1,MPI_INTEGER,MPI_SUM,0,COMM,IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
        WRITE(INFOBUF,102) 'MPI_REDUCE ERROR (', IERR, 
     $       ')    ERRORCLASS = ', ERRORCLASS
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
C
C  Node 0 in communicator checks for proper  value for SUM
C
      IF(RANK_COMM .EQ. 0  .AND.  SUM .NE. 1) THEN
        WRITE(INFOBUF,102) 
     $    'ERROR in SUM:  Number of undefined nodes not = 1'
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        WRITE(INFOBUF,102) 
     $    'Reduce Results:  Sum = ', SUM, '  Communicator size =', 
     $    COMM_SIZE
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        FAIL = FAIL + 1
      END IF
C##############################################################################
      IF (COMM_GRAPH .NE. MPI_COMM_NULL) THEN
        CALL MPI_COMM_FREE(COMM_GRAPH, IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          WRITE(INFOBUF,102) 'MPI_COMM_FREE ERROR (', IERR, 
     $       ')    ERRORCLASS = ', ERRORCLASS
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
      END IF
 3000 CONTINUE
      IF(INTER_FLAG) THEN
        WRITE(INFOBUF,102) 'Skipped inter-communicator'
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF
      IF(COMM_SIZE .LT. MPITEST_MINNODE) THEN
        WRITE(INFOBUF,102) 'Skipping:  Communicator size = ',
     $    COMM_SIZE, ' Minimum size = ', MPITEST_MINNODE
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
      END IF
 3400 CONTINUE
C
C     Node defined for this communicator
C
      CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          WRITE(INFOBUF,102) 'MPI_BARRIER Error (', IERR, 
     $       ')    ERRORCLASS = ', ERRORCLASS
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
        CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM, IERR)
 4000 CONTINUE
C
C             End communicator loop
C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)
      CALL MPI_FINALIZE(IERR)
      END
