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
C                          MPI_Cart_coords
C
C        Reference:  MPI Standard,  Chapter 6:  Process Topologies
C                                   Section 6.5.4  Topology inquiry functions
C
C Uses only intra-communicators,  all inter-communicators are skipped
C Must have a minimum of MINNODE nodes in the communicator
C The test program is limited to testing not more than MAXNDIMS dimensions
C
C The program Calls the MPI_Cart_coords function for each rank in the
C current Cartesian topology. The coordinates  returned by each call are
C checked for being within limits.
C
C In addition a sum total of index values in each dimension is accumulated.
C The sum value for each dimension is compared with a calculated value.
C
C **********************************************************************

#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'


      INTEGER    MPITEST_MAXDIMS
      PARAMETER  ( MPITEST_MAXDIMS = 4)
C                                      Maximum dimensions to generate
      INTEGER    MPITEST_MINNODE
      PARAMETER  ( MPITEST_MINNODE = 6)
C                                      Mimum nodes required for test
      CHARACTER  TEST_DESC*(*)
      PARAMETER( TEST_DESC = 'MPI_CART_COORDS')

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
C                           return value from MPI calls
      INTEGER IERR
      INTEGER ERR
C                           counts total number of PASSes/FAILures/VERIFY
C                           error class of IERR
      INTEGER ERRORCLASS
      INTEGER SIZE 
C                           counts total number of PASSes/FAILures/VERIFY
      INTEGER TEST_VAL
C                           test value to broadcast
      INTEGER I, J, K, L, M
C                           general loop indices
      INTEGER NNODES
C                           number of nodes in Cartesian structure
      INTEGER NDIMS
C                           number of dimensions to generate coordinates for
      INTEGER CART_DIMS
C                           number of dimensions returned by CARTDIMS_GET
      INTEGER TEST_NUMP
C                           the number of processors in current communicator
      INTEGER CART_NUMP
C                           the number of processors in Cartesian communicator
      INTEGER RANK_CART
C                           rank in the cartesian communicator  
      INTEGER TOTAL   
C                           expected sum of coords
      INTEGER INLIST
C                           number of entries in ALL_COORDS
      LOGICAL INTER_FLAG
C                           is comm under test an INTER communicator
      LOGICAL REORDER
C                           flag telling if ranks can be reordered
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
      LOGICAL PERIODS(MPITEST_MAXDIMS)
C                           flag to allow periodic assignment
      INTEGER COORDS(MPITEST_MAXDIMS)
C                           periodic assignment flag vector
C                           Cartesian dimension sizes
      INTEGER ALL_COORDS(MAX_RANKS, MPITEST_MAXDIMS)
C                           list of all generated coordinates

C-----------------------------------------------------------------------------
 99   FORMAT(A,INT_FMT)
 100  FORMAT(A200)
 102  FORMAT(A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT, A,INT_FMT)
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
      TESTNAME = 'MPI_CART_COORDS'
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
      IF(MPITEST_NUMP .LT. MPITEST_MINNODE) THEN
        WRITE(INFOBUF,102) 
     $    'Ranks =', MPITEST_NUMP,
     $    ' MPI_CART_COORDS needs at least ', MPITEST_MINNODE
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
     $                                                   COMM_SIZE)
C
C       Skip to end of loop if this node is not part of current
C       communicator
C
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
C
C  Decide how many dimensions to generate
C
        NDIMS = MPITEST_MAXDIMS
 1220   CONTINUE
        J = MOD(COMM_SIZE, NDIMS)
        IF( J .NE. 0) THEN
          NDIMS = NDIMS -1
          GO TO 1220
        END IF
        DO 1240 I =1,NDIMS
          DIM_SIZE(I) = 0
 1240   CONTINUE
C
C--------------------------  MPI_DIMS_CREATE  ------------------------
C
        NNODES = COMM_SIZE
        CALL MPI_DIMS_CREATE( NNODES,
     $                        NDIMS,
     $                        DIM_SIZE,
     $                        IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          WRITE(INFOBUF,102) 'MPI_DIMS_CREATE ERROR (', IERR, 
     $      ')    ERRORCLASS = ', ERRORCLASS
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
        DO 280 I = 1, NDIMS
          PERIODS(I) = .TRUE.
 280    CONTINUE
C
C  Adjust dimensions, so last dimension is not 1
C
        I = NDIMS
 300    IF(DIM_SIZE(I) .LE. 1  .AND.  I .GE. 1) THEN
          DIM_SIZE(I) = 0
          I = I - 1
          NDIMS = NDIMS - 1
          GO TO 300
        END IF
C
C--------------------------  MPI_CART_CREATE  ------------------------
C
      REORDER = .TRUE.

      CALL   MPI_CART_CREATE(COMM,
     $                       NDIMS,
     $                       DIM_SIZE,
     $                       PERIODS,
     $                       REORDER,
     $                       COMM_CART,
     $                       IERR)

      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)

        WRITE(INFOBUF,102) 'MPI_CART_CREATE ERROR (', IERR, 
     $       ')    ERRORCLASS = ', ERRORCLASS

        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)

        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
      CALL MPI_COMM_RANK(COMM_CART,  RANK_CART, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
        WRITE(INFOBUF,102) 'MPI_COMM_RANK Error (', IERR, 
     $    ')    ERRORCLASS = ', ERRORCLASS
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
      CALL MPI_COMM_SIZE(COMM_CART, CART_NUMP, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
        WRITE(INFOBUF,102) 'MPI_COMM_SIZE Error (', IERR, 
     $    ')    ERRORCLASS = ', ERRORCLASS
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C
C  Node 0 does all of the CART_COORDS calls and tests
C
      IF(RANK_CART .NE. 0) GO TO 2000
C     Retrieve and test coordinates for all nodes
 
      INLIST = 0
C     Test each coordinate with bounds
      DO 700 I = 1, CART_NUMP
        DO 420 J = 1, MPITEST_MAXDIMS
          COORDS(J) = -1
          ALL_COORDS(I,J) = -1
 420    CONTINUE
C
C    ------------------------- MPI_CART_COORDS   ------------------------
C
        LOOP_CNT = LOOP_CNT + 1
        CALL MPI_CART_COORDS(COMM_CART,
     $                       I-1,
     $                       NDIMS,
     $                       COORDS,
     $                       IERR)
        IF (IERR .NE. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          WRITE(INFOBUF,102) 'MPI_CART_COORDS ERROR (', IERR, 
     $    ')    ERRORCLASS = ', ERRORCLASS
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ERRORSTRING = ' '
          CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
          WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
        END IF
C
C  Test that coordinate is within bounds
C       
        DO 460 J = 1, NDIMS
          IF(COORDS(J) .LT. 0  .OR.  COORDS(J) .GE. DIM_SIZE(J)) THEN
            WRITE(INFOBUF,102) 
     $        'Error in COORDS for rank:  = ', I,  
     $        '   Dimension  = ', J  ,  
     $        '   Value = ',  COORDS(J),
     $        '   Bound = ',  DIM_SIZE(J)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
          END IF
 460    CONTINUE
C#############################################################################
      
C
C     Check coords for uniqueness, and put in list
C
        DO 680 K = 1, INLIST
          DO 640 M = 1,NDIMS
            IF(COORDS(M) .NE. ALL_COORDS(I,K) ) GO TO 650
 640      CONTINUE
C
C     Error two sets of coordinates match
C
          WRITE(INFOBUF,102) 
     $       'Error  Non-Unique coordinates:  Coords for Node = ', J,
     $       '  Matches List item #', K  
          CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
 650      CONTINUE
C
C     No match, add this node to the list
C
          INLIST = INLIST + 1
          DO 660 L = 1, NDIMS
            ALL_COORDS(INLIST,L) = COORDS(L)
 660      CONTINUE
 680    CONTINUE
 700  CONTINUE



C#############################################################################

 2000 CONTINUE
      CALL MPI_COMM_FREE(COMM_CART, IERR)
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
C                Node defined for this communicator
      CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
        WRITE(INFOBUF,102) 'MPI_BARRIER Error (', IERR, 
     $    ')    ERRORCLASS = ', ERRORCLASS
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ERRORSTRING = ' '
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
      CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM, IERR)
 4000 CONTINUE
C             End communicator loop
C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)
      CALL MPI_FINALIZE(IERR)
      END
