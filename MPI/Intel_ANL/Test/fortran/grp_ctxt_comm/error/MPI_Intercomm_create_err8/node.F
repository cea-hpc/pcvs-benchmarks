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
C                          MPI_Intercomm_create_err8
C 
C This test verifies that the correct error is returned if 
C MPI_Intercomm_create() is called with invalid arguments.
C 
C MPI_Intercomm_create error tests
C -----------------------------------
C  1)  local_comm=MPI_COMM_NULL ............................ [COMM]
C  2)  peer_comm=MPI_COMM_NULL ............................. [COMM]
C  3)  removed (invalid test)
C  4)  removed (invalid test)
C  5)  peer_comm not containing local and remote leaders ... [ARG/COMM/OTHER]
C  6)  removed
C  7)  local_leader=MPI_ANY_SOURCE ......................... [ARG/OTHER/RANK]
C  8)  remote_leader=MPI_ANY_SOURCE ........................ [ARG/OTHER/RANK]
C  9)  local_leader not a valid rank in local_comm ......... [RANK]
C 10) remote_leader not a valid rank in peer_comm .........  [RANK]
C 
C  We only run on two nodes for this test because there is no requirement
C  that remote_leader be valid for nodes other then the leaders so the other
C  nodes down't need to check it.  As a result some implemenations won't
C  check for remote_leader=MPI_ANY_SOURCE in other nodes and they will hang
C  waiting for communication from the leaders (who have exited with
C  errors.)
C
C  In all cases, expect to receive appropriate error.
C 
C  MPI Calls dependencies for this test:
C    MPI_Intercomm_create(), MPI_Init(), MPI_Finalize()
C    MPI_Error_string(), MPI_Comm_split(), MPI_Comm_dup()
C***********************************************************************

#include "foptions.h"

      PROGRAM MAIN
C
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
C
      INTEGER    MIN_NODES
      PARAMETER (MIN_NODES = 2)
C                                  Minimum nodes needed to run job
      CHARACTER  TEST_DESC*(*)
      PARAMETER (TEST_DESC =
     $'MPI_Intercomm_create: call with REMOTE_LEADER = MPI_ANY_SOURCE')
C
      CHARACTER  ERR_DESC*(*)
      PARAMETER (ERR_DESC  = 'REMOTE_LEADER = MPI_ANY_SOURCE')
C
      INTEGER PASS
      INTEGER FAIL              
C                           counts total number of PASSes/FAILures
      INTEGER IERR
      INTEGER ERR
C                           return value from MPI calls
      INTEGER ERRORCLASS
      INTEGER SIZE 
C                           error class of IERR
      INTEGER RESULT 
C                           result from intercomm_create
      CHARACTER*(IOSIZE)  INFOBUF
C                           buffer for passing messages to MPITEST
      CHARACTER*(MPI_MAX_ERROR_STRING)  ERRORSTRING
C                           string describing error flag
      CHARACTER*32   TESTNAME
C                           name of this test
      INTEGER LOCAL_COMM
C                           local intra-communicator
      INTEGER PEER_COMM
C                           "peer" inter-communicator
      INTEGER NEWINTERCOMM
C                           the new inter-communicator
      INTEGER LOCAL_LEADER  
C                           rank of the local group leader in local_comm
      INTEGER REMOTE_LEADER
C                           rank of the remote leader in peer_comm
      INTEGER TAG
C                           "safe" tag
      INTEGER COLOR
C                           discriminator used by MPI_Comm_split 
      INTEGER KEY
C                           controls ordering in new LOCAL_COMMs
C-----------------------------------------------------------------------------
  98  FORMAT(A,A)
 100  FORMAT(A200)
 101  FORMAT(A, A, INT_FMT)
 102  FORMAT(A, INT_FMT, A, INT_FMT)
 103  FORMAT(A, INT_FMT)
 106  FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A)
C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,103) 'MPI_INIT() returned', IERR 
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
      TESTNAME='MPI_Intercomm_create_err8'
      CALL MPITEST_INIT(IERR)
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      PASS = 0
      FAIL = 0
C
C  Test if we have enough nodes to run test
C
      IF (MPITEST_NUMP .LT.  MIN_NODES)  THEN 
        FAIL = FAIL + 1
        WRITE(INFOBUF, 102) 
     $    'Not enought nodes to run test,  #nodes = ', MPITEST_NUMP,
     $    '  Need at least ', MIN_NODES
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C 
C  Set an errorhandler so we get control back.  
C 
      CALL MPI_ERRHANDLER_SET(MPI_COMM_WORLD, MPI_ERRORS_RETURN, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 103) 
     $    ' MPI_ERRORHANDLER_SET(MPI_COMM_WORLD)  returned ', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C
C     Initialise all ther values for the bad calls
C
      LOCAL_COMM = MPI_COMM_SELF
C
C     Set LOCAL_LEADER -- all are 0 due to the use of keys above
C
      LOCAL_LEADER = 0
C
C     Set PEER_COMM  
C
C
C-----------------------  MPI_Comm_dup  -----------------------------
C
      CALL MPI_COMM_DUP(MPI_COMM_WORLD, PEER_COMM, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 103) ' MPI_COMM_DUP returned ', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C
C     Set remote_leader
C
      REMOTE_LEADER = MPI_ANY_SOURCE
C
C     choose a tag -- anything will do since we only need one
C
      TAG = 42
C
C     Setup errorhandlers on local_comm so the tests work 
C

      CALL MPI_ERRHANDLER_SET(LOCAL_COMM, MPI_ERRORS_RETURN, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        FAIL = FAIL + 1
        WRITE(INFOBUF, 103) 
     $    ' MPI_ERRORHANDLER_SET(LOCAL_COMM) returned ', IERR
        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
        WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF
C
      IF(MPITEST_ME .LT. 2) THEN
        WRITE(INFOBUF, 98) 
     $  'Calling MPI_Intercomm_create with ', ERR_DESC
        CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
C
C-----------------------  MPI_Intercomm_create  -----------------------------
C
        CALL MPI_INTERCOMM_CREATE(LOCAL_COMM, LOCAL_LEADER, PEER_COMM, 
     $                          REMOTE_LEADER, TAG, NEWINTERCOMM, IERR)
        IF (IERR .EQ. MPI_SUCCESS) THEN
          FAIL = FAIL + 1
          WRITE(INFOBUF, 98) TEST_DESC, ' did not FAIL'
          CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
        ELSE
          CALL MPI_ERROR_CLASS(IERR, ERRORCLASS, ERR)
          IF (ERRORCLASS .NE. MPI_ERR_RANK  .AND.
     $        ERRORCLASS .NE. MPI_ERR_ARG   .AND.   
     $        ERRORCLASS .NE. MPI_ERR_OTHER) THEN 
            FAIL = FAIL + 1
            WRITE(INFOBUF,101) TEST_DESC, ' Returned',  ERRORCLASS 
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            WRITE(INFOBUF,106) 
     $        'Expected MPI_ERR_RANK(',   MPI_ERR_RANK,
     $        ')  OR  MPI_ERR_ARG(',      MPI_ERR_ARG,  
     $        ')  OR  MPI_ERR_OTHER(',    MPI_ERR_OTHER, ')'  
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            ERRORSTRING = ' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
            WRITE(INFOBUF,100) ERRORSTRING(1:SIZE)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
          ELSE
            PASS = PASS + 1
            WRITE(INFOBUF,102) 'IERR = ', IERR, '  ERRORCLASS = ',
     $        ERRORCLASS
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
