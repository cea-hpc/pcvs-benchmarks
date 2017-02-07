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
C                        Test for MPI_Barrier()
C 
C Thie code tests the functionality of the MPI_Barrier() function.  This
C MPI function is supposed to cause all processes in a given
C communicator to stop at a barrier until all others arrive at the same
C barrier. 
C 
C The idea of this test is for all nodes but one (which we will call the
C root) to immediately enter the barrier, and when they exit the
C barrier, they all send  a message to the root.  Before it enters the
C barrier, the root probes for messages from the other nodes.  If
C MPI_Barrier() is working properly, these probes (using the MPI
C function MPI_Iprobe()) should not find any messages.  If a message is
C found, then one of the non-root nodes must have passed the barrier.
C This is an error since the root has not yet entered the barrier.
C 
C If the root process does not find any messages after a fixed number of
C probes (the number of probes is fixed by the compiler constant
C MAX_PROBE), then it enters the barrier.  After exiting the barrier,
C the root process receives all the messages.
C 
C This test may be run in any type of communicator with any number of
C processes participating.
C 
C The default behaviour is to loop over the communicator types and sizes
C specified in the files defs.h and mpitest_cfgf.h .  See the
C MPITEST README file for details.
C **********************************************************************

#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER    COMM_INDEX
C                               the array index of the current comm
      INTEGER    COMM_TYPE
C                               the index of the current communicator type
      INTEGER    COMM_COUNT
C                               loop counter for communicator loop
      INTEGER    COMM_SIZE
C                               number of ranks in the current communicator
      LOGICAL    INTER_FLAG
C                               used to test if intercommunicator
      INTEGER    FAIL
      INTEGER    LOOP_CNT
C                               counts total number of failures, loops
      INTEGER    IERR
      INTEGER    ERR
C                               return value from MPI calls
      LOGICAL    FLAG
C                               did the Iprobe find a message ?
      INTEGER    MAX_PROBE
      INTEGER    PROBE_COUNT
C                               the current number of completed probes
      INTEGER    ROOT
C                               the current checker for post-barrier messages
      INTEGER    COMM
C                               MPI communicator under test
      INTEGER    I, J

      INTEGER    BUFFER
      INTEGER    STATUS(MPI_STATUS_SIZE)
      INTEGER    SIZE
      INTEGER    POST_BARRIER_TAG

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 99   FORMAT(A,INT_FMT)
 179  FORMAT(A,I6,A,I6,A,I6,A,I6)

      MAX_PROBE=9000
      POST_BARRIER_TAG=0
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
      TESTNAME = 'MPI_Barrier()'
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF


C
C     Initialize the loop/error counters
C
      LOOP_CNT = 0
      FAIL = 0

C
C     Get number of communicators to test
C
      CALL MPITEST_NUM_COMM_SIZES(COMM_COUNT)
      IF (MPITEST_ME .EQ. 0) THEN
        WRITE (INFOBUF,99)'COMM_COUNT=',COMM_COUNT
        CALL MPITEST_MESSAGE (MPITEST_INFO2, INFOBUF)
      END IF

C
C     Loop through communicators
C
      DO 100 I=1,COMM_COUNT
        CALL MPITEST_GET_COMM_INDEX(I, COMM_INDEX)
        CALL MPITEST_GET_COMM_TYPE(I, COMM_TYPE)

        CALL MPITEST_GET_COMMUNICATOR(COMM_TYPE, COMM_INDEX, COMM,
     $        COMM_SIZE)

C       Skip if intercommunicator

        IF (COMM .NE. MPI_COMM_NULL) THEN
          CALL MPI_COMM_TEST_INTER(COMM, INTER_FLAG, IERR)
          IF (INTER_FLAG) GO TO 201
        END IF

C
C       Loop over different processes being the root
C
        DO 200 ROOT=0, COMM_SIZE-1
C         Print an informational message
          IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
            WRITE (INFOBUF,179) 'commindex', COMM_INDEX, ', commsize',
     $          COMM_SIZE, ', commtype', COMM_TYPE, ', root', ROOT
            CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
          END IF

C
C         Only execute the test if in the current communicator
          IF (COMM .NE. MPI_COMM_NULL) THEN
            LOOP_CNT = LOOP_CNT + 1
C
C           The root node probes, then enters the barrier, then 
C           receives messages
C
            IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
              FLAG = .FALSE.
              PROBE_COUNT = 0
              DO 600 J=0, MAX_PROBE
C
C               Look for post-barrier messages from any other process
C
                CALL MPI_IPROBE(MPI_ANY_SOURCE, POST_BARRIER_TAG, 
     $                            COMM, FLAG, STATUS, IERR)
                IF (IERR .NE. MPI_SUCCESS) THEN
                  WRITE(INFOBUF,99) 'MPI_IPROBE() returned', IERR
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                  INFOBUF=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
                  CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
                END IF
                IF (FLAG) THEN
                  PROBE_COUNT = MAX_PROBE
                END IF
 600          CONTINUE
C
C             If flag is true, then a post-barrier message
C             was received pre-barrier, so the test fails 
C
              IF (FLAG) THEN
                FAIL = FAIL + 1
                INFOBUF = 'Received a post-barrier message '//
     $                    'before entering barrier'
                CALL MPITEST_MESSAGE (MPITEST_NONFATAL, INFOBUF)
              END IF
                  
              CALL MPI_BARRIER(COMM, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                WRITE(INFOBUF,99) 'MPI_BARRIER() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
              END IF
                  
C             Root process receives post-barrier messages
              IF (COMM_SIZE .GT. 1) THEN
                DO 400 J=0, COMM_SIZE-2
                  CALL MPI_RECV(BUFFER, 0, MPI_INTEGER, 
     $                   MPI_ANY_SOURCE, POST_BARRIER_TAG, COMM,
     $                   STATUS, IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                    WRITE(INFOBUF,99) 'MPI_RECV() returned', IERR
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
                    CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
                  END IF
 400            CONTINUE
              END IF
C
C             Non-root processes enter barrier then send message to root
C
            ELSE
              CALL MPI_BARRIER(COMM, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                WRITE(INFOBUF,99) 'MPI_BARRIER() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
              END IF
              CALL MPI_SEND( BUFFER, 0, MPI_INTEGER, ROOT,
     $              POST_BARRIER_TAG, COMM, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                WRITE(INFOBUF,99) 'MPI_SEND() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
              END IF
            END IF
          END IF
      
 200    CONTINUE
 201    CONTINUE
      
      
        CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM, IERR)
 100  CONTINUE

C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)
      
      CALL MPI_FINALIZE(IERR)

      END  
  
  
