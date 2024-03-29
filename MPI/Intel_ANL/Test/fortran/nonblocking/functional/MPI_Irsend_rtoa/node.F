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
C                   Test for MPI_Irsend():  ROOT send to ALL version
C                       with MPI_Irecv():
C
C
C Tests the basic non-blocking MPI_Irsend operation, with both Intra/Inter
C communicators.
C 
C The Rsend or Ready mode Send may be started only if the matching receive
C has already been posted.  The MPI Standard only states that if the matching
C receive has not been posted, the outcome of the operation is undefined.  To
C avoid this, MPI_Irecv() is used and the ranks synchronized before the call
C to MPI_Irsend().
C 
C For an INTRA-communicator, the program selects each node, in turn,
C of the communicator to be the root.  the ROOT SENDS to all of the other
C nodes. Root does NOT send to itself.
C 
C For an INTER-communicator, there are two groups of nodes; each group being
C termed a sub-communicator.  The nodes in each sub-communicator are numbered
C 0 to (n-1) for an n-node sub-communicator.  So,the MPITEST_current_rank will
C return duplicate node numbers for nodes (0 to k-1) where k is the number of
C nodes in the smaller communicator.
C 
C The program cycles through the nodes in one sub-communicator, sending from
C each selected node to  all of the nodes in the other sub-communicator.  Then
C the program reverses the send and receive role of the two sub-communicators,
C and repeats the process.
C 
C This test initializes the send buffer with the root's rank in the
C communicator (or an appropriate value for the non-integer types.)
C Once the receiving nodes have completed their message receive, they check to
C make sure the current root's rank is in the received buffer.
C 
C This test may be run in any communicator, with any data type, and with
C any non-negative message length.
C 
C **********************************************************************
#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM TEST

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
      INTEGER    TYPE_COUNT
C                               number of data types to test
      INTEGER    TEST_TYPE
C                               index of current data type
      INTEGER    LENGTH_COUNT
C                               number of data lengths to test
      INTEGER    MAX_LENGTH
C                               max message length to test
      INTEGER    MAX_BYTE_LENGTH
C                               max byte length to test
      INTEGER    MAX_BUFF_LENGTH
C                               max length that will fit in buffers
      INTEGER    BYTE_LENGTH
C                               current byte length to test
      INTEGER    LENGTH
C                               current length to test 

      INTEGER    FAIL
      INTEGER    LOOP_CNT
C                               counts total number of failures, loops
      INTEGER    LOOP_FAIL
C                               counts number of failures in 1 loop
      INTEGER    ERROR, ERROR2
C                               number of data errors found in buffer
      INTEGER    IERR
      INTEGER    ERR
C                               return value from MPI calls
      INTEGER    ROOT
C                               the current root
      INTEGER    COMM
C                               MPI communicator under test
      INTEGER    BARRIER_COMM
C                               MPI communicator for BARRIER
      LOGICAL    FOUND
C                               Boolean for MPI_GET_ATTR
      INTEGER    COUNT_LEN
C                               Length of received message from MPI_GET_COUNT
      INTEGER    GRP_LUP
C                               For INTER, index of comm group
      LOGICAL    INTER_FLAG
C                               Is comm under test an INTER communicator
      INTEGER    RECV_SIZE
      INTEGER    LEFT_RECV_SIZE
      INTEGER    RIGHT_RECV_SIZE
C                              For INTER, number of receivers on each side
      INTEGER    SEND_SIZE
      INTEGER    LEFT_SEND_SIZE
      INTEGER    RIGHT_SEND_SIZE
C                              For INTER, number of senders on each side
      INTEGER    SEND_GROUP
      INTEGER    RECV_GROUP
C                              For INTER, current group under test
      INTEGER    NTIMES
C                              Number of times to go through loop
C                              INTER=2, INTRA=1
      INTEGER    SENDERS
      INTEGER    SEND_TO
      INTEGER    RECEIVERS
C                              Number of senders/receivers
      LOGICAL    PRINT_NODE
C                              Is node to print informational messages

      INTEGER    I, J, K, L, M

      MPITEST_BUF_TYPE SEND_BUFFER(MAX_BUFF_SIZE+1)
      MPITEST_BUF_TYPE RECV_BUFFER(MAX_BUFF_SIZE+1)
      INTEGER    ERRSIZE

      INTEGER    RECV_STAT(MPI_STATUS_SIZE)
      INTEGER    SEND_STAT(MPI_STATUS_SIZE)
      INTEGER    RECV_REQ
      INTEGER    SEND_REQ

      MPITEST_BUF_TYPE VALUE

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 98   FORMAT(A,INT_FMT,A,INT_FMT)
 99   FORMAT(A,INT_FMT)
 179  FORMAT(A,I6,A,I6,A,I6,A,I6,A,I3,A,INT_FMT)
 189  FORMAT(INT_FMT,A,A,I6,A,I6,A,I6,A,I6,A,I3,A,INT_FMT)

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
      TESTNAME = 'MPI_Irsend_rtoa'
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
C     Get number of communicators, data types and message lengths to test
C
      CALL MPITEST_NUM_COMM_SIZES(COMM_COUNT)
      CALL MPITEST_NUM_DATATYPES(TYPE_COUNT)
      CALL MPITEST_NUM_MESSAGE_LENGTHS(LENGTH_COUNT)

C
C     Get max message length for test
C
      CALL MPITEST_GET_MAX_MESSAGE_LENGTH(MAX_BYTE_LENGTH)

C
C     Loop through communicators
C
      DO 100 I=1,COMM_COUNT
        CALL MPITEST_GET_COMM_INDEX(I, COMM_INDEX)
        CALL MPITEST_GET_COMM_TYPE(I, COMM_TYPE)

        CALL MPITEST_GET_COMMUNICATOR(COMM_TYPE, COMM_INDEX, COMM,
     $        COMM_SIZE)

C       Skip to end of loop if this node is not part of current
C       communicator

        IF (COMM .EQ. MPI_COMM_NULL) GO TO 201

C       Test for INTER-communicator and initialize appropriately if so

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

        PRINT_NODE = .FALSE.

        IF (INTER_FLAG) THEN
          IF (MPITEST_INTER .EQ. 0) THEN
C           Left side of intercommunicator
            CALL MPI_COMM_SIZE(COMM, SEND_SIZE, IERR)
            CALL MPI_COMM_REMOTE_SIZE(COMM, RECV_SIZE, IERR)

            LEFT_SEND_SIZE = SEND_SIZE
            LEFT_RECV_SIZE = RECV_SIZE
            RIGHT_SEND_SIZE = RECV_SIZE
            RIGHT_RECV_SIZE = SEND_SIZE

            IF (MPITEST_CURRENT_RANK .EQ. 0) PRINT_NODE = .TRUE.

            CALL MPI_INTERCOMM_MERGE(COMM, .FALSE., BARRIER_COMM, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_INTERCOMM_MERGE() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF
          END IF

          IF (MPITEST_INTER .EQ. 1) THEN
C           Right side of intercommunicator
            CALL MPI_COMM_SIZE(COMM, SEND_SIZE, IERR)
            CALL MPI_COMM_REMOTE_SIZE(COMM, RECV_SIZE, IERR)

            LEFT_SEND_SIZE = RECV_SIZE
            LEFT_RECV_SIZE = SEND_SIZE
            RIGHT_SEND_SIZE = SEND_SIZE
            RIGHT_RECV_SIZE = RECV_SIZE

            CALL MPI_INTERCOMM_MERGE(COMM, .FALSE., BARRIER_COMM, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_INTERCOMM_MERGE() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF
          END IF

        ELSE
C       Intracommunicator
          IF (MPITEST_CURRENT_RANK .EQ. 0) PRINT_NODE = .TRUE.
          BARRIER_COMM = COMM
        END IF

C
C       Loop through data types.
C
        DO 200 J=1,TYPE_COUNT

          CALL MPITEST_GET_DATATYPE(J, TEST_TYPE)
C
C         Convert the number of bytes in the maximum length message
C         into the number of elements of the current type 
          CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, MAX_BYTE_LENGTH,
     $          MAX_LENGTH)
          CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE,
     $           MAX_BUFF_SIZE*MPITEST_BUF_EXTENT, MAX_BUFF_LENGTH)

          IF (MAX_LENGTH .GE. (MAX_BUFF_LENGTH - 1))
     $         MAX_LENGTH = MAX_BUFF_LENGTH - 1


C
C         Loop through data lengths
C
          DO 300 K=1,LENGTH_COUNT
            CALL MPITEST_GET_MESSAGE_LENGTH(K, BYTE_LENGTH)
            CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, BYTE_LENGTH,
     $          LENGTH)
            IF (LENGTH .GT. MAX_LENGTH) LENGTH = MAX_LENGTH

C
C           Loop over different processes being the root
C
            IF (INTER_FLAG) THEN
              NTIMES = 2
            ELSE
              NTIMES = 1
            END IF

            DO 350 GRP_LUP=1, NTIMES
C             If intercommunicators, test each side
              SEND_GROUP = GRP_LUP - 1
              RECV_GROUP = 2 - GRP_LUP

              SENDERS = COMM_SIZE
              RECEIVERS = COMM_SIZE - 1
              IF (INTER_FLAG .AND. (GRP_LUP .EQ. 1)) THEN
                RECEIVERS = LEFT_RECV_SIZE
                SENDERS = LEFT_SEND_SIZE
              ELSE IF (INTER_FLAG .AND. (GRP_LUP .EQ. 2)) THEN
                RECEIVERS = RIGHT_RECV_SIZE
                SENDERS = RIGHT_SEND_SIZE
              END IF

              DO 400 ROOT=0,SENDERS-1
C               Print an informational message
                IF (PRINT_NODE) THEN
                  INFOBUF=' '
                  WRITE (INFOBUF,179) 
     $              'commindex', COMM_INDEX, ', commsize',
     $              COMM_SIZE, ', commtype', COMM_TYPE, ', root', ROOT,
     $              ', datatype', TEST_TYPE, ', length',  LENGTH
                  CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
                END IF

C
C               All but root on receive side post receives
C
                IF (((INTER_FLAG .EQV. .FALSE.) .AND. 
     $               (MPITEST_CURRENT_RANK .NE. ROOT)) .OR.
     $              ((INTER_FLAG .EQV. .TRUE.) .AND.
     $               (MPITEST_INTER .EQ. RECV_GROUP))) THEN
C
C                 Initialize expected receive data
C
                  CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1,
     $              TEST_TYPE)
                  CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH+1, VALUE,
     $              RECV_BUFFER)
                  LOOP_CNT = LOOP_CNT + 1
                  LOOP_FAIL = 0


                  CALL MPI_IRECV(RECV_BUFFER, LENGTH+1,
     $              MPITEST_MPI_DATATYPES(TEST_TYPE), ROOT, 32767,
     $              COMM, RECV_REQ, IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,99) 'MPI_IRECV() returned', IERR
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE,
     $                   ERR)
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

C
C                 Signal ready for RSEND()
C
                  CALL MPI_BARRIER(BARRIER_COMM, IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,99) 'MPI_BARRIER() returned', IERR
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE,
     $                   ERR)
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

C
C                 Wait for the message
C
                  CALL MPI_WAIT(RECV_REQ, RECV_STAT, IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,99) 'MPI_WAIT(IRECV) returned', IERR
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE,
     $                   ERR)
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                    IF (RECV_STAT(MPI_ERROR) .NE. MPI_SUCCESS) THEN
                      INFOBUF=' '
                      CALL MPI_ERROR_STRING(RECV_STAT(MPI_ERROR),
     $                      INFOBUF, ERRSIZE, ERR)
                      CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    END IF
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

C
C                 Check data for errors and overflow
C
                  CALL MPITEST_DATATEMPLATE_INIT(VALUE, ROOT,
     $              TEST_TYPE)
                  CALL MPITEST_BUFFER_ERRORS(TEST_TYPE, LENGTH, VALUE,
     $              RECV_BUFFER, ERROR)
                  CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1,
     $              TEST_TYPE)
                  CALL MPITEST_BUFFER_ERRORS_OV(TEST_TYPE, LENGTH,
     $              VALUE, RECV_BUFFER, ERROR2)
                  IF ((ERROR + ERROR2) .NE. 0) THEN
                    INFOBUF=' '
                    WRITE (INFOBUF,189) ERROR+ERROR2,
     $                ' errors in buffer, ',
     $                'commindex',COMM_INDEX,', commsize', COMM_SIZE,
     $                ', commtype', COMM_TYPE, ', root', ROOT,
     $                ', datatype', TEST_TYPE, ', length',  LENGTH
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF
                
C
C                 Call MPI_GET_COUNT and compare value with length received
C
                  CALL MPI_GET_COUNT(RECV_STAT, 
     $                MPITEST_MPI_DATATYPES(TEST_TYPE), COUNT_LEN, IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,99) 'MPI_GET_COUNT() returned', IERR
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE,
     $                   ERR)
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

                  IF (LENGTH - COUNT_LEN .NE. 0) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,98) 'Unexpected receive length',
     $                  COUNT_LEN, ', expected', LENGTH
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

C
C                 Check source and tag values.
C
                  IF (RECV_STAT(MPI_TAG) .NE. 32767) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,98) 'Unexpected tag value',
     $                RECV_STAT(MPI_TAG), ', expected', 32767
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

                  IF (RECV_STAT(MPI_SOURCE) .NE. ROOT) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,98) 'Unexpected source value',
     $                RECV_STAT(MPI_SOURCE), ', expected', ROOT
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

                  IF (LOOP_FAIL .NE. 0) FAIL = FAIL + 1
                END IF
C                    Receiver

C
C               Root sends the message
C
                IF (((INTER_FLAG .EQV. .FALSE.) .AND. 
     $               (MPITEST_CURRENT_RANK .EQ. ROOT)) .OR.
     $              ((INTER_FLAG .EQV. .TRUE.) .AND.
     $               (MPITEST_INTER .EQ. SEND_GROUP) .AND.
     $               (MPITEST_CURRENT_RANK .EQ. ROOT))) THEN
                  CALL MPITEST_DATATEMPLATE_INIT(VALUE, 
     $              MPITEST_CURRENT_RANK, TEST_TYPE)
                  CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH+1,
     $              VALUE, SEND_BUFFER)

                  SEND_TO = COMM_SIZE
                  IF (INTER_FLAG) SEND_TO = RECEIVERS

C
C                 Wait for signal receivers are ready for RSEND()
C
                  CALL MPI_BARRIER(BARRIER_COMM, IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,99) 'MPI_BARRIER() returned', IERR
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE,
     $                   ERR)
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                    FAIL = FAIL + 1
                  END IF

                  DO 500 M=0, SEND_TO-1
                    IF ((INTER_FLAG .EQV. .FALSE.) .AND.
     $                  (M .EQ. ROOT)) GO TO 500
C                       Don't send to self
                    LOOP_CNT = LOOP_CNT + 1

                    CALL MPI_IRSEND(SEND_BUFFER, LENGTH,
     $                MPITEST_MPI_DATATYPES(TEST_TYPE), M, 32767,
     $                COMM, SEND_REQ, IERR)
                    IF (IERR .NE. MPI_SUCCESS) THEN
                      INFOBUF=' '
                      WRITE(INFOBUF,99) 'MPI_IRSEND() returned', IERR
                      CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                      INFOBUF=' '
                      CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE,
     $                     ERR)
                      CALL MPITEST_MESSAGE(MPITEST_NONFATAL,ERRORSTRING)
                      FAIL = FAIL + 1
                    END IF

                    CALL MPI_WAIT(SEND_REQ, SEND_STAT, IERR)
                    IF (IERR .NE. MPI_SUCCESS) THEN
                      INFOBUF=' '
                      WRITE(INFOBUF,99)'MPI_WAIT(IRSEND) returned',IERR
                      CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                      INFOBUF=' '
                      CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE,
     $                     ERR)
                      CALL MPITEST_MESSAGE(MPITEST_NONFATAL,ERRORSTRING)
                      IF (SEND_STAT(MPI_ERROR) .NE. MPI_SUCCESS) THEN
                        INFOBUF=' '
                        CALL MPI_ERROR_STRING(SEND_STAT(MPI_ERROR),
     $                        INFOBUF, ERRSIZE, ERR)
                        CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                      END IF
                      FAIL = FAIL + 1
                    END IF
 500              CONTINUE
C                         Send to all receivers
                END IF
C                    Sender
              IF (INTER_FLAG .AND. (MPITEST_INTER .EQ. SEND_GROUP)
     $            .AND. (MPITEST_CURRENT_RANK .NE. ROOT)) THEN
C
C               Need to match barrier above if in the Send group but
C               not the sender (for intercommunicators)
C
                CALL MPI_BARRIER(BARRIER_COMM, IERR)
                IF (IERR .NE. MPI_SUCCESS) THEN
                  INFOBUF=' '
                  WRITE(INFOBUF,99) 'MPI_BARRIER() returned', IERR
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                  INFOBUF=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                  FAIL = FAIL + 1
                END IF
              END IF

 400          CONTINUE
C                     End root loop

 350        CONTINUE
C                   End INTER send group loop
      
 300      CONTINUE
C                 End data length loop
      
 200    CONTINUE
C               End data type loop

        IF (INTER_FLAG) THEN
          CALL MPI_COMM_FREE(BARRIER_COMM, IERR)
          IF (IERR .NE. MPI_SUCCESS) THEN
            INFOBUF=' '
            WRITE(INFOBUF,99) 'MPI_COMM_FREE() returned', IERR
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            INFOBUF=' '
            CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
            CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
            FAIL = FAIL + 1
          END IF
        END IF

 201    CONTINUE

        CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM, IERR)
        CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

 100  CONTINUE
C             End communicator loop

C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)
      
      CALL MPI_FINALIZE(IERR)

      END
