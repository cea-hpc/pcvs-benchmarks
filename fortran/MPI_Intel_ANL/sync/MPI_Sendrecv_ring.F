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
C                 MPI_Sendrecv funtional Ring test
C  
C  
C  Reference:  MPI Standard, Section 3.10
C  
C  In this test each node sends to its next higher numbered logical neighbor,
C  and receives from its  lower logical neighbor; within each 
C  communicator/subcommunicator  for Intra-communicators.  Thus, logical 
C  node k sends to node k+1, and receives from logical node k-1.  The nodes
C  numbers wrap around, so for n nodes, numbered 0 - n-1, node n-1 sends to
C  node 0, and node 0 receives from node n-1.
C  
C  A communicator cannot at the same time be an Intra-communicator, and an
C  Inter-communicator.  Inter-communicator node references, automatically
C  refer to nodes in the opposite subcommunicator.  For Inter-communicators
C  logical node k sends and receives from logical node k in the opposite
C  subcommunicator.  Thus, the logical nodes in each subcommunicator trade
C  messages. The test traces messages in an Inter-communicator for the number
C  of nodes in the smaller subcommunicator. 
C  
C  This test runs in any communicator/subcommunicator of two or more nodes, 
C  with any data type, and with any non-negative message length.
C  
C  The MPITEST environment provides looping over communicator size and
C  type, message length, and data type.  The properties of the loops are
C  encoded in configuration arrays in the file config.h .  See the
C  MPITEST README for further details.
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
      INTEGER    REMOTE_SIZE
C                               number of ranks in remote sub-communicator
      INTEGER    MY_COMM_SIZE
C                               number of ranks in send/recv group
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
C                               current message length to test 
      LOGICAL    ISOK
C                               flag for testing inter-communicator nodes
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

      INTEGER    SEND_CNT_LEN
      INTEGER    RECV_CNT_LEN
C                               Length of message from MPI_GET_COUNT
      INTEGER    GRP_LUP
C                               For INTER, index of comm group
      LOGICAL    INTER_FLAG
C                               Is comm under test an INTER communicator
      INTEGER    RECV_SIZE
      INTEGER    SEND_SIZE
C
      INTEGER    SEND_LENGTH  
C                               Message buffer length for send 
      INTEGER    RECV_LENGTH
C                               Message buffer length receive
      INTEGER    SEND_TO
      INTEGER    RECV_FROM
C                              Number of senders/receivers
      LOGICAL    PRINT_NODE
C                              Is node to print informational messages

      INTEGER    I, J, K, L, M

      MPITEST_BUF_TYPE  SEND_BUFFER(MAX_BUFF_SIZE+1)
      MPITEST_BUF_TYPE  RECV_BUFFER(MAX_BUFF_SIZE+1)

      INTEGER    ERRSIZE

      INTEGER    STAT(MPI_STATUS_SIZE)


      MPITEST_BUF_TYPE VALUE


      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING
C===========================================================================

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
      TESTNAME = 'MPI_SENDRECV_rtoa'
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
C---------------------  Loop over Communicators  ------------------------
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

          ISOK = .TRUE.
          MY_COMM_SIZE = COMM_SIZE


        IF (INTER_FLAG) THEN

            CALL MPI_COMM_SIZE(COMM, MY_COMM_SIZE, IERR)
            CALL MPI_COMM_REMOTE_SIZE(COMM, REMOTE_SIZE, IERR)
            IF(REMOTE_SIZE .LT. MY_COMM_SIZE)
     &                                   MY_COMM_SIZE = REMOTE_SIZE
            IF (MPITEST_CURRENT_RANK .GE. MY_COMM_SIZE) ISOK = .FALSE.
        END IF

          IF((MY_COMM_SIZE .LT. 2) .OR. (ISOK .EQV. .FALSE.)) GO TO 201

          IF (MPITEST_CURRENT_RANK .EQ. 0) PRINT_NODE = .TRUE.


C
C-----------------------  Loop over Data Types  -------------------------
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
C-----------------------  Loop over Message Lengths  --------------------
C
          DO 300 K=1,LENGTH_COUNT
            CALL MPITEST_GET_MESSAGE_LENGTH(K, BYTE_LENGTH)
            CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, BYTE_LENGTH,
     $          LENGTH)
            IF (LENGTH .GT. MAX_LENGTH) LENGTH = MAX_LENGTH

            SEND_LENGTH = LENGTH
            RECV_LENGTH = LENGTH

C
C           Loop over different processes being the root
C

                IF (PRINT_NODE) THEN
                  INFOBUF=' '
                  WRITE (INFOBUF,179) 
     $              'commindex', COMM_INDEX, ', commsize',
     $              COMM_SIZE, ', commtype', COMM_TYPE, ', root', ROOT,
     $              ', datatype', TEST_TYPE, ', length',  LENGTH
                  CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
                END IF

C
C                 Initialize expected receive data
C
                  CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1,
     $              TEST_TYPE)
                  CALL MPITEST_INIT_BUFFER(TEST_TYPE, 
     $              RECV_LENGTH+1, 
     $              VALUE, RECV_BUFFER)


                  CALL MPITEST_DATATEMPLATE_INIT(VALUE, 
     $              MPITEST_CURRENT_RANK, TEST_TYPE)
                  CALL MPITEST_INIT_BUFFER(TEST_TYPE, 
     $              SEND_LENGTH+1,
     $              VALUE, SEND_BUFFER)


                  LOOP_CNT = LOOP_CNT + 1
                  LOOP_FAIL = 0


                  RECV_FROM = 
     $              MOD((MY_COMM_SIZE + MPITEST_CURRENT_RANK - 1),
     $                                                  MY_COMM_SIZE)
                  SEND_TO   = MOD((MPITEST_CURRENT_RANK + 1),
     $                                                  MY_COMM_SIZE)

                  IF(INTER_FLAG) THEN
                    SEND_TO   = MPITEST_CURRENT_RANK
                    RECV_FROM = MPITEST_CURRENT_RANK
                  END IF


                  CALL MPI_SENDRECV(SEND_BUFFER,
     $                     SEND_LENGTH,
     $                     MPITEST_MPI_DATATYPES(TEST_TYPE), 
     $                     SEND_TO, 
     $                     MPITEST_CURRENT_RANK,
     $                     RECV_BUFFER,
     $                     RECV_LENGTH+1,
     $                     MPITEST_MPI_DATATYPES(TEST_TYPE), 
     $                     RECV_FROM,
     $                     RECV_FROM,                         
     $                     COMM, 
     $                     STAT,
     $                     IERR)

                  IF (IERR .NE. MPI_SUCCESS) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,99) 'MPI_SENDRECV() returned', 
     $                IERR
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE,
     $                   ERR)
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF
C
C                 Check data for errors and overflow
C
                  CALL MPITEST_DATATEMPLATE_INIT(VALUE, RECV_FROM,
     $              TEST_TYPE)
                  CALL MPITEST_BUFFER_ERRORS(TEST_TYPE, RECV_LENGTH, 
     $              VALUE,  RECV_BUFFER, ERROR)

                  CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1,
     $              TEST_TYPE)
                  CALL MPITEST_BUFFER_ERRORS_OV(TEST_TYPE, RECV_LENGTH,
     $              VALUE, RECV_BUFFER, ERROR2)
                  IF ((ERROR + ERROR2) .NE. 0) THEN
                    INFOBUF=' '
                    WRITE (INFOBUF,189) ERROR+ERROR2,
     $                ' errors in buffer, ',
     $                'commindex',COMM_INDEX,', commsize', COMM_SIZE,
     $                ', commtype', COMM_TYPE, ', root', ROOT,
     $                ', datatype', TEST_TYPE, ', length', RECV_LENGTH
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF
                
C
C                 Call MPI_GET_COUNT and compare value with length received
C
                  CALL MPI_GET_COUNT(STAT, 
     $                               MPITEST_MPI_DATATYPES(TEST_TYPE), 
     $                               RECV_CNT_LEN,
     $                               IERR)
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

                  IF (RECV_LENGTH - RECV_CNT_LEN .NE. 0) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,98) 
     $                  'Unexpected receive length',
     $                  RECV_CNT_LEN, ', expected', RECV_LENGTH
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

C
C                 Check source and tag values.
C
                  IF (STAT(MPI_TAG) .NE. RECV_FROM) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,98) 'Unexpected tag value',
     $                STAT(MPI_TAG), ', expected', RECV_FROM
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

                  IF (STAT(MPI_SOURCE) .NE. RECV_FROM) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,98) 'Unexpected source value',
     $                STAT(MPI_SOURCE), ', expected', RECV_FROM
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    LOOP_FAIL = LOOP_FAIL + 1
                  END IF

                  IF (LOOP_FAIL .NE. 0) FAIL = FAIL + 1
      
 300      CONTINUE
C                 End data length loop
      
 200    CONTINUE
C               End data type loop


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
