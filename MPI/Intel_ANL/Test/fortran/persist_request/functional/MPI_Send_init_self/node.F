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
C                   Test for MPI_Send_init() to self
C 
C This tests the persistent MPI_Send_init operation from a node to itself.
C 
C To avoid hangs, rank 0 waits for a message from rank 1, and aborts the test
C if it has not been notified of success.
C 
C This test may be run in any communicator, with any data type, and with
C any non-negative message length.
C 
C The MPITEST environment provides looping over communicator size and
C type, message length, and data type.  The properties of the loops are
C encoded in configuration arrays in the file config.h .  See the
C MPITEST README for further details.
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
      INTEGER    LOOP_CNT2
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
      INTEGER    COUNT_LEN
C                               Length of received message from MPI_GET_COUNT
      LOGICAL    INTER_FLAG
C                               Is comm under test an INTER communicator
      LOGICAL    FLAG

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
      TESTNAME = 'MPI_Send_init_self'
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

C       Test for INTER-communicator

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

        IF (INTER_FLAG) GO TO 201

        IF (COMM_SIZE .EQ. 1) GO TO 201
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

            IF (MPITEST_CURRENT_RANK .NE. 0) THEN
              CALL MPITEST_DATATEMPLATE_INIT(VALUE, 
     $          MPITEST_CURRENT_RANK, TEST_TYPE)
              CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH+1,
     $          VALUE, SEND_BUFFER)

                CALL MPI_SEND_INIT(SEND_BUFFER, LENGTH,
     $            MPITEST_MPI_DATATYPES(TEST_TYPE), 
     $            MPITEST_CURRENT_RANK, 1, COMM, SEND_REQ, IERR)
                IF (IERR .NE. MPI_SUCCESS) THEN
                  INFOBUF=' '
                  WRITE(INFOBUF,99) 'MPI_SEND_INIT() returned', IERR
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                  INFOBUF=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                  FAIL = FAIL + 1
                END IF

                CALL MPI_START(SEND_REQ, IERR)
                IF (IERR .NE. MPI_SUCCESS) THEN
                  INFOBUF=' '
                  WRITE(INFOBUF,99) 'MPI_START() returned', IERR
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                  INFOBUF=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                  FAIL = FAIL + 1
                END IF


C
C             Initialize expected receive data
C
              CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1,
     $          TEST_TYPE)
              CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH+1, VALUE,
     $          RECV_BUFFER)
              LOOP_CNT = LOOP_CNT + 1
              LOOP_FAIL = 0


              CALL MPI_RECV(RECV_BUFFER, LENGTH+1,
     $          MPITEST_MPI_DATATYPES(TEST_TYPE), 
     $          MPITEST_CURRENT_RANK, 1, COMM, RECV_STAT, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                INFOBUF=' '
                WRITE(INFOBUF,99) 'MPI_RECV() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                LOOP_FAIL = LOOP_FAIL + 1
              END IF

              CALL MPI_WAIT(SEND_REQ, SEND_STAT, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                INFOBUF=' '
                WRITE(INFOBUF,99) 'MPI_WAIT() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                LOOP_FAIL = LOOP_FAIL + 1
              END IF

              CALL MPI_REQUEST_FREE(SEND_REQ, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                INFOBUF=' '
                WRITE(INFOBUF,99)
     $             'MPI_REQUEST_FREE() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                LOOP_FAIL = LOOP_FAIL + 1
              END IF
C
C             Check data for errors and overflow
C
              CALL MPITEST_DATATEMPLATE_INIT(VALUE, 
     $          MPITEST_CURRENT_RANK, TEST_TYPE)
              CALL MPITEST_BUFFER_ERRORS(TEST_TYPE, LENGTH, VALUE,
     $          RECV_BUFFER, ERROR)
              CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1,
     $          TEST_TYPE)
              CALL MPITEST_BUFFER_ERRORS_OV(TEST_TYPE, LENGTH,
     $          VALUE, RECV_BUFFER, ERROR2)
              IF ((ERROR + ERROR2) .NE. 0) THEN
                INFOBUF=' '
                WRITE (INFOBUF,189) ERROR+ERROR2,
     $            ' errors in buffer, ',
     $            'commindex',COMM_INDEX,', commsize', COMM_SIZE,
     $            ', commtype', COMM_TYPE, ', root', ROOT,
     $            ', datatype', TEST_TYPE, ', length',  LENGTH
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                LOOP_FAIL = LOOP_FAIL + 1
              END IF
                
C
C             Call MPI_GET_COUNT and compare value with length received
C
              CALL MPI_GET_COUNT(RECV_STAT, 
     $            MPITEST_MPI_DATATYPES(TEST_TYPE), COUNT_LEN, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                INFOBUF=' '
                WRITE(INFOBUF,99) 'MPI_GET_COUNT() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                LOOP_FAIL = LOOP_FAIL + 1
              END IF

              IF (LENGTH - COUNT_LEN .NE. 0) THEN
                INFOBUF=' '
                WRITE(INFOBUF,98) 'Unexpected receive length',
     $              COUNT_LEN, ', expected', LENGTH
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                LOOP_FAIL = LOOP_FAIL + 1
              END IF

C
C             Check source and tag values.
C
              IF (RECV_STAT(MPI_TAG) .NE. 1) THEN
                INFOBUF=' '
                WRITE(INFOBUF,98) 'Unexpected tag value',
     $            RECV_STAT(MPI_TAG), ', expected', 1
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                LOOP_FAIL = LOOP_FAIL + 1
              END IF

              IF (RECV_STAT(MPI_SOURCE) .NE. MPITEST_CURRENT_RANK)
     $         THEN
                INFOBUF=' '
                WRITE(INFOBUF,98) 'Unexpected source value',
     $            RECV_STAT(MPI_SOURCE), ', expected', 
     $            MPITEST_CURRENT_RANK
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                LOOP_FAIL = LOOP_FAIL + 1
              END IF

              IF (LOOP_FAIL .NE. 0) FAIL = FAIL + 1
            END IF
C                Send_init to self

C
C           Rank 1 signals rank 0 of success
C

            IF (MPITEST_CURRENT_RANK .EQ. 1) THEN
              CALL MPI_SEND(SEND_BUFFER, 0, MPI_INTEGER,
     $          0, 0, COMM, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                INFOBUF=' '
                WRITE(INFOBUF,99) 'MPI_SEND() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                FAIL = FAIL + 1
              END IF
            END IF

C
C           Rank 0 waits for signal from rank 1
C
            IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
              CALL MPI_IRECV(RECV_BUFFER, 0, MPI_INTEGER, 1, 0,
     $           COMM, RECV_REQ, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                INFOBUF=' '
                WRITE(INFOBUF,99) 'MPI_IRECV() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
                FAIL = FAIL + 1
              END IF

              DO 400 LOOP_CNT = 0, 300
                DO 500 LOOP_CNT2 = 0, 300000
                  CALL MPI_TEST(RECV_REQ, FLAG, RECV_STAT, IERR)
                  IF (IERR .NE. MPI_SUCCESS) THEN
                    INFOBUF=' '
                    WRITE(INFOBUF,99) 'MPI_TEST() returned', IERR
                    CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                    INFOBUF=' '
                    CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, 
     $                   ERR)
                    CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
                    FAIL = FAIL + 1
                  END IF
                  IF (FLAG .EQV. .TRUE.) GO TO 401
 500            CONTINUE
 400          CONTINUE

              INFOBUF='Nodes sending to self did not respond - FAILED'
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)

 401          CONTINUE 
            END IF



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
