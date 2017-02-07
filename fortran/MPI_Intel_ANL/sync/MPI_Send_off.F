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
C      Test for MPI_Send():  ROOT send to ALL version with BYTE offsets
C           and MPI_Recv()
C
C This is a modified version of the  MPI_Send  INTER-COMMUNICATOR version
C of the SEND FROM ROOT TO ALL. The program uses only the CHARACTER data type.
C The charater records are then offset by one to three bytes for both send and
C receive as a test for non-word address boundary conditions.
C 
C The program selects each node, in turn, to be the root.  The ROOT SENDS to
C each of the other nodes.  In this test ROOT does NOT send to itself.
C 
C This test initializes the send buffer with the root's rank in the
C communicator (or an appropriate value for the non-integer types.)
C Once the receiving nodes have completed their message receive, they check to
C make sure the current root's rank is in the received buffer.
C 
C This test may be run in any communicator, with any data type, and with
C any non-negative message length.
C 
C The MPITEST environment provides looping over communicator size and
C type, message length, and data type.  The properties of the loops are
C encoded in configuration arrays in the file config.h .  See the
C MPITEST README for further details.
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
      INTEGER    COUNT_LEN
C                               Length of received message from MPI_GET_COUNT
      LOGICAL    PRINT_NODE
C                              Is node to print informational messages

      INTEGER    I, J, K, L, M

      MPITEST_BUF_TYPE SEND_BUFFER(MAX_BUFF_SIZE+5)
      MPITEST_BUF_TYPE RECV_BUFFER(MAX_BUFF_SIZE+5)
      INTEGER    ERRSIZE

      INTEGER    RECV_STAT(MPI_STATUS_SIZE)

      INTEGER    OFFSET
      INTEGER    SEND_OFFSET
      INTEGER    RECV_OFFSET

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
      TESTNAME = 'MPI_Send_off'
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
      CALL MPITEST_NUM_MESSAGE_LENGTHS(LENGTH_COUNT)

C
C     Get max message length for test
C
      CALL MPITEST_GET_MAX_MESSAGE_LENGTH(MAX_BYTE_LENGTH)

      COMM = MPI_COMM_WORLD
      IF (MPITEST_CURRENT_RANK .EQ. 0) PRINT_NODE = .TRUE.

C
C     We will just test characters in this test, so force it here
C
      MPITEST_TYPES(1) = MPITEST_CHARACTER
      J = 1
      CALL MPITEST_GET_DATATYPE(J, TEST_TYPE)
C
C     Convert the number of bytes in the maximum length message
C     into the number of elements of the current type 
      CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, MAX_BYTE_LENGTH,
     $        MAX_LENGTH)
          CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE,
     $           MAX_BUFF_SIZE*MPITEST_BUF_EXTENT, MAX_BUFF_LENGTH)

      IF (MAX_LENGTH .GE. (MAX_BUFF_LENGTH - 5))
     $     MAX_LENGTH = MAX_BUFF_LENGTH - 5


C
C     Loop through data lengths
C
      DO 200 K=1,LENGTH_COUNT
        CALL MPITEST_GET_MESSAGE_LENGTH(K, BYTE_LENGTH)
        CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, BYTE_LENGTH,
     $      LENGTH)
        IF (LENGTH .GT. MAX_LENGTH) LENGTH = MAX_LENGTH

C
C       Loop through offsets
C
        DO 300 OFFSET=1,4
          SEND_OFFSET = OFFSET
          RECV_OFFSET = 5 - OFFSET

C
C       Loop over different processes being the root
C

          DO 400 ROOT=0,MPITEST_NUMP-1
C           Print an informational message
            IF (PRINT_NODE) THEN
              INFOBUF=' '
              WRITE (INFOBUF,179) 
     $          'commindex', COMM_INDEX, ', commsize',
     $          COMM_SIZE, ', commtype', COMM_TYPE, ', root', ROOT,
     $          ', datatype', TEST_TYPE, ', length',  LENGTH
              CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            END IF

C
C           All but root on receive side post receives
C
            IF (MPITEST_CURRENT_RANK .NE. ROOT) THEN
C
C             Initialize expected receive data
C
              CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1,
     $          TEST_TYPE)
              CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH+1, VALUE,
     $          RECV_BUFFER(RECV_OFFSET))
              LOOP_CNT = LOOP_CNT + 1
              LOOP_FAIL = 0


              CALL MPI_RECV(RECV_BUFFER(RECV_OFFSET), LENGTH+1,
     $          MPITEST_MPI_DATATYPES(TEST_TYPE), ROOT, ROOT,
     $          COMM, RECV_STAT, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                INFOBUF=' '
                WRITE(INFOBUF,99) 'MPI_RECV() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                LOOP_FAIL = LOOP_FAIL + 1
              END IF
C
C             Check data for errors and overflow
C
              CALL MPITEST_DATATEMPLATE_INIT(VALUE, ROOT,
     $          TEST_TYPE)
              CALL MPITEST_BUFFER_ERRORS(TEST_TYPE, LENGTH, VALUE,
     $          RECV_BUFFER(RECV_OFFSET), ERROR)
              CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1,
     $          TEST_TYPE)
              CALL MPITEST_BUFFER_ERRORS_OV(TEST_TYPE, LENGTH,
     $          VALUE, RECV_BUFFER(RECV_OFFSET), ERROR2)
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
              IF (RECV_STAT(MPI_TAG) .NE. ROOT) THEN
                INFOBUF=' '
                WRITE(INFOBUF,98) 'Unexpected tag value',
     $            RECV_STAT(MPI_TAG), ', expected', ROOT
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                LOOP_FAIL = LOOP_FAIL + 1
              END IF

              IF (RECV_STAT(MPI_SOURCE) .NE. ROOT) THEN
                INFOBUF=' '
                WRITE(INFOBUF,98) 'Unexpected source value',
     $            RECV_STAT(MPI_SOURCE), ', expected', ROOT
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                LOOP_FAIL = LOOP_FAIL + 1
              END IF

              IF (LOOP_FAIL .NE. 0) FAIL = FAIL + 1
            END IF
C                Receiver

C
C           Root sends the message
C
            IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN

              CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1, 
     $          TEST_TYPE)
              CALL MPITEST_INIT_BUFFER(TEST_TYPE, 4,
     $          VALUE, SEND_BUFFER(1))

              CALL MPITEST_DATATEMPLATE_INIT(VALUE, 
     $          MPITEST_CURRENT_RANK, TEST_TYPE)
              CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH+1,
     $          VALUE, SEND_BUFFER(SEND_OFFSET))

              DO 500 M=0, MPITEST_NUMP-1
                IF (M .EQ. ROOT) GO TO 500
C                       Don't send to self
                LOOP_CNT = LOOP_CNT + 1

                CALL MPI_SEND(SEND_BUFFER(SEND_OFFSET), LENGTH,
     $            MPITEST_MPI_DATATYPES(TEST_TYPE), M, ROOT,
     $            COMM, IERR)
                IF (IERR .NE. MPI_SUCCESS) THEN
                  INFOBUF=' '
                  WRITE(INFOBUF,99) 'MPI_SEND() returned', IERR
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                  INFOBUF=' '
                  CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                  FAIL = FAIL + 1
                END IF

 500          CONTINUE
C                         Send to all receivers
            END IF
C                    Sender

            CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_BARRIER() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF

 400      CONTINUE
C                     End root loop


 300    CONTINUE
C                   End data offset loop

 200  CONTINUE
C                 End data length loop
      

C
C     Report overall results
C
      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)
      
      CALL MPI_FINALIZE(IERR)

      END
