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
C                       Test for MPI_Allreduce_loc()
C 
C This code tests the MPI_Allreduce() function on the MINLOC and MAXLOC
C operators.  MPI_Allreduce() performs a binary, associative operation in such
C a way that data from process "i" is operated on exactly once.
C 
C The operations to be looped over are in the array MPITEST_default_ops[].
C This array must be initialized at runtime, after the call to MPI_Init().
C 
C **********************************************************************

#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'
      INCLUDE 'reduce.h'

      INTEGER MPITEST_DEFAULT_OPS(10)

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
      INTEGER    NUM_OPS
C                               number of operations to test
      INTEGER    OP_COUNT
C                               index of current operation to test

      INTEGER    FAIL
      INTEGER    LOOP_CNT
C                               counts total number of failures, loops
      INTEGER    ERROR, ERROR2
C                               number of data errors found in buffer
      INTEGER    IERR
      INTEGER    ERR
C                               return value from MPI calls
      INTEGER    COMM
C                               MPI communicator under test
      INTEGER    I, J, K, L

      LOGICAL    HASOP
      INTEGER    LOCALTYPE

      MPITEST_BUF_TYPE  SEND_BUFFER(MAX_BUFF_SIZE*2+MAX_RANKS)
      MPITEST_BUF_TYPE  RECV_BUFFER(MAX_BUFF_SIZE*2+MAX_RANKS)
      INTEGER    ERRSIZE

      MPITEST_BUF_TYPE     VALUE(2)

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 99   FORMAT(A,INT_FMT)
 179  FORMAT(A,I6,A,I6,A,I6,A,I3,A,INT_FMT,A,I3)
 189  FORMAT(INT_FMT,A,A,I6,A,I6,A,I6,A,I3,A,INT_FMT,A,I3)

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
      TESTNAME = 'MPI_Allreduce_loc()'
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
C     Initialize operations
C
      CALL SET_DEFAULT_OPS(MPITEST_DEFAULT_OPS, NUM_OPS)

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

C       Skip if intercommunicator

        CALL MPI_COMM_TEST_INTER(COMM, INTER_FLAG, IERR)
        IF (INTER_FLAG .EQV. .TRUE.) GO TO 201

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
     $          MAX_BUFF_SIZE * MPITEST_BUF_EXTENT, MAX_BUFF_LENGTH)

          IF (MAX_LENGTH .GE. (MAX_BUFF_LENGTH - 1))
     $         MAX_LENGTH = MAX_BUFF_LENGTH - 1
          IF (MAX_LENGTH .LE. 0) MAX_LENGTH = 0


C
C         Loop through data lengths
C
          DO 300 K=1,LENGTH_COUNT
            CALL MPITEST_GET_MESSAGE_LENGTH(K, BYTE_LENGTH)
            CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, BYTE_LENGTH,
     $          LENGTH)
            IF (LENGTH .GT. MAX_LENGTH) LENGTH = MAX_LENGTH

C
C           Loop through the different operations
C
            DO 350 OP_COUNT=1,NUM_OPS
              CALL HAS_OP(OP_COUNT, TEST_TYPE, HASOP)
              IF (HASOP .EQV. .FALSE.)
     $          GO TO 350

C             Print an informational message
              IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                WRITE (INFOBUF,179) 'commindex',COMM_INDEX,
     $              ', commsize', COMM_SIZE, 
     $              ', commtype', COMM_TYPE,
     $              ', datatype', TEST_TYPE, ', length',  LENGTH,
     $              '. op', OP_COUNT
                CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
              END IF
              LOOP_CNT = LOOP_CNT + 1

C
C             Initialize send data, receive buffer
C
                CALL MPITEST_DATATEMPLATE_INIT_POS(VALUE,
     $              MPITEST_CURRENT_RANK+1, 1, TEST_TYPE)
                CALL MPITEST_DATATEMPLATE_INIT_POS(VALUE,
     $              MPITEST_CURRENT_RANK+1, 2, TEST_TYPE)
                CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH*2, VALUE,
     $                SEND_BUFFER)
                CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1,
     $                TEST_TYPE)
                CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH*2+1, VALUE,
     $                RECV_BUFFER)

                IF (MPITEST_MPI_DATATYPES(TEST_TYPE) .EQ. MPI_INTEGER)
     $            THEN
                     LOCALTYPE = MPI_2INTEGER
                ELSE IF (MPITEST_MPI_DATATYPES(TEST_TYPE) .EQ. MPI_REAL)
     $            THEN
                     LOCALTYPE = MPI_2REAL
                ELSE IF (MPITEST_MPI_DATATYPES(TEST_TYPE) .EQ.
     $                   MPI_DOUBLE_PRECISION) THEN
                  LOCALTYPE = MPI_2DOUBLE_PRECISION
                END IF

              IERR = 0
              IF (LENGTH .NE. 0)
     $          CALL MPI_ALLREDUCE(SEND_BUFFER, RECV_BUFFER,
     $             LENGTH, LOCALTYPE,
     $             MPITEST_DEFAULT_OPS(OP_COUNT), COMM, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                WRITE(INFOBUF,99) 'MPI_ALLREDUCE() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                FAIL = FAIL + 1
              END IF
C
C               Check data for errors and overflow
C
              CALL GET_REDUCE_ANSWER(OP_COUNT, COMM_SIZE, VALUE,
     $            TEST_TYPE)
              CALL MPITEST_BUFFER_ERRORS_LOC(TEST_TYPE, LENGTH,
     $             VALUE, RECV_BUFFER, ERROR)
              CALL MPITEST_DATATEMPLATE_INIT(VALUE,
     $            -1, TEST_TYPE)
              CALL MPITEST_BUFFER_ERRORS_OV(TEST_TYPE, LENGTH * 2,
     $            VALUE, RECV_BUFFER, ERROR2)
              IF ((ERROR + ERROR2) .NE. 0) THEN
                IF (IERR .EQ. MPI_SUCCESS) FAIL = FAIL + 1
                WRITE (INFOBUF,189) ERROR+ERROR2,
     $            ' errors in buffer, ',
     $            'commindex',COMM_INDEX,', commsize', COMM_SIZE,
     $            ', commtype', COMM_TYPE,
     $            ', datatype', TEST_TYPE, ', length',  LENGTH,
     $            '. op', OP_COUNT
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              END IF


 350        CONTINUE
      
 300      CONTINUE
      
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
  
  
