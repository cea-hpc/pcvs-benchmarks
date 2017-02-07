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
C          Test for MPI_Sendrecv() to/from  MPI_PROC_NULL
C 
C This tests the MPI_Sendrecv operation to MPI_PROC_NULL.  No
C messages should be sent.
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

      INTEGER    CNT_LEN
C                               length returned from MPI_GET_COUNT

      INTEGER    LOOP_FAIL
      INTEGER    FAIL
      INTEGER    LOOP_CNT
C                               counts total number of failures, loops
      INTEGER    IERR
      INTEGER    ERR
C                               return value from MPI calls
      INTEGER    COMM
C                               MPI communicator under test
      INTEGER    I, J, K

      MPITEST_BUF_TYPE SEND_BUFFER(MAX_BUFF_SIZE+1)
      MPITEST_BUF_TYPE RECV_BUFFER(MAX_BUFF_SIZE+1)

      INTEGER    ERRSIZE

      MPITEST_BUF_TYPE VALUE

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

      INTEGER    STAT(MPI_STATUS_SIZE)
C----------------------------------------------------------------------------

 99   FORMAT(A,INT_FMT)
 169  FORMAT (A,INT_FMT,A)
 179  FORMAT(A,I6,A,I6,A,I6,A,I3,A,INT_FMT)

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
      TESTNAME = 'MPI_Sendrecv_null'
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
          LOOP_FAIL = 0
          DO 300 K=1,LENGTH_COUNT
            CALL MPITEST_GET_MESSAGE_LENGTH(K, BYTE_LENGTH)
            CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, BYTE_LENGTH,
     $          LENGTH)
            IF (LENGTH .GT. MAX_LENGTH) LENGTH = MAX_LENGTH

C
C           All nodes Sendrecv a message to MPI_PROC_NULL
C
C           Print an informational message
            IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
              INFOBUF=' '
              WRITE (INFOBUF,179) 'commindex',COMM_INDEX,', commsize',
     $            COMM_SIZE, ', commtype', COMM_TYPE, 
     $            ', datatype', TEST_TYPE, ', length', LENGTH
              CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            END IF
            LOOP_CNT = LOOP_CNT + 1

            CALL MPI_Sendrecv(SEND_BUFFER, 
     $                        LENGTH,
     $                        MPITEST_MPI_DATATYPES(TEST_TYPE),
     $                        MPI_PROC_NULL,
     $                        1,
     $                        RECV_BUFFER,
     $                        LENGTH,
     $                        MPITEST_MPI_DATATYPES(TEST_TYPE),
     $                        MPI_PROC_NULL,
     $                        1,
     $                        COMM, 
     $                        STAT,
     $                        IERR)

            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_Sendrecv() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF
C
C           Call the MPI_GET_COUNT function and ensure the length
C           received is 0.
C
            CNT_LEN = -1
            CALL MPI_GET_COUNT(STAT,
     $           MPITEST_MPI_DATATYPES(TEST_TYPE), CNT_LEN, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              INFOBUF=' '
              WRITE(INFOBUF,99) 'MPI_GET_COUNT() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' ' 
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              LOOP_FAIL = LOOP_FAIL + 1
            END IF

            IF (CNT_LEN .NE. 0) THEN
              INFOBUF=' '
              WRITE(INFOBUF,169) 'Receive length =', CNT_LEN,
     $             ' expected 0'
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              LOOP_FAIL = LOOP_FAIL + 1
            END IF

C
C           Check tag for MPI_ANY_TAG
C
            IF (STAT(MPI_TAG) .NE. MPI_ANY_TAG) THEN
              INFOBUF=' '
              WRITE(INFOBUF,169) 'Received tag =', STAT(MPI_TAG),
     $             ' expected MPI_ANY_TAG'
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              LOOP_FAIL = LOOP_FAIL + 1
            END IF

C
C           Check source for MPI_PROC_NULL
C
            IF (STAT(MPI_SOURCE) .NE. MPI_PROC_NULL) THEN
              INFOBUF=' '
              WRITE(INFOBUF,169) 'Received source =',
     $              STAT(MPI_SOURCE), ' expected MPI_PROC_NULL'
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              LOOP_FAIL = LOOP_FAIL + 1
            END IF

            IF (LOOP_FAIL .NE. 0) FAIL = FAIL + 1

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
 
 
