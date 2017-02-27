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
C                          Test for MPI_Gatherv()
C 
C This code tests the functionality of the MPI_Gatherv() function.
C This function extends the functionality of MPI_Gather() (to gather
C a piece of data from each node to one root node) so that the pieces
C of data from different nodes may be different sizes for different
C nodes, and so that the displacement from the beginning of the receive
C buffer may leave spaces in between sections of data.
C 
C The latter feature is useful if one is sending data stored as a matrix
C with a leading dimension.  In this case, the beginning of the ith
C column is at location buffer[i*leading_dim], and one might want to
C store matrices whose columns' lengths are less than the leading
C dimension.  In that case, if one wanted to do a Gather() of
C columns of the matrix, one would use MPI_Gatherv().
C 
C This test uses the standard MPITEST library functions to generate the
C message lengths, communicator sizes and types, and buffer types.  Once
C the environment is set up, the variable leading_dim is set to length
C plus some constant value, then the buffers are allocated accordingly
C (send_buffer has size "length", and recv_buffer has size
C "test_nump*leading_dim".)  The receive buffer is initialized to either
C -1, 999, or 'z' (depending on the buffer type in use), and the send
C buffer is initialized to something unique to the process.
C 
C Several subsidiary arrays are necessary for this test.  The arrays
C recv_counts[] and recv_displs[] list the message length and starting
C displacement in the receive array of the different process's messages.
C The arrays counts[] and displs[] and values[] are used for error
C checking.  counts[2*i] is equal to recv_counts[i] (i.e. the length of
C the message from the ith process), but counts[2*i+1] is equal to the
C length of the space in the receive buffer between the message from the
C ith process and that from the (i+1)th process.  In this test,
C counts[2*i+1]=(leading_dim-length) for all 0 < i < test_nump.
C Similarly, displs[2*i] is equal to recv_displs[i] (the displacement of
C the ith message from the beginning of the receive buffer), and
C displs[2*i+1] holds the displacement of the space between the ith and
C (i+1)th messages.  The array values[] holds the correct values which
C should be in the receive buffer upon succesful completion of this
C test.  values[2*i] holds the same value as the send buffer on the ith
C process, and values[2*i+1] holds the value which was in the receive
C buffer before the call to MPI_Gatherv().  Note that values[] is an
C array of type dataTemplate, so each element must be filled with all
C ten possible types of value before it is used.
C
C Revision History:
C  1          gt  Ported from C.
C **********************************************************************

#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'mpif.h'
      INCLUDE 'externalf.h'

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
      INTEGER    ROOT
C                               the current gather root
      INTEGER    FAIL
      INTEGER    LOOP_CNT
C                               counts total number of failures, loops
      INTEGER    ERROR
C                               number of data errors found in buffer
      INTEGER    IERR
      INTEGER    ERR
C                               return value from MPI calls
      INTEGER    COMM
C                               MPI communicator under test
      INTEGER    I, J, K, L, M
      INTEGER    LEADING_DIM

      MPITEST_BUF_TYPE SEND_BUFFER(MAX_BUFF_SIZE+2*MAX_RANKS)
      MPITEST_BUF_TYPE RECV_BUFFER(MAX_BUFF_SIZE+2*MAX_RANKS)
      INTEGER    ERRSIZE

      MPITEST_BUF_TYPE VALUES(2*MAX_RANKS)
      MPITEST_BUF_TYPE VALUE
      INTEGER    COUNTS(2*MAX_RANKS)
      INTEGER    DISPLS(2*MAX_RANKS)
      INTEGER    RECV_COUNTS(2*MAX_RANKS)
      INTEGER    RECV_DISPLS(2*MAX_RANKS)

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

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
      TESTNAME = 'MPI_Gatherv()'
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

          IF (MAX_LENGTH .GE. MAX_BUFF_LENGTH / MPITEST_NUMP - 3) 
     $         MAX_LENGTH = MAX_BUFF_LENGTH / MPITEST_NUMP - 3
          IF (MAX_LENGTH .LE. 0) MAX_LENGTH = 0

C
C         Loop through data lengths
C
          DO 300 K=1,LENGTH_COUNT
            CALL MPITEST_GET_MESSAGE_LENGTH(K, BYTE_LENGTH)
            CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, BYTE_LENGTH,
     $          LENGTH)
            IF (LENGTH .GT. MAX_LENGTH) LENGTH = MAX_LENGTH

            DO 600 ROOT=0, COMM_SIZE-1
C
C             Initialize send data
C
              LEADING_DIM = LENGTH + 2
              CALL MPITEST_DATATEMPLATE_INIT(VALUE,
     $              MPITEST_CURRENT_RANK, TEST_TYPE)
              CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH+1,
     $              VALUE, SEND_BUFFER)
C
C             Initialize receive buffer and template
C
              CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1, TEST_TYPE)
              CALL MPITEST_INIT_BUFFER(TEST_TYPE, COMM_SIZE*LEADING_DIM,
     $              VALUE, RECV_BUFFER)

              M = 0
              DO 400 L=1,COMM_SIZE
                COUNTS(2*L-1)=LENGTH + M
                COUNTS(2*L)=LEADING_DIM - LENGTH - M
                DISPLS(2*L-1)=(L-1)*LEADING_DIM
                DISPLS(2*L)=DISPLS(2*L-1) + LENGTH + M
                RECV_COUNTS(L)=LENGTH+M
                RECV_DISPLS(L)=(L-1)*LEADING_DIM
                CALL MPITEST_DATATEMPLATE_INIT_POS(VALUES, L-1,
     $               2*L - 1, TEST_TYPE)
                CALL MPITEST_DATATEMPLATE_INIT_POS(VALUES, -1,
     $               2*L, TEST_TYPE)
              M = 1 - M
 400          CONTINUE

C             Print an informational message
              IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                WRITE (INFOBUF,179) 'commindex',COMM_INDEX,', commsize',
     $              COMM_SIZE, ', commtype', COMM_TYPE, ', root', ROOT,
     $              ', datatype', TEST_TYPE, ', length',  LENGTH
                CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
              END IF

              LOOP_CNT = LOOP_CNT + 1

              M = MOD(MPITEST_CURRENT_RANK, 2)
              CALL MPI_GATHERV(SEND_BUFFER, LENGTH+M,
     $             MPITEST_MPI_DATATYPES(TEST_TYPE),
     $             RECV_BUFFER, RECV_COUNTS, RECV_DISPLS,
     $             MPITEST_MPI_DATATYPES(TEST_TYPE), ROOT, COMM, IERR)
              IF (IERR .NE. MPI_SUCCESS) THEN
                WRITE(INFOBUF,99) 'MPI_GATHERV() returned', IERR
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                INFOBUF=' '
                CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
                FAIL = FAIL + 1
              END IF
C
C             Check received data for errors . . .
C
              IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
                CALL MPITEST_BUFFER_ERRORS_V(TEST_TYPE, COMM_SIZE*2,
     $              COUNTS, DISPLS, VALUES, RECV_BUFFER, ERROR)
                IF (ERROR .NE. 0) THEN
                  IF (IERR .EQ. MPI_SUCCESS) FAIL = FAIL + 1
                  WRITE (INFOBUF,189) ERROR, ' errors in buffer, ',
     $              'commindex',COMM_INDEX,', commsize',
     $              COMM_SIZE, ', commtype', COMM_TYPE, ', root', ROOT,
     $              ', datatype', TEST_TYPE, ', length', LENGTH
                  CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
                END IF
              END IF
 600        CONTINUE
                
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
  
  
