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
C                        Test for MPI_Alltoallv()
C
C The function MPI_Alltoallv() causes each process to send a message to every
C other process.  The j_th process' message to the i_th process comes from the
C i_th part of j's send buffer, and is received into the j_th part of i's 
C receive buffer.  Each message may be of a different length, and may
C originate at an arbitrary displacement in the send buffer.  Similarly, the
C received messages may be of all different lengths, and maybe received at an
C arbitrary displacement from the beginning of the receive buffer.
C
C This test initializes the buffers in such a way that the receiving process
C can know not only the sending process' identity, but also which piece of the
C sender's data was received.  This is accomplished by setting the i_th chunk
C of data in the j_th node's send buffer to DATA_FACTOR*j+i.  (The i_th chunk of
C data on any node will be sent to the i_th node.)  Then, if the receiving
C process is process j, then its  i_th chunk of received data (which originated
C on the i_th node) should have the value i*DATA_FACTOR+j.
C
C This test also makes the length of the messages and their displacements 
C unique.  The length of the message from the i_th process to the j_th process
C is length + (LENGTH_FACTOR*i)+j.  Here, length is the basic length as
C dictated by the MPITEST message_length looping process.
C
C This test has looping over communicators, data types, and message lengths as
C provided by the MPITEST environment.
C
C Revision History:
C  1          gt  Ported from C.
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
      INTEGER    ERROR
C                               number of data errors found in buffer
      INTEGER    IERR
      INTEGER    ERR
C                               return value from MPI calls
      INTEGER    COMM
C                               MPI communicator under test
      INTEGER    I, J, K, L, M

      MPITEST_BUF_TYPE SEND_BUFFER(2*MAX_BUFF_SIZE+2*MAX_RANKS)
      MPITEST_BUF_TYPE RECV_BUFFER(2*MAX_BUFF_SIZE+2*MAX_RANKS)
      INTEGER    ERRSIZE

      MPITEST_BUF_TYPE VALUES(MAX_RANKS)
      MPITEST_BUF_TYPE VALUE

      INTEGER    SEND_COUNTS(MAX_RANKS)
      INTEGER    RECV_COUNTS(MAX_RANKS)
      INTEGER    SEND_DISPLS(MAX_RANKS)
      INTEGER    RECV_DISPLS(MAX_RANKS)
      INTEGER    SEND_DISPLS_TEMP
      INTEGER    RECV_DISPLS_TEMP

      CHARACTER*(IOSIZE)  INFOBUF
      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 99   FORMAT(A,INT_FMT)
 179  FORMAT(A,I6,A,I6,A,I6,A,I3,A,INT_FMT)
 189  FORMAT(INT_FMT,A,A,I6,A,I6,A,I6,A,I3,A,INT_FMT)

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
      TESTNAME = 'MPI_Alltoallv()'
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

          IF (MAX_LENGTH .GE. (MAX_BUFF_LENGTH/MPITEST_NUMP - 4))
     $         MAX_LENGTH = (MAX_BUFF_LENGTH/MPITEST_NUMP - 4)
          IF (MAX_LENGTH .LT. 0) MAX_LENGTH = 0

C
C         Loop through data lengths
C
          DO 300 K=1,LENGTH_COUNT
            CALL MPITEST_GET_MESSAGE_LENGTH(K, BYTE_LENGTH)
            CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, BYTE_LENGTH,
     $          LENGTH)
            IF (LENGTH .GT. MAX_LENGTH) LENGTH = MAX_LENGTH

C
C           Initialize send data
C
            M=0
            SEND_DISPLS_TEMP = 0
            RECV_DISPLS_TEMP = 0
            DO 400 L=1,COMM_SIZE
              SEND_COUNTS(L)=LENGTH+M
              SEND_DISPLS(L)=SEND_DISPLS_TEMP
              RECV_COUNTS(L)=LENGTH+MOD(MPITEST_CURRENT_RANK,2)
              RECV_DISPLS(L)=RECV_DISPLS_TEMP
              SEND_DISPLS_TEMP=SEND_DISPLS_TEMP+SEND_COUNTS(L)+1
              RECV_DISPLS_TEMP=RECV_DISPLS_TEMP+RECV_COUNTS(L)+1
              CALL MPITEST_DATATEMPLATE_INIT_POS(VALUES,
     $            MPITEST_CURRENT_RANK+L-1, L, TEST_TYPE)
              M = 1 - M
 400        CONTINUE

            CALL MPITEST_INIT_BUFFER_V(TEST_TYPE, COMM_SIZE,
     $          SEND_COUNTS, SEND_DISPLS, VALUES, SEND_BUFFER)

C
C           Initialize receive buffer and template
C
            CALL MPITEST_DATATEMPLATE_INIT(VALUE, -1, TEST_TYPE)
            CALL MPITEST_INIT_BUFFER(TEST_TYPE,
     $            RECV_COUNTS(COMM_SIZE)+RECV_DISPLS(COMM_SIZE),
     $            VALUE, RECV_BUFFER)
            DO 500 L=1,COMM_SIZE
              CALL MPITEST_DATATEMPLATE_INIT_POS(VALUES,
     $            MPITEST_CURRENT_RANK+(L-1), L, TEST_TYPE)
 500        CONTINUE

C           Print an informational message
            IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
              WRITE (INFOBUF,179) 'commindex',COMM_INDEX,', commsize',
     $            COMM_SIZE, ', commtype', COMM_TYPE, 
     $            ', datatype', TEST_TYPE, ', length',  LENGTH
              CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            END IF

            LOOP_CNT = LOOP_CNT + 1

            CALL MPI_ALLTOALLV(SEND_BUFFER, SEND_COUNTS, SEND_DISPLS,
     $           MPITEST_MPI_DATATYPES(TEST_TYPE),
     $           RECV_BUFFER, RECV_COUNTS, RECV_DISPLS,
     $           MPITEST_MPI_DATATYPES(TEST_TYPE), COMM, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
              WRITE(INFOBUF,99) 'MPI_ALLTOALLV() returned', IERR
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              INFOBUF=' '
              CALL MPI_ERROR_STRING(IERR, ERRORSTRING, ERRSIZE, ERR)
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, ERRORSTRING)
              FAIL = FAIL + 1
            END IF
C
C           Check received data for errors
C
            CALL MPITEST_BUFFER_ERRORS_V(TEST_TYPE, COMM_SIZE, 
     $            RECV_COUNTS, RECV_DISPLS, VALUES,
     $            RECV_BUFFER, ERROR)
            IF (ERROR .NE. 0) THEN
              IF (IERR .EQ. MPI_SUCCESS) FAIL = FAIL + 1
              WRITE (INFOBUF,189) ERROR, ' errors in buffer, ',
     $            'commindex',COMM_INDEX,', commsize',
     $            COMM_SIZE, ', commtype', COMM_TYPE, 
     $            ', datatype', TEST_TYPE, ', length', LENGTH
              CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
            END IF
                
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

