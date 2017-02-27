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
C                           Test Template
C
C
C **********************************************************************

#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

C   Used to keep track of communicators under test:
      INTEGER    COMM_INDEX
C                               the array index of the current comm
      INTEGER    COMM_TYPE
C                               the index of the current communicator type
      INTEGER    COMM_COUNT
C                               loop counter for communicator loop
      INTEGER    COMM
C                               MPI communicator under test
      INTEGER    COMM_SIZE
C                               number of ranks in the current communicator

C   Used to keep track of data types to test:
      INTEGER    TYPE_COUNT
C                               number of data types to test
      INTEGER    TEST_TYPE
C                               index of current data type

C  Used to keep track of data lengths to test:
      INTEGER    LENGTH_COUNT
C                               number of data lengths to test
      INTEGER    MAX_LENGTH
C                               max message length to test
      INTEGER    MAX_BYTE_LENGTH
C                               max byte length to test
      INTEGER    BYTE_LENGTH
C                               current byte length to test
      INTEGER    LENGTH
C                               current length to test (extent)

      INTEGER    FAIL
      INTEGER    PASS
C                               counts total number of failures, passes
      INTEGER    IERR
C                               return value from MPI calls
      INTEGER    I, J, K, L
      INTEGER    ERROR, ERROR2

      MPITEST_BUF_TYPE BUFFER(MAX_BUFF_SIZE)
C                               special type defined in foptions.h
C                               when configured properly, all data will
C                               fit in BUFFER, plus some left over.
      INTEGER    ERRSIZE

      MPITEST_BUF_TYPE VALUE
C                               special type defined in foptions.h
C                               contains value that should used to set
C                               or compare buffer.

      CHARACTER*(IOSIZE)  INFOBUF
C                              for messages
      CHARACTER*32   TESTNAME

 99   FORMAT(A,INT_FMT)
 179  FORMAT(A,I6,A,I6,A,I6,A,I6,A,I3,A,INT_FMT)
 189  FORMAT(INT_FMT,A,A,I6,A,I6,A,I6,A,I3,A,INT_FMT)
C  INT_FMT is defined in the .h files too.

C
C     Initialize the MPI environment
C
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
        WRITE(INFOBUF,99) 'MPI_INIT() returned', IERR
        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
C         Fatal error, will not return.
      END IF
C
C     Initialize the MPITEST environment
C
      CALL MPITEST_INIT(IERR)
      TESTNAME = 'Template.F'
      IF (MPITEST_ME .EQ. 0) THEN
C         MPITEST_ME is my rank in MPI_COMM_WORLD
C         MPITEST_NUMP is the size of MPI_COMM_WORLD
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
C         _INFO0 is always printed.
      END IF


C
C     Initialize the loop/error counters
C
      PASS = 0
      FAIL = 0

C
C     Get number of communicators, data types and message lengths to test
C     as defined in mpitest_cfgf.h
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
C        COMM_SIZE is the size of the returned communicator.
C        MPITEST_CURRENT_RANK is my rank in COMM (or MPI_UNDEFINED if not)

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

C
C         Loop through data lengths
C
          DO 300 K=1,LENGTH_COUNT
            CALL MPITEST_GET_MESSAGE_LENGTH(K, BYTE_LENGTH)
            CALL MPITEST_BYTE_TO_ELEMENT(TEST_TYPE, BYTE_LENGTH,
     $          LENGTH)

C
C             Initialize send data
C
              CALL MPITEST_DATATEMPLATE_INIT(VALUE,
     $              MPITEST_CURRENT_RANK, TEST_TYPE)
              CALL MPITEST_INIT_BUFFER(TEST_TYPE, LENGTH+1, VALUE,
     $              BUFFER)

              IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                INFOBUF='About to perform test'
                CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
C        To help in debug only, use _INFO1 or _INFO2
C        for verbose output only.
              ENDIF
C
C             Perform tests here.  For example, do an MPI_BCAST
C             of the specified length, datatype on the specified comm.
C
C             CALL MPI_BCAST(BUFFER, LENGTH,
C    $             MPITEST_MPI_DATATYPES(TEST_TYPE), ROOT, COMM, IERR)
C
C             IF (IERR .NE. MPI_SUCCESS) THEN
C               WRITE(INFOBUF,99) 'MPI_BCAST() returned', IERR
C               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
C               INFOBUF=' '
C               CALL MPI_ERROR_STRING(IERR, INFOBUF, ERRSIZE, ERR)
C               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
C               FAIL = FAIL + 1
C             END IF
C
C             Non-fatal errors are always printed.
C

C
C             Initialize expected receive data
C
              CALL MPITEST_DATATEMPLATE_INIT(VALUE,
     $              MPITEST_CURRENT_RANK, TEST_TYPE)

C
C             Check data for errors and overflow
C
              CALL MPITEST_BUFFER_ERRORS(TEST_TYPE, LENGTH, VALUE,
     $              BUFFER, ERROR)
              CALL MPITEST_DATATEMPLATE_INIT(VALUE,
     $              MPITEST_CURRENT_RANK, TEST_TYPE)
              CALL MPITEST_BUFFER_ERRORS_OV(TEST_TYPE, LENGTH, VALUE,
     $              BUFFER, ERROR2)
C
C        Error and error2 contain the number of errors found in the
C        data and the next element, respectively (checking for overflow).
C
              IF ((ERROR + ERROR2) .NE. 0) THEN
                IF (IERR .EQ. MPI_SUCCESS) FAIL = FAIL + 1
                WRITE (INFOBUF,189) ERROR+ERROR2, ' errors in buffer, ',
     $              'commindex',COMM_INDEX,', commsize',
     $              COMM_SIZE, ', commtype', COMM_TYPE,
     $              ', datatype', TEST_TYPE, ', length',  LENGTH
                CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
              END IF

 300      CONTINUE
C          Data length loop

 200    CONTINUE
C          Data type loop

 201    CONTINUE
C          Not in communicator

        CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM, IERR)
C         We don't call MPI_COMM_FREE directly, since this may be
C         a pre-defined communicator (like MPI_COMM_WORLD) or MPI_COMM_NULL.
C         Let it figure out for us.

 100  CONTINUE
C        Communicator loop

C
C     Report overall results
C
      CALL MPITEST_REPORT(PASS, FAIL, 0, TESTNAME)
C      Reports results, and does any other clean-up necessary.
C      Third parm is manually verify.

      CALL MPI_FINALIZE(IERR)

      END
