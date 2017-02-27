C -----------------------------------------------------------------------------
C MESSAGE PASSING INTERFACE TEST CASE SUITE
C 
C Copyright - 1996 Intel Corporation
C 
C Intel Corporation hereby grants a non-exclusive license under Intel's
C copyright to copy, modify and distribute this software for any purpose
C and without fee, provided that the above copyright notice and the following
C paragraphs appear on all copies.
C 
C Intel Corporation makes no representation that the test cases comprising
C this suite are correct or are an accurate representation of any standard.
C 
C IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT OR
C SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING, CONSEQUENTIAL,
C INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT LIMITED TO INFRINGEMENT,
C LOSS OF USE, BUSINESS INTERRUPTIONS, AND LOSS OF PROFITS, IRRESPECTIVE OF
C WHETHER INTEL HAS ADVANCE NOTICE OF THE POSSIBILITY OF ANY SUCH DAMAGES.
C 
C INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT
C LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
C PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED HEREUNDER
C IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO OBLIGATION TO PROVIDE
C MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS.
C -----------------------------------------------------------------------------
C *****************************************************************************
C                           Test for MPI_IPROBE()
C
C Test to make sure MPI_Iprobe() will return even if no message match the request.
C 
C The root rank will do a Iprobe() with MPI_ANY_TAG
C followed by various tag used.  No message will be sent by any rank.
C The result flag after the MPI_Iprobe() call will be checked to make sure it
C is zero (false).
C 
C This test may be run in any communicator with a minimum of 2 group members.
C 
C The MPITEST environment provides looping over communicator size, root rank.
C The properties of the loops are encoded in configuration arrays in the
C file config.h .
C 
C MPI Calls dependencies for this test:
C   MPI_IPROBE(), MPI_INIT(), MPI_FINALIZE(), MPI_ATTR_GET(),
C   MPI_COMM_TEST_INTER(), MPI_BARRIER(), MPI_ERROR_STRING()
C   MPI_ALLREDUCE(), MPI_COMM_RANK(), MPI_COMM_SIZE()]
C
C Test history:
C
C   1  06/20/96     simont       Original version
C
C *****************************************************************************
C
#include "foptions.h"

      INCLUDE 'mpitest_cfgf.h'

      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

C     The index of the current buffer type
      INTEGER TEST_TYPE

C     The number of process in the current communicator
      INTEGER TEST_NUMP

C     The array index of the current communicator
      INTEGER COMM_INDEX

C     The index of the current communicator type
      INTEGER COMM_TYPE

C     Loop counter for communicator loop
      INTEGER COMM_COUNT

C     The maximum of input communicator from config file 
      INTEGER MAX_COMM_COUNT

C     Communicator
      INTEGER COMM

C     Counts total number of failure(s)
      INTEGER FAIL

C     Return size from MPI_Error_string
      INTEGER SIZE

C     Counts total number of loops through test
      INTEGER LOOP_CNT

C     Return value from MPI call
      INTEGER IERR, IERR2

C     The root rank
      INTEGER ROOT

C     Message tag
      INTEGER TAG

C     MPI_TAG_UB's value
      INTEGER ATTR_UB

C     Maximum tag used
      INTEGER MAXTAG

C     Status object returned from MPI call
      INTEGER STATUS(MPI_STATUS_SIZE)

C     Logical flag
      LOGICAL FLAG, FOUND

      INTEGER MPITEST_MAX_TAG 
      PARAMETER (MPITEST_MAX_TAG = 5)

      CHARACTER*(IOSIZE)  INFOBUF

      CHARACTER*32   TESTNAME
       CHARACTER*(MPI_MAX_ERROR_STRING) ERRORSTRING

 10   FORMAT(A,INT_FMT)
 20   FORMAT(A)
 30   FORMAT(A, A, INT_FMT, A)
 40   FORMAT(A, INT_FMT, A)
 50   FORMAT(A, INT_FMT, A, INT_FMT, A, INT_FMT, A, INT_FMT)
 60   FORMAT(A, INT_FMT, A, INT_FMT)
 85   FORMAT(A, INT_FMT, A, INT_FMT, A)
 88   FORMAT(A, A)
 90   FORMAT(A, INT_FMT, A, INT_FMT, A)

C
C  Initialize the MPI environment and test environment.
C
      CALL MPI_INIT(IERR)

      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF,10) 'MPI_INIT() returned ', IERR 
         CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
         INFOBUF=' '
         CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
      END IF

      CALL MPITEST_INIT(IERR)
      TESTNAME = 'MPI_Iprobe_return'
      IF (MPITEST_ME .EQ. 0) THEN
        INFOBUF = 'Starting test '//TESTNAME
        CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      FAIL = 0
      LOOP_CNT = 0

C     Get MPI_TAG_UB's value
C
      CALL MPI_ATTR_GET(MPI_COMM_WORLD, MPI_TAG_UB, ATTR_UB, FOUND,
     $   IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         WRITE(INFOBUF, 10)
     $      'MPITEST_COMM_TEST_INTER() returned ', IERR
         CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
         INFOBUF=' '
         CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
      END IF

      IF (FOUND .EQV. .FALSE.) THEN
         WRITE(INFOBUF, 20) 'Cannot find a value for key MPI_TAG_UB'
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      ELSE IF (ATTR_UB .LT. MPITEST_TAG_UB_MIN) THEN
         WRITE(INFOBUF, 90) 'Attribute MPI_TAG_UB (', ATTR_UB,
     $      'is less than the required minimum (', MPITEST_TAG_UB_MIN,
     $      ')'
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      IF (MPITEST_MAX_TAG .GT. ATTR_UB) THEN
         MAXTAG = ATTR_UB
      ELSE
         MAXTAG = MPITEST_MAX_TAG
      END IF

C     Set up maximum tag to be used in transmission
C
      IF (MPITEST_MAX_TAG .GT. MPI_TAG_UB) THEN
         MAXTAG = MPI_TAG_UB
      ELSE
         MAXTAG = MPITEST_MAX_TAG
      END IF

C
C     Get number of communicators to test
C
      CALL MPITEST_NUM_COMM_SIZES(MAX_COMM_COUNT)

C
C     Loop through communicators
C
      DO 100 COMM_COUNT=1, MAX_COMM_COUNT, 1
         CALL MPITEST_GET_COMM_INDEX(COMM_COUNT, COMM_INDEX)
         CALL MPITEST_GET_COMM_TYPE(COMM_COUNT, COMM_TYPE)

         CALL MPITEST_GET_COMMUNICATOR(COMM_TYPE, COMM_INDEX,
     $                                 COMM, TEST_NUMP)

         IF (TEST_NUMP .LT. 2) THEN
C           Skipping communicator with comm size < 2
C
            WRITE(INFOBUF, 30) 'Skipping communicator with size < 2',
     $           ' (commtype: ', COMM_TYPE, ') for this test'
            CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
         ELSE
C           All other process(es) not in the current communicator
C           will skip this.
            IF (COMM .EQ. MPI_COMM_NULL) GO TO 800

            CALL MPI_COMM_TEST_INTER(COMM, FLAG, IERR)
            IF (IERR .NE. MPI_SUCCESS) THEN
               WRITE(INFOBUF, 10)
     $              'MPITEST_COMM_TEST_INTER() returned ', IERR
               CALL MPITEST_MESSAGE(MPITEST_NONFATAL, INFOBUF)
               INFOBUF=' '
               CALL MPI_ERROR_STRING(IERR, ERRORSTRING, SIZE, IERR2)
               CALL MPITEST_MESSAGE(MPITEST_FATAL, ERRORSTRING)
            ELSE IF (FLAG .EQV. .TRUE.) THEN
               WRITE(INFOBUF, 40)
     $              'Skipping inter communicator (commtype: ',
     $              COMM_TYPE, ') for this test'
               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            ELSE
               TEST_TYPE = MPITEST_INTEGER

               DO 300 ROOT=0, TEST_NUMP - 1, 1
C                 Print an informational message
C
                  IF (MPITEST_CURRENT_RANK .EQ. 0) THEN
                     WRITE(INFOBUF, 50) '(', COMM_COUNT,
     $                    ') commsize ', TEST_NUMP, ' commtype ',
     $                    COMM_TYPE, ' root ', ROOT
                     CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
                  END IF

                  IF (MPITEST_CURRENT_RANK .NE. MPI_UNDEFINED) THEN
                     LOOP_CNT = LOOP_CNT + 1

                     IF (MPITEST_CURRENT_RANK .EQ. ROOT) THEN
C                       Call MPI_IPROBE() with MPI_ANY_TAG first
C
                        WRITE(INFOBUF, 85)
     $                     'Probing source: ', ROOT,
     $                     ' tag: MPI_ANY_TAG (', MPI_ANY_TAG,
     $                     ')'
                        CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                          INFOBUF)

                        CALL MPI_IPROBE(ROOT, MPI_ANY_TAG,
     $                                 COMM, FLAG, STATUS, IERR)

                        IF (IERR .NE. MPI_SUCCESS) THEN
                           FAIL = FAIL + 1
                           WRITE(INFOBUF, 10)
     $                          'MPI_IPROBE() returned ', IERR
                           CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                                             INFOBUF)
                           INFOBUF=' '
                           CALL MPI_ERROR_STRING(IERR, INFOBUF,
     $                                              SIZE, IERR2)
                           CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                                          INFOBUF)
                        END IF

                        IF (FLAG .EQV. .TRUE.) THEN
                           FAIL = FAIL + 1
                           WRITE(INFOBUF, 88)
     $                        'MPI_Iprobe() returned FLAG = .TRUE.',
     $                        ' when no message has been posted' 

                           CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                        INFOBUF)
                        END IF

                        DO 400 TAG=TEST_NUMP - 1, 0, 1
                           WRITE(INFOBUF, 60)
     $                        'Probing source ', ROOT,
     $                        ' tag: ', TAG
                           CALL MPITEST_MESSAGE(MPITEST_INFO1,
     $                             INFOBUF)

                           CALL MPI_IPROBE(ROOT, TAG, COMM,
     $                             FLAG, STATUS, IERR)

                           IF (IERR .NE. MPI_SUCCESS) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 10) 
     $                           'MPI_GET_COUNT() returned ',
     $                           IERR
                              CALL MPITEST_MESSAGE(
     $                           MPITEST_NONFATAL,
     $                           INFOBUF)
                              INFOBUF=' '
                              CALL MPI_ERROR_STRING(IERR,
     $                           INFOBUF,
     $                           SIZE, IERR2)

                              CALL MPITEST_MESSAGE(
     $                           MPITEST_FATAL,
     $                           INFOBUF)
                           END IF

                           IF (FLAG .EQV. .TRUE.) THEN
                              FAIL = FAIL + 1
                              WRITE(INFOBUF, 88)
     $                           'MPI_Iprobe() returned FLAG = .TRUE.',
     $                           ' when no message has been posted' 

                              CALL MPITEST_MESSAGE(MPITEST_NONFATAL,
     $                           INFOBUF)
                           END IF

 400                    CONTINUE

                     END IF

                     CALL MPI_BARRIER(COMM, IERR)
                     IF (IERR .NE. MPI_SUCCESS) THEN
                        WRITE(INFOBUF,10)
     $                     'MPI_BARRIER() returned ', IERR 
                        CALL MPITEST_MESSAGE(
     $                     MPITEST_NONFATAL, INFOBUF)
                        INFOBUF=' '
                        CALL MPI_ERROR_STRING(IERR,
     $                     INFOBUF, SIZE, IERR2)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL,
     $                     INFOBUF)
                     END IF
                  END IF
 300           CONTINUE

            END IF
         END IF

 800     CALL MPITEST_FREE_COMMUNICATOR(COMM_TYPE, COMM, IERR)
 100  CONTINUE

      CALL MPITEST_REPORT(LOOP_CNT - FAIL, FAIL, 0, TESTNAME)

      CALL MPI_FINALIZE(IERR)

      END

     
