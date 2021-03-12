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
C
#include "foptions.h"
#define FROM_NODE       0
#define FROM_MSG_TYPE   1
#define DATA_LENGTH     2

#define TO_NODE         1

#define MAX_LEN_SIZE    10000
#define MAX_RINGS       1000

#define SEND_DATA       21
#define MSG_TYPE        10000

      INCLUDE 'mpitest_cfgf.h'


      SUBROUTINE INIT_RINGS(NUM_RINGS, RINGS)
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'

      INTEGER NUM_RINGS
      INTEGER RINGS(0:MAX_RINGS - 1, 0:MAX_RANKS - 1,
     $              FROM_NODE:TO_NODE)

      INTEGER I, J, K
      INTEGER DUP_FOUND, R_RING(0:MAX_RANKS-1)

      CHARACTER*(IOSIZE) INFOBUF

      INTEGER INT_STR_LEN, INT_STR_LEN1, INT_STR_LEN2, INT_STR_LEN3
      INTEGER INT_STR_LEN4

      CHARACTER*(IOSIZE) INT_STR, INT_STR1, INT_STR2, INT_STR3
      CHARACTER*(IOSIZE) INT_STR4

      DO 400 I = 0, NUM_RINGS - 1, 1
         DO 300 J = 0, MPITEST_NUMP - 1, 1
            R_RING(J) = J
300      CONTINUE     

         IF (MPITEST_ME .EQ. 0) THEN
            CALL MPITEST_INT_TO_STR(I, INT_STR, INT_STR_LEN)
            INFOBUF = 'ring ' // INT_STR(1:INT_STR_LEN) // ':'
            CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

            DO 350 k = 0, MPITEST_NUMP - 1, 1
               CALL MPITEST_INT_TO_STR(R_RING(K), INT_STR, INT_STR_LEN)
               INFOBUF = INT_STR(1:INT_STR_LEN)

               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
350         CONTINUE
         END IF

         RINGS(I, R_RING(0), FROM_NODE) = R_RING(MPITEST_NUMP - 1)
         RINGS(I, R_RING(0), TO_NODE) = R_RING(1)

         DO 380 J = 1, MPITEST_NUMP - 2, 1
            RINGS(I, R_RING(J), FROM_NODE) = R_RING(J - 1)
            RINGS(I, R_RING(J), TO_NODE) = R_RING(J + 1)
380      CONTINUE

         RINGS(I, R_RING(MPITEST_NUMP - 1), FROM_NODE) =
     $         R_RING(MPITEST_NUMP - 2)
         RINGS(I, R_RING(MPITEST_NUMP - 1), TO_NODE) =
     $         R_RING(0)

400   CONTINUE

      END


      SUBROUTINE SND_RECV(CNT, SND_BUF, RCV_BUF, NUM_RINGS,
     $                    MAX_LEN, RINGS, FAILED)
      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER RINGS(0:MAX_RINGS - 1, 0:MAX_RANKS - 1,
     $              FROM_NODE:TO_NODE)

      INTEGER RCV_BUF(0:MAX_LEN_SIZE-1), SND_BUF(0:MAX_LEN_SIZE-1)

      INTEGER RING, CNT, MAX_LEN, FAILED
      INTEGER NUM_RINGS

      INTEGER J, K, MPI_REQUEST, MPI_STATUS(MPI_STATUS_SIZE), MY_COUNT
      INTEGER FROMNODE, RECV_LEN, RECV_TYPE, SND_LEN, IERR

      CHARACTER*(IOSIZE) INFOBUF

      INTEGER INT_STR_LEN, INT_STR_LEN1, INT_STR_LEN2, INT_STR_LEN3
      INTEGER INT_STR_LEN4

      CHARACTER*(IOSIZE) INT_STR, INT_STR1, INT_STR2, INT_STR3
      CHARACTER*(IOSIZE) INT_STR4

      DO 100 RING = 0, NUM_RINGS - 1, 1
         SND_LEN = MAX_LEN

         DO 50 J = 0, MAX_LEN_SIZE - 1, 1
            RCV_BUF(J) = 0
            SND_BUF(J) = SEND_DATA
 50      CONTINUE

         SND_BUF(FROM_NODE) = MPITEST_ME
         SND_BUF(FROM_MSG_TYPE) = MSG_TYPE + RING
         SND_BUF(DATA_LENGTH) = SND_LEN

C
C        SEND / RECV MESSAGE
C
         CALL MPI_IRECV(RCV_BUF, MAX_LEN, MPI_INTEGER, MPI_ANY_SOURCE,
     $                  10000 + RING, MPI_COMM_WORLD, MPI_REQUEST, IERR)

         IF (IERR .NE. MPI_SUCCESS) THEN
            CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
            INFOBUF = 'MPI_IRECV() returns ' // INT_STR(1:INT_STR_LEN)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         CALL MPI_SEND(SND_BUF, SND_LEN, MPI_INTEGER,
     $                 RINGS(RING, MPITEST_ME, TO_NODE),
     $                 10000 + RING, MPI_COMM_WORLD, IERR)

         IF (IERR .NE. MPI_SUCCESS) THEN
            CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
            INFOBUF = 'MPI_SEND() returns ' // INT_STR(1:INT_STR_LEN)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         CALL MPI_WAIT(MPI_REQUEST, MPI_STATUS, IERR)

         IF (IERR .NE. MPI_SUCCESS) THEN
            CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
            INFOBUF = 'MPI_WAIT() returns ' // INT_STR(1:INT_STR_LEN)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         CALL MPITEST_INT_TO_STR(RING, INT_STR1, INT_STR_LEN1)
         CALL MPITEST_INT_TO_STR(CNT, INT_STR2, INT_STR_LEN2)
         CALL MPITEST_INT_TO_STR(RINGS(RING, MPITEST_ME, TO_NODE),
     $                           INT_STR3, INT_STR_LEN3)
         CALL MPITEST_INT_TO_STR(SND_LEN, INT_STR4, INT_STR_LEN4)

         INFOBUF = 'Ring ' // INT_STR1(1:INT_STR_LEN1) // ' Msg '
     $             // INT_STR2(1:INT_STR_LEN2) // ': Sent to: '
     $             // INT_STR3(1:INT_STR_LEN3) // ', '
     $             // INT_STR4(1:INT_STR_LEN4) // ' bytes'

         CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

C
C        Check received message
C

         CALL MPI_GET_COUNT(MPI_STATUS, MPI_INTEGER, MY_COUNT,
     $                      IERR)
         IF (IERR .NE. MPI_SUCCESS) THEN
            CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
            INFOBUF = 'MPI_GET_COUNT() returns ' //
     $                INT_STR(1:INT_STR_LEN)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         IF (MPI_STATUS(MPI_SOURCE) .NE. RINGS(RING, MPITEST_ME,
     $       FROM_NODE)) THEN
            FAILED = FAILED + 1

            CALL MPITEST_INT_TO_STR(RING, INT_STR1, INT_STR_LEN1)
            CALL MPITEST_INT_TO_STR(CNT, INT_STR2, INT_STR_LEN2)
            CALL MPITEST_INT_TO_STR(RINGS(RING, MPITEST_ME, TO_NODE),
     $                              INT_STR3, INT_STR_LEN3)
            CALL MPITEST_INT_TO_STR(MPI_STATUS(MPI_SOURCE), INT_STR4,
     $                              INT_STR_LEN4)

            INFOBUF = 'Ring ' // INT_STR1(1:INT_STR_LEN1) // ' Msg '
     $                // INT_STR2(1:INT_STR_LEN2) // '*** FAILED ***'
     $                // 'not from expected node ' //
     $               INT_STR3(1:INT_STR_LEN3) // ' != '
     $                // INT_STR4(1:INT_STR_LEN4)

            CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
         END IF

         IF (MY_COUNT .NE. RCV_BUF(DATA_LENGTH)) THEN
            FAILED = FAILED + 1

            CALL MPITEST_INT_TO_STR(RING, INT_STR1, INT_STR_LEN1)
            CALL MPITEST_INT_TO_STR(CNT, INT_STR2, INT_STR_LEN2)
            CALL MPITEST_INT_TO_STR(MY_COUNT,
     $                              INT_STR3, INT_STR_LEN3)
            CALL MPITEST_INT_TO_STR(RCV_BUF(DATA_LENGTH), INT_STR4,
     $                              INT_STR_LEN4)

            INFOBUF = 'Ring ' // INT_STR1(1:INT_STR_LEN1) // ' Msg '
     $                // INT_STR2(1:INT_STR_LEN2) // '*** FAILED ***'
     $                // 'message length from MPI_GET_COUNT() != '
     $                // 'length received ' //
     $                INT_STR3(1:INT_STR_LEN3) // ' != '
     $                // INT_STR4(1:INT_STR_LEN4)

            CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

         END IF
      
         IF (MPI_STATUS(MPI_SOURCE) .NE. RCV_BUF(FROM_NODE)) THEN
            FAILED = FAILED + 1

            CALL MPITEST_INT_TO_STR(RING, INT_STR1, INT_STR_LEN1)
            CALL MPITEST_INT_TO_STR(CNT, INT_STR2, INT_STR_LEN2)
            CALL MPITEST_INT_TO_STR(MPI_STATUS(MPI_SOURCE),
     $                              INT_STR3, INT_STR_LEN3)
            CALL MPITEST_INT_TO_STR(RCV_BUF(FROM_NODE), INT_STR4,
     $                              INT_STR_LEN4)

            INFOBUF = 'Ring ' // INT_STR1(1:INT_STR_LEN1) // ' Msg '
     $                // INT_STR2(1:INT_STR_LEN2) // '*** FAILED ***'
     $                // 'source node from MPI_GET_COUNT() != '
     $                // 'source node received ' //
     $                INT_STR3(1:INT_STR_LEN3) // ' != '
     $                // INT_STR4(1:INT_STR_LEN4)

            CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
         END IF

         DO 90 K = 3, SND_LEN - 1, 1
            IF (RCV_BUF(K) .NE. SEND_DATA) THEN
               CALL MPITEST_INT_TO_STR(RING, INT_STR1, INT_STR_LEN1)
               CALL MPITEST_INT_TO_STR(CNT, INT_STR2, INT_STR_LEN2)
               CALL MPITEST_INT_TO_STR(RCV_BUF(K),
     $                                 INT_STR3, INT_STR_LEN3)
               CALL MPITEST_INT_TO_STR(SEND_DATA, INT_STR4,
     $                                 INT_STR_LEN4)

               INFOBUF = 'Ring ' // INT_STR1(1:INT_STR_LEN1) // ' Msg '
     $                   // INT_STR2(1:INT_STR_LEN2) // '*** FAILED ***'
     $                   // 'buffer received != expected value '
     $                   // INT_STR3(1:INT_STR_LEN3) // ' != '
     $                   // INT_STR4(1:INT_STR_LEN4)

               CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
            END IF
90       CONTINUE
100   CONTINUE

      END



      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      DOUBLE PRECISION LASTPRINT

      INTEGER RINGS(0:MAX_RINGS - 1, 0:MAX_RANKS - 1,
     $              FROM_NODE:TO_NODE)

      INTEGER I, J, IERR

      CHARACTER*(IOSIZE) INFOBUF

      INTEGER MPI_STATUS(MPI_STATUS_SIZE), MPI_REQUEST
      INTEGER MY_COUNT

      INTEGER NUM_RINGS, MAX_LOOPS
      INTEGER CNT, FAILED

      INTEGER MAX_LEN

      INTEGER RCV_BUF(0:MAX_LEN_SIZE-1), SND_BUF(0:MAX_LEN_SIZE-1)

      INTEGER INT_STR_LEN, INT_STR_LEN1, INT_STR_LEN2, INT_STR_LEN3
      INTEGER INT_STR_LEN4

      CHARACTER*(IOSIZE) INT_STR, INT_STR1, INT_STR2, INT_STR3
      CHARACTER*(IOSIZE) INT_STR4

      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
         INFOBUF='MPI_INIT() returns ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      CALL MPITEST_INIT(IERR)

      NUM_RINGS = 5 * MPITEST_NUMP

      MAX_LEN = MAX_LEN_SIZE / (MPITEST_NUMP + 5)

      MAX_LOOPS = 10 * NUM_RINGS

      FAILED = 0

      IF (MPITEST_ME .EQ. 0) THEN
         INFOBUF = 'Starting rings test'
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

         CALL MPITEST_INT_TO_STR(MAX_LEN, INT_STR, INT_STR_LEN)
         INFOBUF = 'MAX_LEN = ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

         CALL MPITEST_INT_TO_STR(MAX_LOOPS, INT_STR, INT_STR_LEN)
         INFOBUF = 'MAX_LOOPS = ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

         CALL MPITEST_INT_TO_STR(MPITEST_NUMP, INT_STR, INT_STR_LEN)
         INFOBUF = 'NUM_RANKS = ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

         CALL MPITEST_INT_TO_STR(NUM_RINGS, INT_STR, INT_STR_LEN)
         INFOBUF = 'NUM_RINGS = ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      CALL INIT_RINGS(NUM_RINGS, RINGS)

      CALL MPI_BARRIER(MPI_COMM_WORLD, IERR)

      IF (IERR .NE. MPI_SUCCESS) THEN
         CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
         INFOBUF = 'MPI_BARRIER() returns ' //
     $             INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      LASTPRINT = MPI_WTIME()

      CNT = 0

200   IF (CNT .NE. MAX_LOOPS) THEN
         CNT = CNT + 1
         CALL SND_RECV(CNT, SND_BUF, RCV_BUF, NUM_RINGS, MAX_LEN,
     $                 RINGS, FAILED)

         IF ((MPITEST_ME .EQ. 0) .AND.
     $       (MPI_WTIME() - LASTPRINT .LT. 60.0)) THEN
            CALL MPITEST_INT_TO_STR(CNT, INT_STR, INT_STR_LEN)
            INFOBUF = 'All rings shifted ' // INT_STR(1:INT_STR_LEN)
     $                // ' times'
            LASTPRINT = MPI_WTIME()
         END IF

         GO TO 200
      ELSE
         GO TO 300
      END IF

 300  INFOBUF = 'RINGS'
      CALL MPITEST_REPORT(MAX_LOOPS - FAILED, FAILED, 0, INFOBUF)

      CALL MPI_FINALIZE(IERR)

      IF (IERR .NE. MPI_SUCCESS) THEN
         CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
         INFOBUF = 'MPI_FINALIZE() returns ' //
     $             INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      END
         
