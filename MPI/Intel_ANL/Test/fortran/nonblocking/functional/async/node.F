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
#define DFL_NUM_MSGS 50
#define MAX_TYPESIZE 8

      INCLUDE 'mpitest_cfgf.h'


      SUBROUTINE INIT_EXPECT(EXPECT, MAXCNT)
      INCLUDE 'mpitestf.h'

      INTEGER EXPECT(MAX_RANKS * MAX_TYPESIZE), I, NUM_MSGS
      INTEGER ODD_NODE, NUM_ODDS, MAXCNT

      CHARACTER*(IOSIZE) INFOBUF

      NUM_MSGS = MAXCNT / (MPITEST_NUMP - 1)

      DO 100 I = 1, MPITEST_NUMP, 1
         EXPECT(I) = NUM_MSGS
 100  CONTINUE

      EXPECT(MPITEST_ME + 1) = 0

      ODD_NODE = MPITEST_ME
      NUM_ODDS = MOD(MAXCNT, MPITEST_NUMP - 1)

      DO 200 I = 1, NUM_ODDS, 1
         IF (ODD_NODE .EQ. 0) THEN
            ODD_NODE = MPITEST_NUMP - 1
         ELSE
            ODD_NODE = ODD_NODE - 1
         END IF
         EXPECT(ODD_NODE + 1) = EXPECT(ODD_NODE + 1) + 1
 200  CONTINUE

      END


      PROGRAM MAIN

      INCLUDE 'mpitestf.h'
      INCLUDE 'externalf.h'
      INCLUDE 'mpif.h'

      INTEGER RCNT, SCNT
      INTEGER FAILLOOP, I, IERR
      INTEGER EXPECT(MAX_RANKS * MAX_TYPESIZE)
      INTEGER REC_TALLY(MAX_RANKS * MAX_TYPESIZE)

      INTEGER RBUF(DFL_NUM_MSGS * MAX_RANKS * MAX_TYPESIZE)
      INTEGER SBUF(DFL_NUM_MSGS * MAX_RANKS * MAX_TYPESIZE)

      INTEGER DEST_NODE, LEN
      INTEGER TYPE, TYPESIZE, SIZE

      DOUBLE PRECISION LASTPRINT

      INTEGER MPI_STATUS(MPI_STATUS_SIZE), SEND_REQUEST, RECV_REQUEST
      INTEGER MY_COUNT

      LOGICAL MPI_FLAG

      INTEGER FAILED, LOOPCNT, MAXCNT, MAXLOOPCNT

      INTEGER MAXLEN
      INTEGER INT_STR_LEN, INT_STR_LEN1, INT_STR_LEN2, INT_STR_LEN3

      CHARACTER*(IOSIZE) INFOBUF

      CHARACTER*(IOSIZE) INT_STR, INT_STR1, INT_STR2, INT_STR3

      LEN = 0
      CALL MPI_INIT(IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
         INFOBUF='MPI_INIT() returns ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      CALL MPITEST_INIT(IERR)

      CALL MPI_TYPE_SIZE(MPI_INTEGER, TYPESIZE, IERR)
      IF (IERR .NE. MPI_SUCCESS) THEN
         CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
         INFOBUF = 'MPI_TYPE_SIZE() returns ' //
     $             INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
      END IF

      MAXLEN = DFL_NUM_MSGS * (MPITEST_NUMP - 1)
      MAXLOOPCNT = DFL_NUM_MSGS

      MAXCNT = MAXLEN / TYPESIZE

      IF (MPITEST_ME .EQ. 0) THEN
         INFOBUF = 'Start ASYNC test'
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

         CALL MPITEST_INT_TO_STR(MAXCNT, INT_STR, INT_STR_LEN)
         INFOBUF = 'MAXCNT = ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

         CALL MPITEST_INT_TO_STR(MAXLEN, INT_STR, INT_STR_LEN)
         INFOBUF = 'MAXLEN = ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

         CALL MPITEST_INT_TO_STR(MAXLOOPCNT, INT_STR, INT_STR_LEN)
         INFOBUF = 'MAXLOOPCNT = ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

         CALL MPITEST_INT_TO_STR(MPITEST_NUMP, INT_STR, INT_STR_LEN)
         INFOBUF = 'NUM_RANKS = ' // INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)

      END IF

C
C     INITIALIZE BUFFER
C
      DO 100 I = 1, MAXCNT, 1
         RBUF(I) = I
         SBUF(I) = I
100   CONTINUE

      CALL INIT_EXPECT(EXPECT, MAXCNT)

      FAILED = 0
      LOOPCNT = 0

200   IF (MAXLOOPCNT .NE. 0) THEN
         LOOPCNT = LOOPCNT + 1

         IF (MAXLOOPCNT .GT. 0) THEN
            MAXLOOPCNT = MAXLOOPCNT - 1
         END IF

         FAILLOOP = 0
         DO 300 I = 1, MPITEST_NUMP, 1
            REC_TALLY(I) = 0 
300      CONTINUE

         LEN = LEN + 1

         TYPE = LOOPCNT
         DEST_NODE = MOD(MPITEST_ME + 1, MPITEST_NUMP)

         SCNT = 0
         RCNT = 0

         CALL MPI_IRECV(RBUF, MAXLEN, MPI_INTEGER, MPI_ANY_SOURCE, 
     $                  TYPE, MPI_COMM_WORLD, RECV_REQUEST, IERR)

         IF (IERR .NE. MPI_SUCCESS) THEN
            CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
            INFOBUF = 'MPI_IRECV() returns ' //
     $                INT_STR(1:INT_STR_LEN)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         CALL MPI_ISEND(SBUF, LEN, MPI_INTEGER, DEST_NODE, TYPE,
     $                  MPI_COMM_WORLD, SEND_REQUEST, IERR)

         IF (IERR .NE. MPI_SUCCESS) THEN
            CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
            INFOBUF = 'MPI_ISEND() returns ' //
     $                INT_STR(1:INT_STR_LEN)
            CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
         END IF

         LASTPRINT = MPI_WTIME()

400      IF ((SCNT .LT. MAXCNT) .OR. (RCNT .LT. MAXCNT)) THEN
            IF (SCNT .LT. MAXCNT) THEN
               CALL MPI_TEST(SEND_REQUEST, MPI_FLAG, MPI_STATUS, IERR)

               IF (IERR .NE. MPI_SUCCESS) THEN
                  CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
                  INFOBUF = 'MPI_TEST() (send request) returns ' //
     $                      INT_STR(1:INT_STR_LEN)
                  CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
               END IF

               IF (MPI_FLAG .EQV. .TRUE.) THEN
                  CALL MPITEST_INT_TO_STR(LOOPCNT, INT_STR, INT_STR_LEN)
                  CALL MPITEST_INT_TO_STR(SCNT, INT_STR1, INT_STR_LEN1)
                  CALL MPITEST_INT_TO_STR(DEST_NODE, INT_STR2,
     $                                    INT_STR_LEN2)
                  CALL MPITEST_INT_TO_STR(LEN, INT_STR3, INT_STR_LEN3)

                  INFOBUF = 'loop ' // INT_STR(1:INT_STR_LEN)
     $                      // ' - MPI_ISEND() '
     $                      // ' complete: scnt = ' // 
     $                      INT_STR1(1:INT_STR_LEN1)
     $                      // ', dest_node = ' //
     $                      INT_STR2(1:INT_STR_LEN2)
     $                      // ', len = ' //
     $                      INT_STR3(1:INT_STR_LEN3)

                  CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

                  SCNT = SCNT + 1

                  DEST_NODE = MOD(DEST_NODE + 1, MPITEST_NUMP)

                  IF (DEST_NODE .EQ. MPITEST_ME) THEN
                     DEST_NODE = MOD(DEST_NODE + 1, MPITEST_NUMP)
                  END IF

                  IF (SCNT .LT. MAXCNT) THEN
                     CALL MPI_TYPE_SIZE(MPI_INTEGER, SIZE, IERR)

                     IF (IERR .NE. MPI_SUCCESS) THEN
                        CALL MPITEST_INT_TO_STR(IERR, INT_STR,
     $                                          INT_STR_LEN)
                        INFOBUF = 'MPI_TYPE_SIZE() returns ' //
     $                            INT_STR(1:INT_STR_LEN)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                     END IF

                     CALL MPI_ISEND(SBUF, LEN, MPI_INTEGER, DEST_NODE,
     $                              TYPE, MPI_COMM_WORLD, SEND_REQUEST,
     $                              IERR)

                     IF (IERR .NE. MPI_SUCCESS) THEN
                        CALL MPITEST_INT_TO_STR(IERR, INT_STR,
     $                                          INT_STR_LEN)
                        INFOBUF = 'MPI_ISEND() returns ' //
     $                            INT_STR(1:INT_STR_LEN)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                     END IF

                  END IF
               END IF                 
            END IF

            IF (RCNT .LT. MAXCNT) THEN
               CALL MPI_TEST(RECV_REQUEST, MPI_FLAG, MPI_STATUS, IERR)

               IF (IERR .NE. MPI_SUCCESS) THEN
                  CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
                  INFOBUF = 'MPI_TEST() (recv request) returns ' //
     $                      INT_STR(1:INT_STR_LEN)
                  CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
               END IF

               IF (MPI_FLAG .EQV. .TRUE.) THEN

                  CALL MPITEST_INT_TO_STR(LOOPCNT, INT_STR, INT_STR_LEN)
                  CALL MPITEST_INT_TO_STR(RCNT, INT_STR1, INT_STR_LEN1)
                  CALL MPI_GET_COUNT(MPI_STATUS, MPI_INTEGER, MY_COUNT,
     $                               IERR)

                  IF (IERR .NE. MPI_SUCCESS) THEN
                     CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
                     INFOBUF = 'MPI_GET_COUNT() returns ' //
     $                         INT_STR(1:INT_STR_LEN)
                     CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                  END IF

                  CALL MPITEST_INT_TO_STR(MPI_STATUS(MPI_SOURCE),
     $                                    INT_STR2, INT_STR_LEN2)

                  CALL MPITEST_INT_TO_STR(MY_COUNT, INT_STR3,
     $                                    INT_STR_LEN3)

                  INFOBUF = 'loop ' // INT_STR(1:INT_STR_LEN)
     $                      // ' - MPI_IRECV() '
     $                      // ' complete: rcnt = ' //
     $                      INT_STR1(1:INT_STR_LEN1)
     $                      // ', src_node = ' 
     $                      // INT_STR2(1:INT_STR_LEN2)
     $                      // ', len = ' //
     $                      INT_STR3(1:INT_STR_LEN3)

                  CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)

                  REC_TALLY(MPI_STATUS(MPI_SOURCE) + 1) = 
     $               REC_TALLY(MPI_STATUS(MPI_SOURCE) + 1) + 1 
                  RCNT = RCNT + 1

                  IF (RCNT .LT. MAXCNT) THEN
                     CALL MPI_IRECV(RBUF, MAXLEN, MPI_INTEGER,
     $                              MPI_ANY_SOURCE,
     $                              TYPE, MPI_COMM_WORLD,
     $                              RECV_REQUEST, IERR)

                     IF (IERR .NE. MPI_SUCCESS) THEN
                        CALL MPITEST_INT_TO_STR(IERR, INT_STR,
     $                                          INT_STR_LEN)
                        INFOBUF = 'MPI_IRECV() returns ' //
     $                            INT_STR(1:INT_STR_LEN)
                        CALL MPITEST_MESSAGE(MPITEST_FATAL, INFOBUF)
                     END IF

                  END IF
               END IF
            END IF

            IF (MPI_WTIME() - LASTPRINT .GT. 60.0) THEN
               IF (MPITEST_ME .EQ. 0) THEN
                  CALL MPITEST_INT_TO_STR(LOOPCNT, INT_STR, INT_STR_LEN)
                  CALL MPITEST_INT_TO_STR(SCNT, INT_STR1, INT_STR_LEN1)
                  CALL MPITEST_INT_TO_STR(RCNT, INT_STR2, INT_STR_LEN2)

                  INFOBUF = 'loop ' // INT_STR(1:INT_STR_LEN) //
     $                      ': scnt = '
     $                      // INT_STR1(1:INT_STR_LEN1)
     $                      // ', rcnt = ' //
     $                      INT_STR2(1:INT_STR_LEN2)

                  CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
               END IF

               LASTPRINT = MPI_WTIME()
            END IF

            GO TO 400
         ELSE
            GO TO 450
         END IF

450      DO 460 I = 1, MPITEST_NUMP, 1
            IF (REC_TALLY(I) .NE. EXPECT(I)) THEN
               FAILLOOP = FAILLOOP + 1

               CALL MPITEST_INT_TO_STR(LOOPCNT, INT_STR, INT_STR_LEN)
               CALL MPITEST_INT_TO_STR(I, INT_STR1, INT_STR_LEN1)
               CALL MPITEST_INT_TO_STR(REC_TALLY(I), INT_STR2,
     $                                 INT_STR_LEN2)
               CALL MPITEST_INT_TO_STR(EXPECT(I), INT_STR3,
     $                                 INT_STR_LEN3)

               INFOBUF = 'LOOP ' // INT_STR(1:INT_STR_LEN) //
     $                   ': REC_TALLY('
     $                   // INT_STR1(1:INT_STR_LEN1) // ') = '
     $                   // INT_STR2(1:INT_STR_LEN2)
     $                   // ', expected ' //
     $                   INT_STR3(1:INT_STR_LEN3)

               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            END IF
460      CONTINUE

         IF (MPITEST_ME .EQ. 0) THEN
            IF (FAILLOOP .EQ. 0) THEN
               CALL MPITEST_INT_TO_STR(LOOPCNT, INT_STR, INT_STR_LEN)
               INFOBUF = 'loop ' // INT_STR(1:INT_STR_LEN)
     $                   // ': Completed'

               CALL MPITEST_MESSAGE(MPITEST_INFO1, INFOBUF)
            ELSE
               FAILED = FAILED + 1
               CALL MPITEST_INT_TO_STR(LOOPCNT, INT_STR, INT_STR_LEN)
               INFOBUF = 'loop ' // INT_STR(1:INT_STR_LEN)
     $                   // ': *** FAILED ***'

               CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
            END IF
         END IF

         GO TO 200
      ELSE
         GO TO 500
      END IF

      
 500  INFOBUF = 'ASYNC'
      CALL MPITEST_REPORT(LOOPCNT - FAILED, FAILED, 0, INFOBUF)

      CALL MPI_FINALIZE(IERR)

      IF (IERR .NE. MPI_SUCCESS) THEN
         CALL MPITEST_INT_TO_STR(IERR, INT_STR, INT_STR_LEN)
         INFOBUF = 'MPI_FINALIZE() returns ' //
     $             INT_STR(1:INT_STR_LEN)
         CALL MPITEST_MESSAGE(MPITEST_INFO0, INFOBUF)
      END IF

      END
